library(tidyverse)
library(data.table)

orgrate <- fread("~/GitHub/r-codes/ILO/orgrate.csv")

str(orgrate)

sum_org <- orgrate[, .(avg_rate=mean(obs_value)), by=.(ref_area.label, time)][order(ref_area.label, time)]

sum_org %>% 
  filter(avg_rate > 50) %>% 
  ggplot(aes(x=time, y=avg_rate, color=ref_area.label)) +
  geom_line()

coverage <- fread("~/GitHub/r-codes/ILO/coverage.csv")

str(coverage)
glimpse(coverage)

sum_cov <- coverage[, .(avg_rate=mean(obs_value)), by=.(ref_area.label, time)][order(ref_area.label, time)]

sum_cov %>% 
  filter(avg_rate > 70) %>% 
  ggplot(aes(x=time, y=avg_rate, color=ref_area.label)) +
  geom_line()

total_labor <- sum_org %>% 
  select(ref_area.label, avg_rate, time) %>% 
  rename(org_rate = avg_rate) %>% 
  left_join(sum_cov, by=c("ref_area.label", "time")) %>% 
  rename(cov_rate = avg_rate) %>% 
  na.omit() %>% 
  rename(country=ref_area.label)

total_labor %>% 
  ggplot(aes(x=org_rate, cov_rate)) +
  geom_point(aes(color=country)) +
  geom_abline()

index_df <- total_labor %>% 
  group_by(country) %>% 
  summarise(count=n()) %>% 
  filter(count>=10)

fin_total_labor <- total_labor %>% 
  filter(country %in% index_df$country) 

fin_total_labor %>% 
  ggplot(aes(org_rate, cov_rate)) +
  geom_point(aes(color=country), size=1) +
  geom_abline() +
  facet_wrap(~country) +
  ylab("단체협약 적용률") + xlab("노조조직률")

# explanation below
# year_in=seq(2003,2013)
#select_nations<-total_labor %>%
#  group_by(country)%>%
#  summarize_at(vars(time),funs(sum(.%in%year_in)))%>%
#  filter(time==length(year_in))%>%
#  select(country)%>%
#  pull(1)


# step1
# df%>%group_by(country)%>%summarize_at(vars(time),funs(sum(.%in%year_in)))
# this group country, and sum of time %in% year_in. Thus, if each country's time are in year_in vector , the summation will be same with length(year_in)
# stpe2
# filter(time==length(year_in))%>%select(country)%>%pull(1) 
# now filter the summation is length(year_in) and extract nations name.

# filter nations name
#output<-total_labor%>%filter(country%in%select_nations)
#output

#김영우way
#k <- total_labor %>% 
#  count(country, time) %>% 
#  group_by(country) %>% 
#  filter(sum(n) >= 11)
