library(tidyr)
library(ggplot2)
library(dplyr)

library("lubridate")
ec2 <- 
  ggplot2::economics %>% 
  tbl_df() %>%
  transmute(year = year(date), month = month(date), rate = uempmed) %>%
  filter(year > 2005) %>%
  spread(year, rate)

ec2

# 9.3.1 Gather

gather(ec2, key=year, value=unemp, `2006`:`2015` )
gather(ec2, key=year, value=unemp, -month)

economics_2 <- gather(ec2, year, rate, `2006`:`2015`, convert=T, na.rm=T)

economics_2 %>% 
  ggplot(aes(year+(month-1)/12, rate)) +
  geom_line()

economics_2 %>% 
  ggplot(aes(month, rate, group=year)) +
  geom_line(aes(color=year), size=1)

# 9.3.2 Spread

weather <- data_frame(
  day = rep(1:3, 2),
  obs = rep(c("temp", "rain"), each = 3),
  val = c(c(23, 22, 20), c(0, 0, 5))
)

weather
spread(weather, key=obs, value=val)

#9.3.3 Exercise

library(devtools)
install_github("rstudio/EDAWR")
library(EDAWR)

population
gather(population, key=year, value=population, `1995`:`2013`)
gather(population, key=year, value=population, -country)

population_2 <- gather(population, year, population, `1995`:`2013`, convert=T, na.rm=T)

storms
data(storms)
head(storms)

spread(storms, key=storm, value=pressure)
spread(storms, key=storm, value=wind)

stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocks
stocksm <- stocks %>% gather(stock, price, -time)
stocksm %>% spread(stock, price)

data(tb)

tb
tb2 <-tb %>% 
  gather(age, population, `child`:`elderly`) %>% 
  spread(sex, population) %>% 
  mutate(g_rate = male / female)

# 9.4 Seperate and Unite

trt <- data_frame(
  var = paste0(rep(c("beg", "end"), each=3), "_", rep(c("a", "b", "c"))),
  val = c(1, 4, 2, 10, 5, 11)
)
trt
trt %>% spread(var, val)
separate(trt, var, c("time", "treatment", "_"))

data(who)
glimpse(who)

who2 <- who %>% 
  gather(treatment, value, `new_sp_m014`:`new_rel_f65`) %>% 
  separate(treatment, c("type", "trt", "num"), "_")

# 9.5 Case Studies

# 9.5.1 Blood Pressure

bpd <- readr::read_table(
  "name    age       start    week1    week2    week3
  Anne   35   2014-03-27   100/80   100/75   120/90
  Ben    41   2014-03-09   110/65   100/65   135/70
  Carl    33   2014-04-02   125/80     <NA>     <NA>", na = "<NA>") 

bpd_1 <- gather(bpd, week, bp, week1:week3)
bpd_1
bpd_2 <- separate(bpd_1, bp, c("sys", "dia"), "/")
bpd_3 <- extract(bpd_2, week, "week", "(\\d)", convert=T)
bpd_3
bpd_4 <- arrange(bpd_3, name, week)
bpd_4

# 9.5.2 Test Scores

scores <- data_frame(
  person = rep(c("Greg", "Sally", "Sue"), each=2),
  time = rep(c("pre", "post"), 3),
  test1 = round(rnorm(6, mean=80, sd=4), 0),
  test2 = round(jitter(test1, 15), 0)
)
scores
scores_1 <- gather(scores, test, score, test1:test2)
scores_1
scores_2 <- spread(scores_1, time, score)
scores_2
scores_3 <- mutate(scores_2, diff=post-pre)
scores_3

scores_3 %>% 
  ggplot(aes(person, diff, color=test)) +
  geom_point() +
  geom_path(aes(group=person), color="grey50",
            arrow=arrow(length=unit(0.25, "cm")))
