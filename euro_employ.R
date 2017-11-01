library(tidyverse)
employ <- read.csv(file.choose(), stringsAsFactors = F)

employ$Value <- gsub(":", NA, employ$Value)

employ$GEO[grepl("Germany", employ$GEO)] <- "Germany"

employ$Value <- as.numeric(employ$Value)

employ %>% 
  filter(GEO %in% c("France", "Germany", "Norway", "Sweden", "Denmark", "Finland", "Greece", "Italy", "Slovenia", "United Kingdom", "United States", "European Union (28 countries)")) %>% 
  filter(TIME >=2000) %>% 
  group_by(GEO) %>% 
  filter(AGE=="From 15 to 64 years") %>% 
  ggplot(aes(x=TIME, y=Value)) +
  geom_point(aes(color=GEO)) +
  geom_line(aes(color=GEO), size=1) +
  geom_hline(yintercept = 17.9) +
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015)) +
  labs(color = "Country", x= "Year", y= "Employees with a contract of limited duration Rate (%)")

employ %>% 
  filter(Value <= mean(employ$Value, na.rm=T)) %>% 
  filter(AGE=="From 15 to 64 years") %>% 
  group_by(GEO) %>% 
  ggplot(aes(x=TIME, y=Value)) +
  geom_point(aes(color=GEO)) +
  geom_line(aes(color=GEO))

mean(employ$Value[employ$GEO=="European Union (28 countries)"], na.rm=T)
