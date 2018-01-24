#Chapter 1.5.1
library(tidyverse)

turnout <- read_csv("qq_social_science/turnout.csv")
dim(turnout)
summary(turnout)

##dplyr way
glimpse(turnout)
range(turnout$year)

#Q2 turnout rate calculation
turnout <- turnout %>% 
  mutate(VAP_rate = total / (VAP + overseas) * 100, 
         VEP_rate = total / (VEP + overseas) * 100)
turnout

#Q3 difference between VAP and ANES / VEP and ANES
turnout <- turnout %>% 
  mutate(VAP_diff = abs(VAP_rate - ANES),
         VEP_diff = abs(VEP_rate - ANES))
turnout
range(turnout$VAP_diff)
range(turnout$VEP_diff)

#Q4 pres / mid diff

pres <- c(1, 3, 5, 7, 9, 11, 13, 14)
mid <- c(2, 4, 6, 8, 10, 12)

turnout <- turnout %>% 
  mutate(checker = ifelse(row_number(VAP) %in% pres, "pres", "mid"),
         checker = factor(checker))
turnout %>% 
  group_by(checker) %>% 
  summarise(mean = mean(VEP_diff))

first <- 1:7

turnout <- turnout %>% 
  mutate(checker2 = ifelse(row_number(VAP) %in% first, "first", "second"))

plot(turnout[first,]$VEP_diff)
plot(turnout[-first,]$VEP_diff)

##ggplot way
ggplot(turnout) +
  geom_point(aes(1:14, VEP_diff, color = checker2)) +
  scale_x_continuous(breaks = 1:14) + 
  scale_y_continuous(breaks = 9:30)

#Q6
turnout <- turnout %>% 
  mutate(VAP_adjust = VAP - felons - overseas,
         total_adjust = total - osvoters,
         total_rate_adjust = total_adjust / VAP_adjust * 100)

turnout %>% 
  filter(year == 2008) %>% 
  select(year, VAP_adjust, total_adjust, total_rate_adjust)

# 1.5.2

# Q1 CBR

kenya <- read_csv("qq_social_science/kenya.csv")
sweden <- read_csv("qq_social_science/sweden.csv")
world <- read_csv("qq_social_science/world.csv")
