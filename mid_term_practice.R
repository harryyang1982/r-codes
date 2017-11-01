install.packages("tidytext")
library(tidytext)

summary(mtcars)

head(mtcars$cyl, 10)

data(package="ggplot2")
library(tidyverse)
data(diamonds)

View(diamonds)
View(economics)
View(txhousing)

glimpse(txhousing)
summary(txhousing)

table(txhousing$month)
table(txhousing$city)

data()
data(Nile)
head(Nile)
glimpse(Nile)
data(population)
glimpse(population)
data(sleep)
glimpse(sleep)
glimpse(Orange)
glimpse(AirPassengers)
glimpse(airquality)
glimpse(trees)
glimpse(presidential)
glimpse(volcano)
volcano
glimpse(rock)

install.packages("Lahman")
library(Lahman)

data()
data("LahmanData")
glimpse(LahmanData)
data("Appearances")
glimpse(Appearances)
data("AllstarFull")
glimpse(AllstarFull)
data("AwardsPlayers")
glimpse(AwardsPlayers)
data("Batting")
glimpse(Batting)

data(economics)

glimpse(economics)

economics$status <- ifelse(economics$uempmed >= 10, "high",
                           ifelse(economics$uempmed >= 5, "normal", "low"))

ggplot(economics) +
  geom_bar(aes(status))

economics %>% 
  group_by(status) %>% 
  summarise(average_unemployed = mean(unemploy))

Batting %>% 
  filter(HR>=30) %>% 
  arrange(desc(HR)) %>% 
  head(10)

Batting %>% 
  group_by(playerID) %>% 
  summarise(SUM_HR = sum(HR)) %>% 
  arrange(desc(SUM_HR)) %>% 
  head(10)

Batting %>% 
  group_by(playerID) %>% 
  summarise(SUM_RBI = sum(RBI)) %>% 
  arrange(desc(SUM_RBI)) %>% 
  head(10)

Batting <- Batting %>% 
  mutate(avg=round(H/AB, 3))

summary(Batting)
ggplot(Batting) +
  geom_bar(aes(RBI))

Batting %>% 
  group_by(playerID) %>% 
  summarise(SUM_RBI = sum(RBI)) %>% 
  arrange(desc(SUM_RBI)) %>% 
  head(50) %>% 
  ggplot() +
  geom_bar(aes(x=reorder(playerID, SUM_RBI), y=SUM_RBI, fill=SUM_RBI), stat="identity") +
  coord_flip()
