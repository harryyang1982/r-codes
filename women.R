library(tidyverse)
library(stringr)

women <- read.csv(file.choose())
women <- women %>% 
  mutate(year=as.numeric(substr(women$구분, 1, 4)), position=str_sub(women$구분, 6, 9), gender=str_sub(women$구분, -2, -1))

women$계 <- NULL
women$구분 <- NULL

women %>% 
  group_by(year, position)
write.csv(women, "women2.csv")

women %>% 
  filter(position=="전임교수") %>% 
  ggplot() +
  geom_bar(aes(year, gender, fill=gender), stat="identity")

