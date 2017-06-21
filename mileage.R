library(readxl)
library(tidyverse)

mileage <- read_excel("mileage2017.xlsx", skip=2)
mileage <- mileage[-c(131:138),]
mileage <- mileage[,-1]


mileage %>% 
  filter(마일리지합계 != 0 & 마일리지합계 > 50) %>% 
  summarise(SUM=sum(마일리지합계), AVERAGE=mean(마일리지합계), N=n())

mileage %>% 
  summarise(SUM=sum(마일리지합계), AVERAGE=mean(마일리지합계), N=n())

marinemap <- read_excel("marinemapdb.xlsx")
