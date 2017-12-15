library(readxl)
library(tidyverse)

score <- read_excel("2017_contemp_homework.xlsx")
score

gathered <- score %>% 
  gather(name, value, `김민주`:`유시원`)

gathered[gathered$과제순서 %in% c(1,2),]$value <- gathered[gathered$과제순서 %in% c(1,2),]$value * 2

gathered

spreaded <- gathered %>% 
  spread(name, value)
