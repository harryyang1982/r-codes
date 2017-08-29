library(tidyverse)

mydata <- read.csv(file.choose())

ggplot(mydata[mydata$carat<2.5,], aes(carat, price, color=clarity)) +
  geom_point(alpha=0.1) +
  geom_smooth()
