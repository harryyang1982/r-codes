library(data.table)
library(readxl)
library(dplyr)
library(ggplot2)

avg_ten <- fread(file.choose(), stringsAsFactors = F)

glimpse(avg_ten)
avg_ten$TIME <- as.double(avg_ten$TIME)
glimpse(avg_ten)

avg_ten %>% 
  filter(TENURE=="TOTD", COUNTRY=="KOR") %>% 
  ggplot(aes(x=TIME, y=Value)) +
  geom_line(aes(color=SEX)) +
  facet_wrap(~Age)

