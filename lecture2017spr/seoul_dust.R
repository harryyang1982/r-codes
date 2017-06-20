dust <- read.csv("서울dust.csv", stringsAsFactors = F)
colnames(dust)
colnames(dust) <- c("date", "place", "co2", "ozone", "co", "ch2", "tinydust", "stinydust")

str(dust)

library(tidyverse)

group_dust <- dust %>% 
  group_by(place) %>% 
  summarise(M = mean(tinydust, na.rm=T),
            SE = sd(tinydust, na.rm=T) / sqrt(length(na.omit(tinydust))),
            N = length(na.omit(tinydust)))

group_dust$place <- factor(group_dust$place)
group_dust$place <- reorder(group_dust$place, group_dust$M)

ggplot(group_dust, aes(x=M, xmin=M-SE, xmax=M+SE, y= place)) + 
  geom_point() +
  geom_segment(aes(x=M-SE, xend=M+SE, 
                   y=place, yend=place), color="blue") +
  theme(axis.text=element_text(size=8)) +
  xlab("미세먼지 농도 평균") + ylab("구역") 



plot(group_dust$place)
