library(tidyverse)

seoul <- read_csv("서울dust.csv")
head(seoul)

names(seoul) <- c("date", "place", "CO2", "O3", "CO", "SO2", "TinyDust", "MicroDust")
seoul

seoul$Year <- substr(seoul$date, 1, 4)
seoul$Month <- substr(seoul$date, 5,6)
seoul$Day <- substr(seoul$date, 7,8)
str(seoul)

seoul$date2 <- as.Date(as.character(seoul$date), format="%Y%m%d")
str(seoul)

ggplot(data=seoul[seoul$place %in% c("강남구", "마포구", "중랑구"),]) +
  geom_line(aes(x=date2, y=TinyDust, group=place, color=place)) +
  geom_hline(yintercept=50)

seoul_tiny <- seoul %>% 
  group_by(place) %>% 
  filter(TinyDust > 50) %>% 
  summarise(count=length(place), avg=mean(TinyDust))

ggplot(data=seoul[seoul$TinyDust > 50,]) +
  geom_line(aes(x=date2, y=TinyDust, group=place, color=place)) +
  geom_hline(yintercept=50)
