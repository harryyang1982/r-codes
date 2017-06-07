# 12-1 히스토그램 심화

library(tidyverse)

ggplot(diamonds) +
  geom_bar(aes(x=cut))

diamonds %>% 
  count(cut)

table(diamonds$cut) ## 같다

ggplot(diamonds) +
  geom_histogram(aes(x=carat), binwidth = 0.5)

diamonds %>% 
  count(cut_width(carat, 0.5))

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(smaller, aes(x=carat)) + geom_histogram(binwidth=0.1)

ggplot(smaller, aes(x=carat, color=cut)) +
  geom_freqpoly(binwidth=0.1)

# 12-2 선형 그래프 그려보기

# 12-2-1. 기본 꺾은 선형 그래프 그리기

## A. X축이 연속형 변수일 때

str(airquality)

airquality$MonthDay <- as.Date(paste(as.numeric(airquality$Month), airquality$Day, "1973", sep="-"), format="%m-%d-%Y")
str(airquality)

ggplot(airquality) + geom_line(aes(x=MonthDay, y=Wind))

## B. X축이 범주형 변수일 때

ggplot(airquality, aes(x=MonthDay, y=Wind)) + geom_line()
ggplot(airquality, aes(x=MonthDay, y=Wind, group=1)) + geom_line()

## C. Y축 조정하기
ggplot(airquality[140:153, ], aes(x=MonthDay, y=Wind, group=1)) + geom_line()

ggplot(airquality[140:153, ], aes(x=MonthDay, y=Wind, group=1)) + geom_line()+ylim(0, max(airquality$Wind))

# 12-2-2 다양한 꺾은선형 그래프 그리기

ggplot(airquality[airquality$Month %in% c(5,8),], aes(x=Day, y=Wind, group=factor(Month))) + geom_line()

airquality$Month <-as.factor(airquality$Month)
ggplot(airquality[airquality$Month %in% c(5,8), ], aes(x=Day, y=Wind, group=Month, color=Month, linetype=Month)) +
  geom_line(lwd=2)

ggplot(airquality[airquality$Month %in% c(5,8),], 
       aes(x=Day, y=Wind, group=Month, color=Month)) +
  geom_line(aes(linetype=Month)) +
  geom_point()

# 12-3 영역형 그래프 그려보기

data(airquality)
ggplot(airquality[airquality$Month %in% c(5), ], 
       aes(x=Day, y=Wind, group=factor(Month) )) +
  geom_area()

# 12-4 누적 영역형 그래프 그리기

airquality$Month <- as.factor(airquality$Month)
ggplot(airquality,
       aes(x=Day, y=Wind, fill=Month, group=Month)) +
  geom_area()

ggplot(airquality,
       aes(x=Day, y=Wind, fill=Month, group=Month)) +
  geom_area(color="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues", breaks=rev(levels(airquality$Month)))
