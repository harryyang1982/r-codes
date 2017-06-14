library(tidyverse)

data(airquality)

# 선형 그래프 그리기


ggplot(data=airquality, aes(x=Day, y=Wind, group=factor(Month))) +
  geom_line()


ggplot(data=airquality[airquality$Month %in% c(5,8),], aes(x=Day, y=Wind, group=factor(Month))) +
  geom_line()

airquality$Month <- factor(airquality$Month)
ggplot(airquality, aes(x=Day, y=Wind, group=Month, color=Month, linetype=Month)) +
  geom_line(lwd=2)

ggplot(airquality, aes(x=Day, y=Wind, group=Month, color=Month, linetype=Month)) +
  geom_line() +
  geom_point()


# 영역형 그래프 그리기

data(airquality)

ggplot(data=airquality[airquality$Month %in% c(5),], aes(x=Day, y=Wind, group=Month)) +
  geom_area()

ggplot(data=airquality, aes(x=Day, y=Wind, group=Month)) +
  geom_area(aes(fill=Month))

ggplot(data=airquality, aes(x=Day, y=Wind, group=Month)) +
  geom_area(aes(alpha=Month))

# 누적 영역형 그래프 그리기

ggplot(airquality,
       aes(x=Day, y=Wind, fill=Month, group=Month)) +
  geom_area(color="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues", breaks=rev(levels(airquality$Month)))
