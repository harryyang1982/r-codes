library(tidyverse)
mpg

# 11.1 준비

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy))

mpg$drv
str(mpg)
mpg$drv

# 11.2-2 점 그래프로 살펴보기

ggplot(mpg) +
  geom_point(aes(x=hwy, y=cyl))

## 색깔 바꾸기

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy, color=class))

## 크기 바꾸기

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy, size=class))

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy, alpha=class))

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy, shape=class))

# 색깔 넣기

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy), color="blue")

#11-2-3-1 연습문제

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy, color="blue"))

#11-2-5 점 그래프의 경향성 파악하기

# left
ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy))

# right
ggplot(mpg) +
  geom_smooth(aes(x=displ, y=hwy))

# drv 타입에 따라 분리
ggplot(mpg) +
  geom_smooth(aes(x=displ, y=hwy, linetype=drv))

ggplot(mpg) +
  geom_smooth(aes(x=displ, y=hwy, color=drv))
              
# 다양한 추세선 보기

ggplot(mpg) +
  geom_smooth(aes(x=displ, y=hwy))

ggplot(mpg) +
  geom_smooth(aes(x=displ, y=hwy, group=drv))

# 점 그래프와 추세선 함께 표현하기

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy)) +
  geom_smooth(aes(x=displ, y=hwy))

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point() +
  geom_smooth()

ggplot(mpg) +
  geom_smooth(aes(x=displ, y=hwy, color=drv)) +
  geom_point(aes(x=displ, y=hwy, color=drv))

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color=class)) +
  geom_smooth()

# 11-2-5-1 연습문제

ggplot(mpg, aes(x=displ, y=hwy, color=drv)) +
  geom_point() +
  geom_smooth(se=F)

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(mpg, aes(x=displ, y=hwy)) +
  geom_smooth(mpg, aes(x=displ, y=hwy))

# 11-2-6 점들이 겹칠 때 해결법

ggplot(data=mpg) +
  geom_point(aes(x=displ, y=hwy), position = "jitter")

# 11-3 막대그래프 다이아몬드 측정해 보기

ggplot(diamonds) +
  geom_bar(aes(x=cut))

ggplot(diamonds) +
  stat_count(mapping=aes(x=cut))

ggplot(diamonds) +
  geom_bar(mapping = aes(x=cut, y= ..prop.., group =1))

# 11-3-2. 다양한 막대그래프의 표현

#left
ggplot(diamonds) +
  geom_bar(mapping = aes(x=cut, color=cut))

#right
ggplot(diamonds) +
  geom_bar(mapping = aes(x=cut, fill=cut))

ggplot(diamonds) +
  geom_bar(mapping=aes(x=cut, fill=clarity))

ggplot(diamonds) +
  geom_bar(mapping = aes(x=cut, fill=clarity), position = "fill")

ggplot(diamonds) +
  geom_bar(mapping = aes(x=cut, fill=clarity), position = "dodge")
