library(ggplot2)
library(dplyr)

ggplot(diamonds, aes(x, y)) +
  geom_bin2d()

filter(diamonds, x==0 | y== 0)

diamonds_ok <- filter(diamonds, x >0, y >0, y < 20)
ggplot(diamonds_ok, aes(x, y)) +
  geom_bin2d() +
  geom_abline(slope=1, color="white", size=1, alpha=0.5)

#10.2.2 Missing Values

x <- c(1, NA, 2)
x ==1
x >2
x+10

x == NA
x!=NA

is.na(x)

#10.2.3 Exercises

#1
diamonds %>% 
  filter(x==y)

diamonds %>% 
  filter(depth >=55, depth <70)

diamonds %>% 
  filter(carat < median(carat))

diamonds %>% 
  filter()

diamonds %>% 
  mutate(pcc = price / carat) %>% 
  filter(pcc > 10000)

diamonds %>% 
  filter(cut %in% c("Good", "Very Good"))

#3

diamonds %>% 
  filter(x >0, y >0, y < 20) %>% 
  ggplot(aes(x, y)) +
  geom_bin2d()

diamonds %>% 
  filter(x >0, z >0, z < 10) %>% 
  ggplot(aes(x, z)) +
  geom_bin2d()

diamonds %>% 
  filter(z>0, z<10, y>0, y<20) %>% 
  ggplot(aes(y,z)) +
  geom_bin2d()

#4
install.packages("ggplot2movies")
library(ggplot2movies)

data(package="ggplot2movies")

data("movies")
head(movies)

movies %>% 
  ggplot(aes(rating)) +
  geom_freqpoly(aes(color=is.na(budget)))

#5

NA & F
NA | T
NA * 0

#10.3 Create New Variables

diamonds_ok2 <- mutate(diamonds_ok,
                       sym = x-y,
                       size = sqrt(x ^ 2 + y ^ 2))

diamonds_ok2
diamonds_ok2

diamonds_ok2 %>% 
  ggplot(aes(size, sym)) +
  stat_bin2d()

diamonds_ok2 %>% 
  ggplot(aes(abs(sym))) +
  geom_histogram(binwidth=0.1)

diamonds_ok3 <- diamonds_ok2 %>% 
  filter(abs(sym) < 0.2)

diamonds_ok3 %>% 
  ggplot(aes(abs(sym))) +
  geom_histogram(binwidth=0.01)

# 10.3.2 Exercises

?diamonds

#1
diamonds_ex <- diamonds %>% 
  mutate(volume = x*y*z) %>% 
  mutate(density = carat / volume) %>% 
  mutate(ppc = price / carat) %>% 
  mutate(lc = log(carat), lp = log(price))

#2
diamonds %>% 
  filter(x > 0, z > 0, z < 10) %>% 
  ggplot(aes(x, z)) +
  stat_bin2d()
#3
diamonds %>% 
  mutate(new_depth = round(z/(x+y)*2*100, 1))
#4
diamonds %>% 
  mutate(index = ifelse(x > y, "x > y", 
                        ifelse(x == y, "equal", "x < y"))) %>% 
  ggplot(aes(index)) +
  geom_bar()
                        
#10-4 Group-wise Summaries

by_clarity <- diamonds %>% 
  group_by(clarity) %>% 
  summarise(price = mean(price))

by_clarity
ggplot(by_clarity, aes(clarity, price)) +
  geom_line(aes(group=1), color="grey80") +
  geom_point(size=2)

ggplot(by_clarity, aes(clarity, price)) +
  geom_line(aes(group=1), color="grey80") +
  geom_point(size=2)

cut_depth <- diamonds %>% 
  group_by(cut, depth) %>% 
  summarise(n=n()) %>% 
  filter(depth > 55, depth < 70) 
cut_depth

ggplot(cut_depth, aes(depth, n, color=cut)) +
  geom_line()

cut_depth %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(depth, prop, color=cut)) +
  geom_line()

#10.4.1 Useful Tools

diamonds %>% 
  summarise(n_big = sum(carat >= 4), prop_cheap = mean(price < 1000))

#10.4.2 Statistical Considerations

by_clarity <- diamonds %>% 
  group_by(clarity) %>% 
  summarise(
    n=n(), mean = mean(price), fq = quantile(price, 0.25), uq = quantile(price, 0.75)
  )
by_clarity %>% 
  ggplot(aes(clarity, mean)) +
  geom_line(aes(group = 1), color="grey50") +
  geom_point(aes(size = n))

data(Batting, package = "Lahman")
batters <- Batting %>% 
  filter(AB > 0)

ba <- batters %>% 
  group_by(playerID) %>% 
  summarize(ba = sum(H, na.rm=T) / sum(AB, na.rm = T))

ggplot(ba, aes(ba)) +
  geom_histogram(binwidth = 0.01)

batters %>% 
  group_by(playerID) %>% 
  summarise(ba = sum(H, na.rm=T) / sum(AB, na.rm=T), ab=sum(AB, na.rm=T)) %>% 
  ggplot(aes(ab, ba)) +
  geom_bin2d(bins = 100) +
  geom_smooth()

ba <- batters %>% 
  group_by(playerID) %>% 
  summarise(ba = sum(H, na.rm=T) / sum(AB, na.rm=T), ab=sum(AB, na.rm=T))
  
ba %>% 
  filter(ab >=10) %>% 
  ggplot(aes(ab, ba)) +
  geom_bin2d() +
  geom_smooth()
