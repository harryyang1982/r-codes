library(tidyverse)


a <- seq(0, 10, 0.01)
b <- sin(a)
c <- cos(a)

k <- data.frame(a,b,c)

ggplot(k) +
  geom_point(aes(x=a, y=b), color="blue", size=0.1) +
  geom_point(aes(x=a, y=c), color="red", size=0.1) +
  ylim(-1, 1) +
  geom_hline(yintercept=0) +
  ggtitle("Sin and Cos Graph")

s <- seq(-2 * pi, 2 * pi, 0.01)
s2 <- sin(s)
s3 <- cos(s)
p <- data.frame(s, s2, s3)
ggplot(p) +
  geom_point(aes(x=s, y=s2), color="blue", size=0.1) +
  geom_point(aes(x=s, y=s3), color="red", size=0.1) +
  ylim(-1, 1) +
  geom_hline(yintercept=0) +
  ggtitle("Sin and Cosine Graph")



f <- function(x) {cos(x)}
f2 <- function(x) {sin(x)}

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))

p + stat_function(fun= sin, aes(color="Sin"), size=1.1) +
  stat_function(fun= cos, aes(color="Cos"), size=1.1) + 
  stat_function(fun= tan, aes(color="Tan"), size=1.1) +
  xlim(0, 10) +
  ylim(-1, 1) +
  geom_hline(yintercept=0) +
  ggtitle("Sin, Cos and Tangent Graph")
