# 4.2 Building a Scatterplot

library(tidyverse)

ggplot(mpg, aes(displ, hwy, color=factor(cyl))) +
  geom_point()

ggplot(mpg, aes(displ, hwy, color=factor(cyl))) +
  geom_line() +
  theme(legend.position = "none")

ggplot(mpg, aes(displ, hwy, color=factor(cyl))) +
  geom_bar(stat="identity", position = "identity", fill=NA) +
  theme(legend.position="none")

ggplot(mpg, aes(displ, hwy, color=factor(cyl))) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(mpg, aes(displ, hwy, color=factor(cyl))) +
  geom_point() +
  geom_smooth()

# 4.2.2 Scaling

vignette("ggplot2-specs")

# 4.3 Adding Complexity

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~year)

# 4.4 Components of the Layered Grammar

