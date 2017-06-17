#2.3

library(ggplot2)

mpg

data(mpg)

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point()

ggplot(mpg, aes(displ, hwy)) +
  geom_point()

#2.3.1 Exercises
ggplot(mpg, aes(cty, hwy)) +
  geom_point()

ggplot(mpg, aes(model, manufacturer)) +
  geom_point()

#2.3.1.3.

ggplot(mpg, aes(cty, hwy)) + geom_point()
ggplot(diamonds, aes(carat, price)) + geom_point()
ggplot(diamonds, aes(carat, price)) + geom_point() + geom_jitter()
ggplot(economics, aes(date, unemploy)) + geom_line()
ggplot(mpg, aes(cty)) + geom_histogram()

#2.4 Color, size, shape and other aesthetic attributes

ggplot(mpg, aes(displ, cty, color=class)) +
  geom_point()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color="blue"))
ggplot(mpg, aes(displ, hwy)) + geom_point(color="blue")

#2.4.1 Exercises

#2.5 Facetting
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class)

#2.5.1 Exercises

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~hwy)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~cyl)

?facet_wrap()

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class, nrow=4)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~ cyl + drv)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class, scales = "free")

#2.6 Plot Geoms
#2.6.1 Adding a Smoother to a Plot

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth()

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(se=F)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(span = 0.2)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(span = 1)

library(mgcv)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method="gam", formula=y~s(x))

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = "lm")

library(MASS)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = "rlm")

#2.6.2 Boxplots and Jittered Points

ggplot(mpg, aes(drv, hwy)) +
  geom_point()

ggplot(mpg, aes(factor(drv), hwy)) +
  geom_point()

ggplot(mpg, aes(drv, hwy)) + geom_jitter()
ggplot(mpg, aes(drv, hwy)) + geom_boxplot()
ggplot(mpg, aes(drv, hwy)) + geom_violin()

#2.6.3 Histograms and Frequency Polygons

ggplot(mpg, aes(hwy)) + geom_histogram()
ggplot(mpg, aes(hwy)) + geom_freqpoly()

ggplot(mpg, aes(hwy)) +
  geom_freqpoly(binwidth=2.5)
ggplot(mpg, aes(hwy)) +
  geom_freqpoly(binwidth=1)

ggplot(mpg,aes(displ, color=drv)) +
  geom_freqpoly(binwidth=0.5)

ggplot(mpg, aes(displ, fill=drv)) +
  geom_histogram(binwidth=0.5) +
  facet_wrap(~drv, ncol=1)

ggplot(mpg, aes(displ, fill=drv)) +
  geom_histogram(binwidth=0.5)

#2.6.4 Bar Charts

ggplot(mpg, aes(manufacturer)) +
  geom_bar()

drugs <- data.frame(
  drug = c("a", "b", "c"),
  effect = c(4.2, 9.7, 6.1)
)

ggplot(drugs, aes(drug, effect)) + geom_bar(stat = "identity")
ggplot(drugs, aes(drug, effect)) + geom_point()

ggplot(drugs, aes(drug, effect, color=drug)) + geom_point()

#2.6.5 Time Series with Line and Path Plots

ggplot(economics, aes(date, unemploy / pop)) +
  geom_line()

ggplot(economics, aes(date, uempmed)) +
  geom_line()

ggplot(economics, aes(unemploy / pop, uempmed)) +
  geom_path() +
  geom_point()

year <- function(x) as.POSIXlt(x)$year + 1900
ggplot(economics, aes(unemploy / pop, uempmed)) +
  geom_path(color="grey50") +
  geom_point(aes(color=year(date)))

data(economics)

?as.POSIXlt

as.POSIXlt(economics$date) + 1900

#2.6.6 Exercises

ggplot(mpg, aes(cty, hwy)) +
  geom_point()

ggplot(mpg[order(mpg$hwy),], aes(class, hwy)) +
  geom_boxplot()

ggplot(mpg, aes(reorder(class, hwy), hwy)) +
  geom_boxplot()

ggplot(diamonds, aes(carat)) + 
  geom_histogram()

ggplot(diamonds, aes(carat)) + 
  geom_histogram(binwidth=0.5)

ggplot(diamonds, aes(cut, price)) +
  geom_boxplot()

ggplot(diamonds, aes(cut, price)) +
  geom_histogram() # needs to remove outliers

?geom_bar

ggplot(mpg, aes(class)) +
  geom_bar(aes(weight=displ))

ggplot(mpg, aes(class)) +
  geom_bar()

ggplot(mpg, aes(model, manufacturer)) +
  geom_boxplot() +
  facet_wrap(~class)

# 2.7 Modifying the Axis

ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha=1/3)

ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha=1/3) +
  xlab("city driving (mpg)") +
  ylab("highway driving (mpg)")

ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha=1/3) +
  xlab(NULL) +
  ylab(NULL)

ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width=0.25)

ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width=0.25) +
  xlim("f", "r") +
  ylim(20,30)

ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width=0.25, na.rm=T) +
  ylim(NA, 30)

#2.8 Output

p <- ggplot(mpg, aes(displ, hwy, color = factor(cyl))) +
  geom_point()

print(p)

p2 <- ggplot(mpg, aes(displ, hwy, color = cyl)) +
  geom_point()

ggsave("plot.png", width=5, height=5)

summary(p)

saveRDS(p, "plot.rds")
q <- readRDS("plot.rds")

#2.9 Quick Plots

qplot(displ, hwy, data=mpg)
qplot(displ, data=mpg)
qplot(displ, hwy, data=mpg, color="blue")
qplot(displ, hwy, data=mpg, color=I("blue"))
