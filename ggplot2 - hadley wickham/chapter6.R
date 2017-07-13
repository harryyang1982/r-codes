library(tidyverse)

# 6.2 Modifying scales

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color=class))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color=class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color=class)) +
  scale_x_continuous("A really awesome x axis") +
  scale_y_continuous("An amazingly great y axis")

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous("Label 1") +
  scale_x_continuous("Label 2")

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous("Label 2")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color=class)) +
  scale_x_sqrt() +
  scale_color_brewer()

# 6.2.1 Exercises

ggplot(mpg, aes(displ)) +
  geom_point(aes(y=hwy)) +
  scale_y_continuous("Highway mpg")

ggplot(mpg, aes(class, displ)) +
  geom_point(aes(color=drv)) +
  scale_x_discrete("Type of car") +
  scale_y_continuous("Displacement (1)") +
  scale_color_discrete("Drive\ntrain")

# 6.3 Guides: Legends and Axes

# 6.3.1 Scale Title

df <- data.frame(x=1:2, y=1, z="a")
df
p <- ggplot(df, aes(x, y)) + geom_point()
p + scale_x_continuous("X axis")
p + scale_x_continuous(quote(a + mathematical ^ expression))

p <- ggplot(df, aes(x, y)) + geom_point(aes(color=z))
p +
  xlab("X axis") +
  ylab("Y axis")
p + labs(x="X axis", y="Y axis", color="Color\nlegend")

p <- ggplot(df, aes(x, y)) +
  geom_point() +
  theme(plot.background = element_rect(color="grey50"))
p + labs(x = "", y="")
p + labs(x = NULL, y= NULL) #""에서 베젤이 사라짐

# 6.3.2 Breaks and Labels

df <- data.frame(x = c(1,3,5) * 1000, y=1)
axs <- ggplot(df, aes(x, y)) +
  geom_point() +
  labs(x=NULL, y=NULL)
axs
axs + scale_x_continuous(breaks = c(2000, 4000))
axs + scale_x_continuous(breaks = c(2000, 4000), labels=c("2k", "4k")) #breaks 보이는 축 단위, labels 격자 이름

leg <- ggplot(df, aes(y, x, fill=x)) +
  geom_tile() +
  labs(x=NULL, y= NULL)
leg
leg + scale_fill_continuous(breaks = c(2000, 4000))
leg + scale_fill_continuous(breaks = c(2000, 4000), labels = c("2k", "4k"))

df2 <- data.frame(x = 1:3, y = c("a", "b", "c")) 
ggplot(df2, aes(x, y)) +
  geom_point()
ggplot(df2, aes(x, y)) +
  geom_point() +
  scale_y_discrete(labels = c(a = "apple", b = "banana", c = "carrot"))

axs + scale_x_continuous(breaks = NULL)
axs + scale_x_continuous(labels=NULL)

leg + scale_fill_continuous(breaks = NULL)
leg + scale_fill_continuous(labels = NULL)

axs + scale_y_continuous(labels=scales::percent_format())
axs + scale_y_continuous(labels=scales::dollar_format("$"))
leg + scale_fill_continuous(labels = scales::unit_format("k", 1e-3))

df <- data.frame(x = c(2, 3, 5, 10, 200, 3000), y= 1)
df
ggplot(df, aes(x, y)) +
  geom_point() +
  scale_x_log10()

mb <- as.numeric(1:10 %o% 10^(0:4))
ggplot(df, aes(x, y)) +
  geom_point() +
  scale_x_log10(minor_breaks = log10(mb))

# 6.3.3 Exercises
## 1
ggplot(mpg) +
  geom_point(aes(displ, hwy)) +
  scale_x_continuous(breaks = c(2:7), labels = c("2L", "3L", "4L", "5L", "6L", "7L")) +
  labs(x="Displacement", y=quote(Highway*(miles/gallon)))
                   
ggplot(mpg) +
  geom_point(aes(displ, hwy)) +
  scale_x_continuous(breaks = c(2:7), labels = paste0(2:7, "L")) +
  labs(x="Displacement", y=quote(Highway*(miles/gallon)))

##3
ggplot(mpg) +
  geom_point(aes(displ, hwy, color=drv)) +
  scale_color_discrete(labels=c("4wd", "fwd", "rwd"))

# 6.4 Legends
# 6.4.1 Layers and Legends

df <- data.frame(x=1:3, y=1:3, z=c("a", "b", "c"))

ggplot(df, aes(y, y)) +
  geom_point(size = 4, color="grey20") +
  geom_point(aes(color=z), size=2)

ggplot(df, aes(y,y)) +
  geom_point(size = 4, color="grey20", show.legend=T) +
  geom_point(aes(color=z), size=2)

norm <- data.frame(x = rnorm(1000), y = rnorm(1000))
norm$z <- cut(norm$x, 3, labels = c("a", "b", "c"))
norm
ggplot(norm, aes(x, y)) +
  geom_point(aes(color=z), alpha=0.1)
ggplot(norm, aes(x, y)) +
  geom_point(aes(color=z), alpha=0.1) +
  guides(color=guide_legend(override.aes = list(alpha=1)))

df <- data.frame(x=0, y=1:3, z=c("a", "b", "c"))

ggplot(df, aes(x, y)) + geom_point(aes(color=z))
ggplot(df, aes(x, y)) + geom_point(aes(shape=z))
ggplot(df, aes(x, y)) + geom_point(aes(shape=z, color=z))

# 6.4.2 Legend Layout
df <- data.frame(x = 1:3, y= 1:3, z = c("a", "b", "c"))
base <- ggplot(df, aes(x, y)) +
  geom_point(aes(color=z), size=3) +
  xlab(NULL) + ylab(NULL)

base + theme(legend.position = "right")
base + theme(legend.position = "bottom")
base + theme(legend.position = "none")

base <- ggplot(df, aes(x, y)) +
  geom_point(aes(color=z), size=3)
base + theme(legend.position=c(0,1), legend.justification = c(0,1))
base + theme(legend.position=c(0.5, 0.5), legend.justification = c(0.5, 0.5))
base + theme(legend.position = c(1,0), legend.justification = c(1,0))

# 6.4.3 Guide Functions
df <- data.frame(x = 1, y= 1:3, z=1:3)
base <- ggplot(df, aes(x, y)) + geom_raster(aes(fill=z))
base
base + scale_fill_continuous(guide = guide_legend())
base + guides(fill = guide_legend())

# 6.4.3.1 guide_legend()

df <- data.frame(x = 1, y=1:4, z=letters[1:4])
# Base Plot
p <- ggplot(df, aes(x, y)) + geom_raster(aes(fill = z))
p
p + guides(fill = guide_legend(ncol = 2))
p + guides(fill = guide_legend(ncol = 2, byrow = T))

p <- ggplot(df, aes(1, y)) + geom_bar(stat = "identity", aes(fill = z))
p
p + guides(fill = guide_legend(reverse = T))

# 6.4.3.2 guide_colorbar

df <- data.frame(x = 1, y = 1:4, z=4:1)
df
p <- ggplot(df, aes(x, y)) + geom_tile(aes(fill=z))
p
p + guides(fill=guide_colorbar(refverse=T))
p + guides(fill=guide_colorbar(barheight=unit(4, "cm")))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color=drv, shape=drv)) +
  guides(color=guide_legend("Drive train"), shape=guide_legend("Drive train"))

mpg

ggplot(mpg, aes(displ, hwy, color=class)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme(legend.position = "bottom") +
  guides(color=guide_legend(nrow=1)) +
  labs(color=NULL)

# 6.5 Limits

df <- data.frame(x = 1:3, y = 1:3)
base <- ggplot(df, aes(x, y)) + geom_point()
base
base + scale_x_continuous(limits = c(1.5, 2.5))
base + scale_x_continuous(limits = c(0,4))

base + xlim(0, 4)
base + xlim(4, 0)
base + lims(x = c(0,4))

ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill=density)) +
  theme(legend.position="none")
ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill=density)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(legend.position = "none")

df <- data.frame(x = 1:5)
p <- ggplot(df, aes(x, 1)) + geom_tile(aes(fill=x), color="white")
p
p + scale_fill_gradient(limits = c(2,4))
p + scale_fill_gradient(limits = c(2,4), oob=scales::squish)

# 6.5.1 Exercises

fwd <- subset(mpg, drv=="f")
rwd <- subset(mpg, drv=="r")

ggplot(fwd, aes(displ, hwy, color=class)) + geom_point() +
  scale_x_continuous(limits = c(2, 7), oob=scales::squish) +
  scale_y_continuous(limits = c(15, 45), oob=scales::squish) +
  scale_color_discrete(limits=c("compact", "midsize", "minivan", "subcompact", "2seater", "subcompact", "suv"))
  

ggplot(rwd, aes(displ, hwy, color=class)) + geom_point() +
  scale_x_continuous(limits = c(2, 7), oob=scales::squish) +
  scale_y_continuous(limits = c(15, 45), oob=scales::squish) +
  scale_color_discrete(limits=c("compact", "midsize", "minivan", "subcompact", "2seater", "subcompact", "suv"))

ggplot(mpg, aes(displ, hwy, color=class)) + geom_point() +
  scale_x_continuous(limits = c(NA, NA))

# 6.6 Scales Toolbox
# 6.6.1 Continuous Position Scales

# Convert from fuel economy to fuel consumption
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(trans="reciprocal")

# Log transform x and y axes
ggplot(diamonds, aes(price, carat)) +
  geom_bin2d() +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

base <- ggplot(economics, aes(date, psavert)) +
  geom_line(na.rm=T) +
  labs(x=NULL, y=NULL)
base

base + scale_x_date(date_labels="%y", date_breaks="5 years")
base + scale_x_date(date_labels="%Y", date_breaks="5 years")

base + scale_x_date(
  limits=as.Date(c("2004-01-01", "2005-01-01")),
  date_labels="%b %y",
  date_minor_breaks="1 month"
)
base + scale_x_date(
  limits = as.Date(c("2004-01-01", "2004-06-01")),
  date_labels="%m/%d",
  date_minor_breaks = "2 weeks"
)

# 6.6.2 Color

# 6.6.2.1 Continuous

erupt <- ggplot(faithfuld, aes(waiting, eruptions, fill=density)) +
  geom_raster() +
  scale_x_continuous(NULL, expand=c(0,0)) +
  scale_y_continuous(NULL, expand=c(0,0)) +
  theme(legend.position = "none")

erupt

erupt + scale_fill_gradient(low = "white", high="black")
erupt + scale_fill_gradient(
  low = munsell::mnsl("5G 9/2"),
  high = munsell::mnsl("5G 6/8")
)

mid <- median(faithfuld$density)
erupt + scale_fill_gradient2(midpoint=mid)

erupt + scale_fill_gradientn(colors = terrain.colors(7))
erupt + scale_fill_gradientn(colors = colorspace::heat_hcl(7))
erupt + scale_fill_gradientn(colors = colorspace::diverge_hcl(7))

erupt + scale_fill_distiller()
erupt + scale_fill_distiller(palette = "RdPu")
erupt + scale_fill_distiller(palette = "YlOrBr")

df <- data.frame(x = 1, y= 1:5, z= c(1,3,2, NA, 5))
p <- ggplot(df, aes(x, y)) + geom_tile(aes(fill = z), size=5)
p
p + scale_fill_gradient(na.value=NA)
p + scale_fill_gradient(low = "black", high = "white", na.value = "red")

# 6.6.2.2 Discrete

df <- data.frame(x = c("a", "b", "c", "d"), y = c(3,4,1,2))
bars <- ggplot(df, aes(x, y, fill=x)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none")
bars

bars + scale_fill_hue(c=40)
bars + scale_fill_hue(h = c(180, 300))

bars + scale_fill_brewer(palette = "Set1")
bars + scale_fill_brewer(palette = "Set2")
bars + scale_fill_brewer(palette = "Accent")

bars + scale_fill_grey()
bars + scale_fill_grey(start = 0.5, end = 1)
bars + scale_fill_grey(start = 0, end = 0.5)

install.packages("wesanderson")
library(wesanderson)
bars + scale_fill_manual(values = wes_palette("GrandBudapest"))
bars + scale_fill_manual(values = wes_palette("Zissou"))
bars + scale_fill_manual(values = wes_palette("Rushmore"))

df <- data.frame(x = 1:3 + runif(30), y= runif(30), z=c("a", "b", "c"))
point <- ggplot(df, aes(x, y)) +
  geom_point(aes(color=z)) +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL)
point + scale_color_brewer(palette = "Set1")
point + scale_color_brewer(palette = "Set2")
point + scale_color_brewer(palette = "Pastel1")

df <- data.frame(x = 1:3, y = 3:1, z = c("a", "b", "c"))
area <- ggplot(df, aes(x, y)) +
  geom_bar(aes(fill=z), stat = "identity") +
  theme(legend.position = "none") +
  labs(x= NULL, y= NULL)
area + scale_fill_brewer(palette = "Set1")
area + scale_fill_brewer(palette = "Set2")
area + scale_fill_brewer(palette = "Pastel1") #밝을 수록 점 그래프에서 좋고 희미할 수록 영역형에서 좋다

# 6.6.3 The Manual Discrete Scale

plot <- ggplot(msleep, aes(brainwt, bodywt)) +
  scale_x_log10() +
  scale_y_log10()
plot +
  geom_point(aes(color=vore)) +
  scale_color_manual(
    values = c("red", "orange", "green", "blue"),
    na.value = "grey50"
  )

colors <- c(
  carni = "red",
  insecti = "orange",
  herbi = "green",
  omni = "blue"
)
plot +
  geom_point(aes(color = vore)) +
  scale_color_manual(values = colors)

huron <- data.frame(year = 1875:1972, level = as.numeric(LakeHuron))

ggplot(huron, aes(year)) +
  geom_line(aes(y = level + 5, color = "above")) +
  geom_line(aes(y = level - 5, color = "below"))

ggplot(huron, aes(year)) +
  geom_line(aes(y = level + 5, color = "above")) +
  geom_line(aes(y = level - 5, color = "below")) +
  scale_color_manual("Direction",
                     values = c("above" = "red", "below" = "blue")
  )

# 6.6.4 The Identity Scale

head(luv_colours)
ggplot(luv_colours, aes(u, v)) +
  geom_point(aes(color = col), size = 3) +
  scale_color_identity() +
  coord_equal()

# 6.6.5 Exercises
