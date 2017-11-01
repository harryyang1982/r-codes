library(tidyverse)

mpg2 <- subset(mpg, cyl!=5 & drv %in% c("4", "f") & class!="2seater")

base <- ggplot(mpg2, aes(displ, hwy)) +
  geom_blank() +
  xlab(NULL) +
  ylab(NULL)

base

# 7.2.1

base + facet_wrap(~class, ncol=3)
base + facet_wrap(~class, ncol=3, as.table=F)

base + facet_wrap(~class, nrow=3)
base + facet_wrap(~class, nrow=3, dir="v")


# 7.2.2 Facet Grid

base + facet_grid(. ~ cyl)
base + facet_grid(drv~.)
base + facet_grid(drv~cyl)

# 7.2.3 Controlling Scales

p <- ggplot(mpg2, aes(cty, hwy)) +
  geom_abline() +
  geom_jitter(width = 0.1, height = 0.1)
p+ facet_wrap(~cyl)
p+ facet_wrap(~cyl, scales = "free")

economics_long
ggplot(economics_long, aes(date, value)) +
  geom_line() +
  facet_wrap(~variable, scales="free_y", ncol=1)

mpg2$model <- reorder(mpg2$model, mpg2$cty)
mpg2$manufacturer <- reorder(mpg2$manufacturer, -mpg2$cty)
ggplot(mpg2, aes(cty, model)) +
  geom_point() +
  facet_grid(manufacturer~., scales = "free", space = "free") +
  theme(strip.text.y=element_text(angle = 0))

# 7.2.4 Missing Facetting Variables

df1 <- data.frame(x = 1:3, y= 1:3, gender = c("f", "f", "m"))
df2 <- data.frame(x = 2, y=2)

ggplot(df1, aes(x, y)) +
  geom_point(data=df2, color="red", size=2) +
  geom_point() +
  facet_wrap(~gender)

# 7.2.5 Grouping vs. Facetting

df <- data.frame(
  x=rnorm(120, c(0, 2, 4)),
  y=rnorm(120, c(1,2,1)),
  z=letters[1:3]
)

ggplot(df, aes(x, y)) +
  geom_point(aes(color=z))

ggplot(df, aes(x, y)) +
  geom_point() +
  facet_wrap(~z)

df_sum <- df %>% 
  group_by(z) %>% 
  summarise(x=mean(x), y=mean(y)) %>% 
  rename(z2 = z)

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_point(data=df_sum, aes(color=z2), size=4) +
  facet_wrap(~z)

df2 <- select(df, -z)

ggplot(df, aes(x, y)) +
  geom_point(data=df2, color="grey70") +
  geom_point(aes(color=z)) +
  facet_wrap(~z)

# 7.2.6 Continuous Variables

## Bins of Width 1
mpg2$disp_w <- cut_width(mpg2$displ, 1)
## Six bins of equal length
mpg2$disp_i <- cut_interval(mpg2$displ, 6)
## Six bins containing equal numbers of points
mpg2$disp_n <- cut_number(mpg2$displ, 6)

plot <- ggplot(mpg2, aes(cty, hwy)) +
  geom_point() +
  labs(x = NULL, y= NULL)
plot + facet_wrap(~disp_w, nrow=1)

plot + facet_wrap(~disp_i, nrow=1)

plot + facet_wrap(~disp_n, nrow=1)

# 7.2.7

# 7.2.7.1
ggplot(diamonds, aes(carat, price)) +
  geom_point() +
  facet_wrap(~cut)
ggplot(diamonds, aes(cut, price)) +
  geom_point() +
  facet_wrap(~carat)

# 7.2.7.2
diagg <- ggplot(diamonds, aes(carat, price)) +
  geom_point() +
  facet_wrap(~color, nrow=1)
diagg

mpg3 <- filter(mpg, class!= "2seater")
mpg2 <- select(mpg3, -class)

# 7.2.7.4
ggplot(mpg3, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(data=mpg2, aes(displ, hwy), se=F) +
  facet_wrap(~class)

# 7.4.1 Zooming into a Plot with coord_cartesian()

base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth()

base
base + scale_x_continuous(limits = c(5,7))
base + coord_cartesian(xlim=c(5,7))

ggplot(mpg, aes(displ, cty)) +
  geom_point() +
  geom_smooth()

ggplot(mpg, aes(cty, displ)) +
  geom_point() +
  geom_smooth()

ggplot(mpg, aes(displ, cty)) +
  geom_point() +
  geom_smooth() +
  coord_flip()

# 7.5 Non-linear Coordinate Systems

rect <- data.frame(x=50, y=50)
line <- data.frame(x=c(1,200), y=c(100,1))
base <- ggplot(mapping=aes(x,y)) +
  geom_tile(data=rect, aes(width=50, height=50)) +
  geom_line(data=line) +
  xlab(NULL) + ylab(NULL)

base
base + coord_polar("x")
base + coord_polar("y")

base + coord_flip()
base + coord_trans(y = "log10")
base + coord_fixed()

df <- data.frame(r=c(0,1), theta=c(0, 3/2*pi))
ggplot(df, aes(r, theta)) +
  geom_line() +
  geom_point(size=2, color="red")

interp <- function(rng, n) {
  seq(rng[1], rng[2], length=n)
}

munched <- data.frame(
  r=interp(df$r, 15),
  theta = interp(df$theta, 15)
)

ggplot(munched, aes(r, theta)) +
  geom_line() +
  geom_point(size = 2, color="red")

transformed <- transform(munched,
                         x=r*sin(theta),
                         y=r*cos(theta))

ggplot(transformed, aes(x,y)) +
  geom_point(size=2, color="red") +
  coord_fixed()

# 7.5.1 Trnasformations with coord_trans()

base <- ggplot(diamonds, aes(carat, price)) +
  stat_bin2d() +
  geom_smooth(method="lm") +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position="none")

base

base +
  scale_x_log10() +
  scale_y_log10()

pow10 <- scales::exp_trans(10)
base +
  scale_x_log10() +
  scale_y_log10() +
  coord_trans(x=pow10, y=pow10)

# 7.5.2 Polar Coordinates with coord_polar()

base <- ggplot(mtcars, aes(factor(1), fill=factor(cyl))) +
  geom_bar(width=1) +
  theme(legend.position="none") +
  scale_x_discrete(NULL, expand=c(0,0)) +
  scale_y_continuous(NULL, expand=c(0,0))

base
base + coord_polar(theta="y")
base + coord_polar()

# 7.5.3 Map Projections with coord_map()

nzmap <- ggplot(map_data("nz"), aes(long, lat, group=group)) +
  geom_polygon(fill = "white", color="black") +
  xlab(NULL) + ylab(NULL)

nzmap
nzmap + coord_quickmap()

world <- map_data("world")
worldmap <- ggplot(world, aes(long, lat, group=group)) +
  geom_path() +
  scale_y_continuous(NULL, breaks = (-2:3) * 30, labels = NULL) +
  scale_x_continuous(NULL, breaks = (-4:4) * 45, labels = NULL)

worldmap + coord_map()
worldmap + coord_map("ortho")
worldmap + coord_map("stereographic")
