#3.2 Basic Plot Types
library(ggplot2)

df <- data.frame(
  x = c(3,1,5),
  y = c(2,4,6),
  label = c("a", "b", "c")
)
df
p <- ggplot(df, aes(x, y, label=label)) +
  labs(x = NULL, y = NULL) + # Hide axis label
  theme(plot.title = element_text(size = 12))

p + geom_point() + ggtitle("point")
p + geom_text() + ggtitle("text")
p + geom_bar(stat="identity") + ggtitle("bar")
p + geom_tile() + ggtitle("raster")

p + geom_line() + ggtitle("line")
p + geom_area() + ggtitle("area")
p + geom_path() + ggtitle("path")
p + geom_polygon() + ggtitle("polygon")

# 3.2.1 Exercises

# 3.3 Labels

df <- data.frame(x = 1, y= 3:1, family=c("sans", "serif", "mono"))
ggplot(df, aes(x, y)) +
  geom_text(aes(label=family, family=family))

df <- data.frame(x = 1, y= 3:1, face=c("plain", "bold", "italic"))
ggplot(df, aes(x,y)) +
  geom_text(aes(label=face, fontface=face))

df <- data.frame(
  x=c(1,1,2,2,1.5),
  y=c(1,2,1,2,1.5),
  text=c(
    "bottom-left", "bottom-right",
    "top-left", "top-right", "center"
  )
)
ggplot(df, aes(x, y)) +
  geom_text(aes(label=text))
ggplot(df, aes(x, y)) +
  geom_text(aes(label=text), vjust="inward", hjust="inward")

df <- data.frame(trt = c("a", "b", "c"), resp=c(1.2, 3.4, 2.5))
ggplot(df, aes(resp, trt)) +
  geom_point() +
  geom_text(aes(label=paste0("(", resp, ")")), nudge_y = -0.25) +
  xlim(1, 3.6)

ggplot(mpg, aes(displ, hwy)) +
  geom_text(aes(label = model)) +
  xlim(1, 8)

ggplot(mpg, aes(displ, hwy)) +
  geom_text(aes(label = model), check_overlap = T) +
  xlim(1, 8)

label <- data.frame(
  waiting = c(55, 80),
  eruptions = c(2, 4.3),
  label = c("peak one", "peak two")
)

ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_tile(aes(fill = density)) +
  geom_label(data = label, aes(label = label))

ggplot(mpg, aes(displ, hwy, color=class)) +
  geom_point()

install.packages("directlabels")

ggplot(mpg, aes(displ, hwy, color=class)) +
  geom_point(show.legend = F) +
  directlabels:: geom_dl(aes(label=class), method="smart.grid")

# 3.4 Annotations *** 어려움 *** 

ggplot(economics, aes(date, unemploy)) +
  geom_line()

presidential <- subset(presidential, start > economics$date[1])

ggplot(data = economics) +
  geom_rect(aes(xmin=start, xmax=end, fill = party),
    ymin = -Inf, ymax = Inf, alpha = 0.2,
    data = presidential) +
  geom_vline(aes(xintercept = as.numeric(start)),
    data = presidential,
    color = "grey50", alpha = 0.5) +
  geom_text(aes(x = start, y= 2500, label = name), 
    data = presidential,
    size = 3, vjust = 0, hjust = 0, nudge_x = 50) +
  geom_line(aes(date, unemploy)) +
  scale_fill_manual(values = c("blue", "red"))

yrng <- range(economics$unemploy)
xrng <- range(economics$date)
caption <- paste(strwrap("Unemployment rates in the US have varied a lot over the years", 40), collapse = "\n")

ggplot(economics, aes(date, unemploy)) +
  geom_line() +
  geom_text(
    aes(x, y, label = caption),
    data = data.frame(x = xrng[1], y = yrng[2], caption = caption),
    hjust = 0, vjust = 1, size = 4
  )

ggplot(economics, aes(date, unemploy)) +
  geom_line() +
  annotate("text", x = xrng[1], y= yrng[2], label = caption,
           hjust=0, vjust=1, size=4)
xrng; yrng

ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d() +
  facet_wrap(~cut, nrow=1)

mod_coef <- coef(lm(log10(price) ~ log10(carat), data=diamonds))
ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d() +
  geom_abline(intercept = mod_coef[1], slope = mod_coef[2],
              color="white", size=1) +
  facet_wrap(~cut, nrow=1)

# 3.5 Collective Geoms

data(Oxboys, package="nlme")
head(Oxboys)

# 3.5.1 Multiple Groups, One Aesthetic

ggplot(Oxboys, aes(age, height, group=Subject)) +
  geom_point() +
  geom_line()

ggplot(Oxboys, aes(age, height)) +
  geom_point() +
  geom_line()

# 3.5.2 Different Groups on Different Layers

ggplot(Oxboys, aes(age, height, group=Subject)) +
  geom_line() +
  geom_smooth(method = "lm", se=F)

ggplot(Oxboys, aes(age, height)) +
  geom_line(aes(group = Subject)) +
  geom_smooth(method = "lm", size=2, se=F)

# 3.5.3 Overriding the Default Grouping

ggplot(Oxboys, aes(Occasion, height)) +
  geom_boxplot()

ggplot(Oxboys, aes(Occasion, height)) +
  geom_boxplot() +
  geom_line(color="#3366FF", alpha=0.5)

ggplot(Oxboys, aes(Occasion, height)) +
  geom_boxplot() +
  geom_line(aes(group = Subject), color="#3366FF", alpha=0.5)

# 3.5.4 Matching Aesthetics to Graphic Objects

df <- data.frame(x = 1:3, y = 1:3, color=c(1,3,5))

ggplot(df, aes(x, y, color=factor(color))) +
  geom_line(aes(group=1), size=2) +
  geom_point(size = 5)

ggplot(df, aes(x, y, color=color)) +
  geom_line(aes(group=1), size=2) +
  geom_point(size = 5)

## 50개로 쪼개서 분류하기
xgrid <- with(df, seq(min(x), max(x), length=50))
interp <- data.frame(
  x = xgrid,
  y = approx(df$x, df$y, xout=xgrid)$y,
  color=approx(df$x, df$color, xout=xgrid)$y
)
ggplot(interp, aes(x, y, color=color)) +
  geom_line(size = 2) +
  geom_point(data=df, size=5)

ggplot(mpg, aes(class)) +
  geom_bar()

ggplot(mpg, aes(class, fill=drv)) +
  geom_bar()

ggplot(mpg, aes(class, fill=hwy)) +
  geom_bar()

ggplot(mpg, aes(class, fill=hwy, group=hwy)) +
  geom_bar()

# 3.5.5 Exercises

ggplot(mpg, aes(cyl, hwy)) +
  geom_boxplot()

# 3.5.5.1
ggplot(mpg, aes(cyl, hwy, group=cyl)) +
  geom_boxplot()

# 3.5.5.2
ggplot(mpg, aes(factor(round(displ)), cty)) +
  geom_boxplot()

# 3.5.5.3

# 3.5.5.4

ggplot(mpg, aes(drv)) +
  geom_bar()

ggplot(mpg, aes(drv, fill=hwy, group=hwy)) +
  geom_bar()

library(dplyr)
mpg2 <- mpg %>% 
  arrange(hwy) %>% 
  mutate(id = seq_along(hwy))
ggplot(mpg2, aes(drv, fill=hwy, group=id)) +
  geom_bar(color="white")

# 3.5.5.5

install.packages('babynames')
library(babynames)

hadley <- filter(babynames, name == 'Hadley')
hadley
ggplot(hadley, aes(year, n, color=sex)) +
  geom_line()

# 3.6 Surface Plots

ggplot(faithfuld, aes(eruptions, waiting)) +
  geom_contour(aes(z = density, color=..level..))

ggplot(faithfuld, aes(eruptions, waiting)) +
  geom_raster(aes(fill=density))

## bubble plots work better with fewer observations
small <- faithfuld[seq(1, nrow(faithfuld), by=10),]
ggplot(small, aes(eruptions, waiting)) +
  geom_point(aes(size=density), alpha=1/3) +
  scale_size_area()

# 3.7 Drawing Maps

# 3.7.1 Vector Boundaries

library(ggplot2)
library(dplyr)
mi_counties <- map_data("county", "michigan") %>% 
  select(lon = long, lat, group, id=subregion)

head(mi_counties)

ggplot(mi_counties, aes(lon, lat)) +
  geom_polygon(aes(group = group)) +
  coord_quickmap()

ggplot(mi_counties, aes(lon, lat)) +
  geom_polygon(aes(group = group), fill= NA, color="grey50") +
  coord_quickmap()

library(USAboundaries)
c18 <- us_boundaries(as.Date("1820-01-01"))
c18df <- fortify(c18)
##> Regions defined for each Polygons
head(c18df)
ggplot(c18df, aes(long, lat)) +
  geom_polygon(aes(group=group), color="grey50", fill=NA) +
  coord_quickmap()

# 3.7.2 Point Metadata

mi_cities <- maps::us.cities %>% 
  tbl_df() %>% 
  filter(country.etc =="MI") %>% 
  select(-country.etc, lon = long) %>% 
  arrange(desc(pop))
mi_cities

ggplot(mi_cities, aes(lon, lat)) +
  geom_point(aes(size = pop)) +
  scale_size_area() +
  coord_quickmap()

ggplot(mi_cities, aes(lon, lat)) +
  geom_polygon(aes(group=group), mi_counties, fill=NA, color="grey50") +
  geom_point(aes(size=pop), color="red") +
  scale_size_area() +
  coord_quickmap()

# 3.7.3 Raster Images
