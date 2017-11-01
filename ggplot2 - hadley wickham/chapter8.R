library(tidyverse)

base <- ggplot(mpg, aes(cty, hwy, color=factor(cyl))) +
  geom_jitter() +
  geom_abline(color="grey50", size=2)
base

labelled <- base +
  labs(
    x="City mileage/gallon",
    y="Highway mileage/gallon",
    color="Cylinders",
    title="Highway and city mileage are highly correlated"
  ) +
  scale_color_brewer(type="seq", palette="Spectral")
labelled

styled <- labelled +
  theme_bw() +
  theme(
    plot.title = element_text(face="bold", size=12),
    legend.background = element_rect(fill = "white", size=4, color="white"),
    legend.justification = c(0,1),
    legend.position = c(0, 1),
    axis.ticks = element_line(color="grey70", size=0.2),
    panel.grid.major = element_line(color="grey70", size=0.2),
    panel.grid.minor = element_blank()
  )
styled

# 8.2 Complete Themes

df <- data.frame(x=1:3, y=1:3)
base <- ggplot(df, aes(x, y)) + geom_point()
base + theme_grey() + ggtitle("theme_grey()")
base + theme_bw() + ggtitle("theme_bw()")
base + theme_linedraw() + ggtitle("theme_linedraw()")

base + theme_light() + ggtitle("theme_lights()")
base + theme_dark() + ggtitle("theme_dark()")
base + theme_minimal() + ggtitle("theme_minimal()")

base + theme_classic() + ggtitle("theme_classic()")
base + theme_void() + ggtitle("theme_void()")

library(ggthemes)

?ggthemes

base + theme_tufte() + ggtitle("theme_tufte()")
base + theme_solarized() + ggtitle("theme_solarized()")
base + theme_excel() + ggtitle("theme_excel()")

# 8.2.1 Exercises

data("economics")
economics
library(Lahman)
data("Batting")

bak <- Batting %>% 
  mutate(avg=round(H/AB, 3)) %>% 
  group_by(teamID) %>% 
  summarise(SUM_RBI=sum(RBI), SUM_HR=sum(HR), MEAN_avg=round(mean(avg, na.rm=T),3)) %>% 
  filter(SUM_RBI > 50000)

base2 <- ggplot(bak, aes(x=MEAN_avg, y=SUM_RBI, color=teamID, size=SUM_RBI)) +
  geom_point()

base2 + theme_economist() + scale_color_economist() + ggtitle("AVG doesn't mean RBI") + xlab(NULL) + ylab(NULL)

# 8.3 Modifying Theme Components

base_t <- base + labs(title = "This is a ggplot") + xlab(NULL) + ylab(NULL)
base_t + theme(plot.title = element_text(size=16))
base_t + theme(plot.title = element_text(face = "bold", color="red"))
base_t + theme(plot.title = element_text(hjust=1))

base_t + theme(plot.title = element_text(margin=margin()))
base_t + theme(plot.title = element_text(margin=margin(t=10, b=10)))
base_t + theme(axis.title.y = element_text(margin=margin(r=10)))

base + theme(panel.grid.major = element_line(color="black"))
base + theme(panel.grid.major = element_line(size=2))
base + theme(panel.grid.major = element_line(linetype="dotted"))

base + theme(plot.background = element_rect(fill="grey80", color=NA))
base + theme(plot.background = element_rect(color="red", size=2))
base + theme(panel.background = element_rect(fill="linen"))

base
last_plot() + theme(panel.grid.minor=element_blank())
last_plot() + theme(panel.grid.major = element_blank())

last_plot() + theme(panel.background = element_blank())
last_plot() + theme(
  axis.title.x=element_blank(),
  axis.title.y=element_blank()
)
last_plot() + theme(axis.line = element_line(color="grey50"))

old_theme <- theme_update(
  plot.background = element_rect(fill = "lightblue3", color=NA),
  panel.background = element_rect(fill = "lightblue3", color=NA),
  axis.text = element_text(color = "linen"),
  axis.title = element_text(color = "linen")
)
base
theme_set(old_theme)
base

# 8.4 Theme Elements

# 8.4.1 Plot Elements
base + theme(plot.background = element_rect(color = "grey50", size = 2))
base + theme(
  plot.background = element_rect(color = "grey50", size = 2),
  plot.margin = margin(2, 2, 2, 2)
)
base + theme(plot.background = element_rect(fill = "lightblue"))

# 8.4.2 Axis Elements

df <- data.frame(x = 1:3, y = 1:3)
base <- ggplot(df, aes(x, y)) + geom_point()

base + theme(axis.line = element_line(color = "grey50", size = 1))
base + theme(axis.text = element_text(color = "blue", size = 12))
base + theme(axis.text.x = element_text(angle = -90, vjust = 0.5))

df <- data.frame(
  x = c("label", "a long label", "an even longer label"),
  y = 1:3
)
base <- ggplot(df, aes(x, y)) + geom_point()
base

base +
  theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust =0)) +
  xlab(NULL) +
  ylab(NULL)

# 8.4.3 Legend Elements

df <- data.frame(x = 1:4, y = 1:4, z = rep(c("a", "b"), each = 2))
base <- ggplot(df, aes(x, y, color = z)) + geom_point()

base + theme(
  legend.background = element_rect(
    fill = "lemonchiffon",
    color = "grey50",
    size = 1
  )
)

base + theme(
  legend.key = element_rect(color = "grey50"),
  legend.key.width = unit(0.9, "cm"),
  legend.key.height = unit(0.75, "cm")
)

base + theme(
  legend.text = element_text(size = 15),
  legend.title = element_text(size = 15, face = "bold")
)

# 8.4.4 Panel Elements

base <- ggplot(df, aes(x, y)) + geom_point()
base + theme(panel.background = element_rect(fill = "lightblue"))
base + theme(
  panel.grid.major = element_line(color = "grey60", size = 0.8)
)
base + theme(
  panel.grid.major.x = element_line(color = "grey60", size = 0.8)
)

base2 <- base + theme(plot.background = element_rect(color = "grey50"))
base2 + theme(aspect.ratio = 9 / 16)
base2 + theme(aspect.ratio = 2 / 1)
base2 + theme(aspect.ratio = 1)

# 8.4.5 Facetting Elements

df <- data.frame(x = 1:4, y = 1:4, z = c("a", "a", "b", "b"))
base_f <- ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~z)

base_f
base_f + theme(panel.margin = unit(0.5, "in"))
base_f + theme(
  strip.background = element_rect(fill = "grey20", color = "grey80", size = 1),
  strip.text = element_text(color = "white")
)

base2 + theme_dark()
last_plot() +
  theme(plot.background = element_rect(color = "black"))

# 8.5 Saving Output

pdf("output.pdf", width = 6, height = 6)
ggplot(mpg, aes(displ, cty)) + geom_point()
dev.off()

ggplot(mpg, aes(displ, cty)) + geom_point()
ggsave("output.pdf")

