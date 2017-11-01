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

