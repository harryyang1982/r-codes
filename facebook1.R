library(dplyr)
library(data.table)
library(ggplot2)
raw.data <- 
  data.frame(
    grade = c(rep('A',30), rep('B',40), rep('C',30), rep('D',15), rep('F',5)))
data <-
  raw.data %>%
  group_by(grade) %>%
  summarise(user.count = n())
total.user.count <- data %>% summarise(sum(user.count)) %>% .[[1]]

ratio.data <- 
  data %>%
  mutate(
    ratio = user.count/total.user.count,
    cum.ratio = cumsum(ratio))
my.theme <- 
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
format.percent <- function(x) paste(floor(x*100),'%',sep='')
format.grade <- function(x) paste(x,'학점',sep='')
colour.grade <- with(ratio.data, case_when(grade=='A'~'blue', TRUE~'yellow'))
# bar chart with label
ratio.data %>%
  ggplot(aes(x='', y=ratio, alpha=grade, group=rev(grade))) +
  geom_bar(stat='identity', width=0.7, fill='red') +
  scale_y_continuous(label=format.percent) +
  scale_x_discrete(expand = c(1,1)) +
  scale_alpha_discrete(label=format.grade) +
  my.theme + 
  coord_flip() +
  geom_text(
    aes(label=format.percent(ratio)), 
    y=ratio.data$cum.ratio, 
    alpha=1, fontface='bold', hjust=0.5, 
    colour=colour.grade) +
  labs(alpha='성적')
