library(foreign)
library(tidyverse)

y2016 <- read.spss(file.choose(), to.data.frame = T)
y2016_2 <- read.spss(file.choose(), to.data.frame = T)
y2016_3 <- read.spss(file.choose(), to.data.frame = T)
y2016_4 <- read.spss(file.choose(), to.data.frame = T)
y2016_5 <- read.spss(file.choose(), to.data.frame = T)

y2016_t <- y2016 %>% 
  left_join(y2016_2, by=c("hid", "pid", "type1", "type2", "month", "m_id")) %>% 
  left_join(y2016_3, by=c("hid", "pid"))

sum_2016 <- y2016_t %>% 
  mutate(age_cut = cut(age, c(-Inf, 18, 28, 38, 48, 58, 68, Inf),
                       labels=c("g10", "g20", "g30", "g40", "g50", "g60", "g_over60"))) %>% 
  group_by(type1, sido, sex, age_cut, q6_1) %>% 
  count()

str(sum_2016)

gene_2016 <- sum_2016 %>% 
  filter(type1=="해외여행") %>% 
  group_by(age_cut, q6_1) %>% 
  count()

table(gene_2016$q6_1)
