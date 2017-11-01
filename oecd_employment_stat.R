library(tidyverse)

oecd_tenure <- read.csv(file.choose(), stringsAsFactors = F)

table(oecd_tenure$Age)

glimpse(oecd_tenure)

table(oecd_tenure$SEX)
table(oecd_tenure$COUNTRY)

kr_tenure <- oecd_tenure %>% 
  filter(COUNTRY=="KOR" & Age=="Total" & SEX=="MW") %>% 
  ggplot(aes(x=TIME, y=Value)) +
  geom_line(aes(color=TENURE))

ger_tenure <- oecd_tenure %>% 
  filter(COUNTRY=="DEU" & Age=="Total" & SEX=="MW") %>% 
  ggplot(aes(x=TIME, y=Value)) +
  geom_line(aes(color=TENURE))

ger_tenure
