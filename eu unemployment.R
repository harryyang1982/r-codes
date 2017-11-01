library(tidyverse)

k <- read.csv(file.choose(), stringsAsFactors = F)

pk <- k %>% 
  group_by(TIME, GEO) %>% 
  filter(UNIT=="Percentage of active population")

table(pk$SEX)
glimpse(pk)
pk$Value <- gsub(":", NA, pk$Value)
pk$Value <- as.numeric(pk$Value)
pk$GEO <- gsub("Germany (until 1990 former territory of the FRG)", "Germany", pk$GEO)

pk %>% 
  filter(GEO %in% c("France", "Germany (until 1990 former territory of the FRG)", "Norway", "Sweden", "Japan", "United Kingdom", "United States")) %>% 
  filter(TIME >= 1990) %>% 
  ggplot(aes(x=TIME, y=Value)) +
  geom_line(aes(color=GEO), size=1) +
  geom_point(aes(color=GEO)) +
  scale_y_continuous(limits=c(0,13)) +
  scale_x_continuous(breaks=c(1990, 1995, 2000, 2005, 2010, 2015)) +
  scale_color_discrete(labels=c("France", "Germany", "Norway", "Sweden", "Japan", "United Kingdom", "United States")) +
  labs(color = "Country", x= "Year", y= "Unemployment Rate (%)")


