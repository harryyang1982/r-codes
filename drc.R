library(tidyverse)
library(readxl)

drc <- read_excel("dataresearch.xlsx")

for(i in 2:length(drc)) {
  drc[,i] <- round(drc[,i], 0)
}

# 이념 성향
# 보수

drc %>% 
  select(q11, q61:q64) %>% 
  cor() %>% 
  corrplot()



drc %>% 
  select(q12, q61:q64) %>% 
  cor() %>% 
  corrplot()

# 중도

drc %>% 
  select(q13, q61:q64) %>% 
  cor() %>% 
  corrplot()

# 진보

drc %>% 
  select(q14, q61:q64) %>% 
  cor() %>% 
  corrplot()

drc %>% 
  select(q15, q61:q64) %>% 
  cor() %>% 
  corrplot()


# 문항별

drc %>% 
  select(q11:q16) %>% 
  cor()

# drc_t

drc_t <- data.frame(t(drc[,2:57]), stringsAsFactors = F)
str(drc_t)

by_var <- drc_t
colnames(by_var) <- c("total", "male", "female", "g20", "g30", "g40", "g50", "g60", "seoul", "ggicn", "djchn", "pk", "tk", "gj", "kwjj")

by_var %>% 
  select(male, female) %>% 
  cor()

by_var %>% 
  select(g20:g60) %>% 
  cor() %>% 
  corrplot()

library(moonBook)

by_var %>% 
  select(male, female) %>% 
  cor() %>% 
  corrplot()

by_var %>% 
  select(seoul:kwjj) %>% 
  cor() %>% 
  corrplot()
