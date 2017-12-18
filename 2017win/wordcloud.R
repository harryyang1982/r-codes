library(KoNLP)
library(tidyverse)
library(wordcloud)

thesis <- readLines("2017win/ma_thesis_yang.txt")
txt <- sapply(thesis, extractNoun, USE.NAMES = F)

df_txt <- unlist(txt)
filtered_df <- df_txt %>% 
  str_replace_all("[^0-9a-zA-Z가-힣 ]", "")

wordcount <- as.tibble(table(filtered_df))

wc_df <- wordcount %>% 
  rename(word=filtered_df, frequency=n) %>% 
  filter(nchar(word) >= 2) %>% 
  arrange(desc(frequency))

pal<-brewer.pal(12,"Paired")

wordcloud(word = wc_df$word,
          freq = wc_df$frequency,
          max.words = 100,
          min.freq = 5,
          random.order = F,
          scale = c(5, 1),
          colors = pal)

