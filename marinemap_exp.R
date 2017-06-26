library(readxl)
library(tidyverse)

marinemap <- read_excel("marinemap2.xlsx", skip=5)
marinemap <- marinemap[,-c(27:34)]

colnames(marinemap)[1:2] <- c("학년", "성명")
nsubject <- marinemap[1,]
marinemap <- marinemap[-c(1:3),]


kwd <- substr(colnames(marinemap), 1,1)

marinemap_list <- list(NULL)
name <- t(marinemap[,2])

for (i in 1:nrow(marinemap)) {
  record1 <- list(marinemap[i,])
  marinemap_list[i] <- record1
}

head(marinemap_list, 3)
tail(marinemap_list, 3)
marinemap_list[118]

# new way

new_map <- marinemap %>% 
  gather(3:dim(marinemap)[2], key="subject", value="score")

knitr::kable(new_map %>% head())

?kable

new_map2 <- new_map %>% 
  mutate(subject = sapply(1:length(new_map$subject), function(x) strsplit(new_map$subject, "_")[[x]][1]) %>% 
  as.factor())

new_map3 <- new_map2 %>% 
  mutate(score = as.numeric(score))

knitr::kable(new_map3 %>% head())
library(knitr)

# sort data by score
sort_dat <- new_map3 %>% arrange(subject, 성명, desc(score))
# remove NA ?  # it is not desired. I am not sure.
sort_dat <- sort_dat[sort_dat$score %>% is.na() %>% not(), ]
knitr::kable(sort_dat %>% head())
