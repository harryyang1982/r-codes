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
