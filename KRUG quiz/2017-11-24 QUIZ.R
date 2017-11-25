# Quiz : 고객별 초기날짜값[=init.buy] 및 다음 구매까지 걸린 일수(=terms)를 이용하여 yyyy-MM-dd 형태로 표현하는 함수(=term.dates)를 작성하시오. 

set.seed(171124)
buy.term <- function(n) floor(rgamma(n,5,1))
terms <- matrix(buy.term(30), 10, 3)
colnames(terms) <- LETTERS[1:3]
(init.buy <- c('2017-11-01','2017-11-14','2017-11-23'))
terms

# Solving

library(tidyverse)

term.dates <- function(N) {
  date <- tibble(A = parse_date(init.buy[1]), B = parse_date(init.buy[2]), C = parse_date(init.buy[3]))
  for (k in 1:nrow(N)) {
    date[k+1, ] <- date[k, ] + terms[k,]
  }
  return(date)
}
term.dates(terms)
