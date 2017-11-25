library(tidyverse)

set.seed(171124)
buy.term <- function(n) floor(rgamma(n,5,1))
terms <- matrix(buy.term(30), 10, 3)
colnames(terms) <- LETTERS[1:3]
(init.buy <- c('2017-11-01','2017-11-14','2017-11-23'))
terms

term.dates <- function(N) {
  date <- tibble(A = parse_date(init.buy[1]), B = parse_date(init.buy[2]), C = parse_date(init.buy[3]))
  for (k in 1:nrow(N)) {
    date[k+1, ] <- date[k, ] + terms[k,]
  }
  return(date)
}
term.dates(terms)
