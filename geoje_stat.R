library(tidyverse)
library(readxl)

popu <- read_excel("geoje_ingu.xlsx")
str(popu)
labour <- read_excel("geoje_labour.xlsx", skip=1)
?read_excel

popu$X__1 <- substr(popu$X__1, 1, 4)
popu$X__1 <- as.numeric(popu$X__1)
str(popu)

names(popu) <- c("year", "total", "men", "women")
popu

popu$sex.rate <- popu$men/popu$women

ggplot(popu) +
  geom_line(aes(x=year, y=sex.rate), col="red", size=2) +
  ylim(1, 1.2) +
  ggtitle("거제시 성비 추이(1990~2016)") +
  theme(plot.title=element_text(face="bold", size=30, vjust=2, color="orange")) +
  xlab("연도") +
  ylab("성비") +
  scale_x_continuous(breaks=c(1990, 1995, 2000, 2005, 2010, 2015))


ggplot(popu) +
  geom_line(aes(x=year, y=total), size=2, col="blue") +
  scale_x_continuous(breaks=c(1990, 1995, 2000, 2005, 2010, 2015)) +
  ggtitle("거제시 인구 추이(1990~2016)") +
  theme(plot.title=element_text(face="bold", size=30, vjust=2)) +
  xlab("연도") +
  ylab("인구")

ggplot(popu) +
  geom_bar(aes(x=year, y=men), stat="identity") +
  geom_bar(aes(x=year, y=women), stat="identity")

?geom_line
?reshape
