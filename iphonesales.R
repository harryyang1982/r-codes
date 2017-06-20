library(ggplot2)
library(reshape2)

iphone.sales <- c(0.27, 1.12, 2.32, 1.7, 0.72, 6.89, 4.36, 3.79, 5.21, 7.37, 8.74, 8.75, 8.4, 14.1, 16.24, 18.65, 20.34, 17.07, 37.04, 35.06, 26.03, 26.91, 47.79, 37.43, 31.24, 33.8, 51.03, 43.72, 35.2, 39.27, 74.47, 61.17, 47.53, 48.05, 74.78, 51.19, 40.4, 45.51, 78.29, 50.76)
quarter.apple <- c(3,4, rep(1:4, 9), 1,2)
year.apple <- c(rep(2007, 2), rep(2008:2016, each=4), rep(2017, 2))

ip.sales <- data.frame(iphone.sales, year.apple, quarter.apple)
ip.sales$yq <- paste(year.apple, quarter.apple, sep="0")
ipad.sales <- c(rep(NA, 12), 3.27, 4.19, 7.33, 4.69, 9.25, 11.12, 15.43, 11.8, 17.04, 14.04, 22.86, 19.48, 14.62, 14.08, 26.04, 16.35, 13.28, 12.32, 21.42, 12.62, 10.93, 9.88, 16.12, 10.25, 9.95, 9.27, 13.08, 8.92)

apple.sales <- cbind(ip.sales, ipad.sales)

renew.apple.sales <- melt(apple.sales, 
                          id.vars = c("year.apple", "quarter.apple", "yq"),
                          variable.name = "type",
                          value.name = "sales")

ggplot(renew.apple.sales, aes(x=yq, y=sales, group=type, color=type)) +
  geom_line() +
  geom_smooth()
