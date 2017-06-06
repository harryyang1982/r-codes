# 12.1 Galton 데이터

library(UsingR)

data(galton)
table(galton$child, galton$parent)
xtabs(~child+parent, data=galton) ## 위와 같음

out <- lm(child~parent, data=galton)
summary(out)

plot(child~parent, data=galton)
abline(out, col="red")

# 12.1.1 해결 방법
# 12.1.1.1 해결 방법 1. jittering
plot(jitter(child, 5) ~ jitter(parent,5), galton)

# 12.1.1.2 해결 방법 2. sunflowerplot

sunflowerplot(galton)

# 12.1.1.3 해결 방법 3. 점 크기 조절

freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freqData$parent)),
     as.numeric(as.vector(freqData$child)),
     pch = 21, col = "black", bg = "lightblue",
     cex = .15 * freqData$freq,
     xlab = "parent", ylab = "child")
lm1 <- lm(galton$child ~ galton$parent)
lines(galton$parent, lm1$fitted.values, col="red", lwd=3)

# 12.1.2 보충 설명

table(galton$child, galton$parent)
freqData <- data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
tail(freqData)

plot(freqData$child~freqData$parent, pch=21, col="black", bg="lightblue", cex=0.15*freqData$freq)
str(freqData)
plot(as.numeric(as.vector(freqData$child))~as.numeric(as.vector(freqData$parent)), pch=21, col="black", bg="lightblue", cex=0.15*freqData$freq, xlab="parent", ylab="child")
lm1 <- lm(galton$child ~ galton$parent)
lines(galton$parent, lm1$fitted.values, col="red", lwd=3)

summary(lm1)

# 12.2 고밀도 산점도(High-density scatter plot)
require(SwissAir)
data("AirQual")
str(AirQual)

Ox <- AirQual[,c("ad.O3", "lu.O3", "sz.O3")] + AirQual[, c("ad.NOx", "lu.NOx", "sz.NOx")] - AirQual[,c("ad.NO", "lu.NO", "sz.NO")]
Ox
names(Ox) <- c("ad", "lu", "sz")
Ox
names(Ox)
str(Ox)

plot(lu~sz, data=Ox)

# 12.2.1 hexbin 패키지의 hexbin() 사용
install.packages("hexbin")
require(hexbin)
bin <- hexbin(Ox$lu, Ox$sz, xbins=50)
plot(bin, main="Hexagonal binning ")

# 12.2.2 smoothScatter() 이용
smoothScatter(Ox$lu, Ox$sz, main="Scatterplot by Smoothed Densities")

Lab.palette <- colorRampPalette(c("blue", "orange", "red"), space="Lab")
smoothScatter(Ox$lu, Ox$sz, colramp = Lab.palette)

# 12.2.3 IDPmisc 패키지의 iplot 이용

install.packages("IDPmisc")
require(IDPmisc)
iplot(Ox$lu, Ox$sz, xlab="Schwyz", ylab="Lucerne", 
      main = "Image Scatter Plot with Color Indicating Density")
ipairs(Ox)

ilagplot(Ox$ad, set.lags = 1:9,
         ztransf = function(x) {x[x<1] <- 1; log2(x)})
