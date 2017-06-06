# 11.1 Galton의 데이터

install.packages("UsingR")

require(UsingR)
data(galton) ; str(galton)

par(mfrow=c(1,2))
hist(galton$child, col="blue", breaks = 100)
hist(galton$parent, col="blue", breaks = 100)
par(mfrow=c(1,1))

cor.test(galton$child, galton$parent)

xtabs(~child+parent, data=galton)

out <- lm(child~parent, data=galton)
summary(out)

plot(child~parent, data=galton)
abline(out, col="red")

# 11.3 단순 선형회귀

women
fit <- lm(weight~height, data=women)
summary(fit)

cor.test(women$weight, women$height)

plot(weight~height, data=women)
abline(fit, col="blue")


# 11.4 다항회귀

fit2 <- lm(weight~height+I(height^2), data=women)
summary(fit2)

plot(weight~height, data=women)
lines(women$height, fitted(fit2), col="red")

fit3 <- lm(weight~height+I(height^2)+(height^3), data=women)
plot(weight~height, data=women)
lines(women$height, fitted(fit3), col="brown")

require(car)
scatterplot(weight~height, data=women)

scatterplot(weight~height, data=women, pch=19,
            spread=F, smoother.args=list(lty=2),
            main="Women Age 30-39",
            xlab="Height(inches)", ylab="Weight(lbs.)")

# 11.5 다중회귀(Multiple linear regression)

## 11.5.1 모형선택

## 11.5.2 birthwt 데이터

require(MASS)
tail(birthwt)
str(birthwt)

out <- lm(bwt ~ age+lwt+factor(race)+smoke+ptl+ht+ui, data=birthwt)
anova(out)
out2 <- lm(bwt~ lwt+factor(race)+smoke+ht+ui, data=birthwt)
anova(out2, out)

anova(out2)

# 11.6 상호작용이 있는 다중회귀

data(mtcars)
fit <- lm(mpg~hp*wt, data=mtcars)
summary(fit)

mean(mtcars$wt)
sd(mtcars$wt)

plot(mpg~hp, data=mtcars, main="Interaction of hp:wt")
curve(31.41-0.06*x, add=T)
curve(23.37-0.03*x, add=T, lty=2, col=2)
curve(15.33-0.003*x, add=T, lty=3, col=3)
legend("topright", c("2.2", "3.2", "4.2"), title="wt", lty=1:3, col=1:3)
