#14.1 acs data

library(moonBook)
library(tidyverse)

data(acs)

acs$smoking <- factor(acs$smoking, levels=c("Never", "Ex-smoker", "Smoker"))
plot(age~smoking, data=acs, color=c("red", "green", "blue"))
ggplot(data=acs) +
  geom_boxplot(aes(x=smoking, y=age))

densityplot(age~smoking, data=acs)
ggplot(data=acs) +
  geom_density(aes(age, color=smoking))

out1 <- lm(age~smoking, data=acs)
anova(out1)

summary(out1)

out2 <- lm(age~smoking+BMI, data=acs)
summary(out2)

colors <- rainbow(acs$smoking)
colors <- colors[c(3,2,1)]
plot(age~BMI, col=colors, data=acs)
legend("topright", legend=levels(acs$smoking), pch=1, col=colors, lty=1)
curve(85.077-0.767*x, col=colors[1], add=T)
curve(85.077-0.767*x-1.130, col=colors[2], add=T)
curve(85.077-0.767*x-8.074, col=colors[3], add=T)

require(multcomp)
tukey <- glht(out2, linfct=mcp(smoking="Tukey"))
summary(tukey)

# 14.2 예2 나이와 성별에 따른 요골동맥의 동맥경화 정도

data(radial)

out <- lm(NTAV~age, data=radial)
summary(out)

plot(NTAV~age, data=radial, col=ifelse(radial$sex=="M", "blue", "red"))
abline(out, col="red", lwd=2)
title(expression(italic(NTAV==0.385%*%Age + 44.34)), family="Times")

out1 <- lm(NTAV~age-1, data=radial)
summary(out1)

plot(NTAV~age, data=radial, col=ifelse(radial$sex=="M", "blue", "red"))
abline(out1, col="red", lwd=2)
title(expression(italic(NTAV==1.065%*%Age)), family="Times")

par(mfrow=c(2,2))
plot(out1)
par(mfrow=c(1,1))

hist(radial$NTAV)

par(mfrow=c(1,2))
plot(sqrt(NTAV)~age, data=radial, main="sqrt transformation")
plot(log(NTAV)~age, data=radial, main="Log transformation")
par(mfrow=c(1,1))

summary(car::powerTransform(radial$NTAV))

out2 <- lm(log(NTAV) ~(age-1)+male, data=radial)
summary(out2)

shapiro.test(resid(out2))

plot(log(NTAV) ~ age, data=radial, col=ifelse(radial$sex=="M", "blue", "red"))
curve(0.0594*x, col="red", lty=2, add=T)
curve(0.0594*x+0.656, col="blue", lty=1, add=T)
legend("topleft", legend=c("Male", "Female"), col=c("blue", "red"), pch=1, lty=1:2)

# 로그변환하지 않은 상태로 회귀선 그리기

plot(NTAV~age, data=radial, col=ifelse(radial$sex == "M", "blue", "red"))
curve(exp(0.0594*x), col="red", lty=2, add=T)
curve(exp(0.0594*x+0.656), col="blue", add=T)
title(expression(NTAV==e^(0.0594%*%age+0.656%*%male)), family="Times")
legend(36,170, legend=c("Male", "Female"), col=c("blue", "red"), pch=1, lty=1:2)

# 14.3 퀴즈: 남, 여를 동그라미 대신 M,F로 표시하기
plot(NTAV~age, data=radial, col=ifelse(radial$sex == "M", "blue", "red"), pch=as.character(sex))
curve(exp(0.0594*x), col="red", lty=2, add=T)
curve(exp(0.0594*x+0.656), col="blue", add=T)
title(expression(NTAV==e^(0.0594%*%age+0.656%*%male)), family="Times")
legend(36,170, legend=c("Male", "Female"), col=c("blue", "red"), pch=c("M","F"), lty=1:2)
