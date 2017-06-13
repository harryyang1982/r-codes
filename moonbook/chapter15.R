#15.1 glm() 함수

###> glm(formula, family=family(link=function), data)

#15.2 logistic regression

#15.2.1 Dose-Response

dose <- c(1,1,2,2,3,3)
response <- c(0,1,0,1,0,1)
count <-c(7,3,5,5,2,8)

toxic <- data.frame(dose, response, count)
toxic

out <- glm(response~dose, weights=count, family=binomial, data=toxic)
summary(out)

exp(1.1051)
exp(coef(out))
exp(confint(out))

plot(response~dose, data=toxic, type="n", main="Predicted Probability of Response")
curve(predict(out, data.frame(dose=x), type="resp"), add=T)

plot(response~dose, data=toxic, type="n", main="Predicted Probability of Response")
curve(predict(out, data.frame(dose=x), type="resp"), add=T)
curve(predict(out, data.frame(dose=x), type="resp", se.fit=T)$fit +
        predict(out, data.frame(dose=x), type="resp", se.fit=T)$se.fit * 1.96,
      add=T, col="blue", lty=2)
curve(predict(out, data.frame(dose=x), type="resp", se.fit=T)$fit -
        predict(out, data.frame(dose=x), type="resp", se.fit=T)$se.fit * 1.96,
      add=T, col="blue", lty=2)

#15.2.2 예2 대장암 데이터

require(survival)
str(colon)

?survival::colon

colon1 <- na.omit(colon)
result <- glm(status~rx+sex+age+obstruct+perfor+adhere+nodes+differ+extent+surg, family=binomial, data=colon1)
summary(result)

reduced.model <- step(result)
summary(reduced.model)

extractOR(reduced.model)

#15.2.3 과산포(Overdispersion)

fit <- glm(formula = status ~ rx + obstruct + adhere + nodes + extent +
          surg, family=binomial, data=colon1)
fit.od <- glm(formula = status~rx + obstruct + adhere + nodes + extent + surg, family=quasibinomial, data=colon1)
pchisq(summary(fit.od)$dispersion*fit$df.residual, fit$df.residual, lower=F)

#15.2.4 plot for odds ratios

ORplot(reduced.model, main="Plot for Odds Ratios for Reduced Model")

ORplot(reduced.model, type=2, main="Bar Plot for ORs with type=2")

ORplot(reduced.model, type=2, show.OR=F, show.CI=T, pch=15, lwd=3,
       col=c("blue", "red"), main="Plot for Odds Ratios; type=2, show.CI=TRUE")

ORplot(reduced.model, type=3, main="Bar Plot for ORs with type=3")

ORplot(reduced.model, type=3, show.CI=T, main="Bar plot for ORs with 95% CI")

# 15.3 포아송 회귀(Poisson Regression)

# 15.3.1 Breslow seizure data

data(breslow.dat, package="robust")
summary(breslow.dat[c(6,7,8,10)])

par(mfrow=c(1,2))
attach(breslow.dat)
hist(sumY, breaks=20, freq=F, col="salmon", main="Distribution of seizures")
lines(density(sumY), col="blue", lwd=2)
plot(sumY~Trt, main="Group Comparison")
par(mfrow=c(1,1))

out <- glm(sumY ~ Base+Age+Trt, family=poisson, data=breslow.dat)
summary(out)
extractOR(out, digits=3)
ORplot(out, type=3, show.OR=F, show.CI=T, main="Result of Poisson Regression")

# 15.3.2 과산포(Overdispersion) 문제

library(qcc)
qcc.overdispersion.test(breslow.dat$sumY, type="poisson")

fit <- glm(sumY~Base+Age+Trt, family=quasipoisson, data=breslow.dat)
summary(fit)

extractOR(fit, digits=3)
ORplot(fit, type=2, show.CI = T, main="Result of Quasipoisson Regression")
