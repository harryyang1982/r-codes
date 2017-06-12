states <- as.data.frame(state.x77[, c("Murder", "Population", "Illiteracy", "Income", "Frost")])

fit <- lm(Murder~ Population+Illiteracy + Income + Frost, data=states)
summary(fit)

confint(fit)

#13.1.1 전형적인 회귀진단 방법

library(tidyverse)

fit <- lm(weight~height, data=women)
fit

# old-fashioned (internal function)
plot(weight~height, data=women)
abline(fit, col="red")
title(expression(italic(weight=3.45%*%height-87.52)))

# ggplot way
ggplot(data=women, aes(x=height, y=weight)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, color="red") +
  ggtitle(expression(italic(weight==3.45%*%height~87.52)))


par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

fit2 <- lm(weight~height+I(height^2), data=women)
par(mfrow=c(2,2))
plot(fit2)
par(mfrow=c(1,1))

newfit <- lm(weight~height + I(height^2), data=women[-c(13,15),])

par(mfrow=c(2,2))
plot(newfit)
par(mfrow=c(1,1))


states <- as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

# 13.1.2 다중공선성(Multicolinearity)

require(car)
vif(fit) 

sqrt(vif(fit))>2 ##sqrt(vif) 값이 2보다 크면 다중공선성에 문제

# 13.2 이상 관측치(outlier)

outlierTest(fit)

# 13.2.2 큰 지레점(High leverage points)

hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)

# 13.2.3 영향관측치(influential observation)

cutoff <- 4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

plot(fit, which=3)
plot(fit, which=4)
plot(fit, which=1)
plot(fit, which=2)

par(mfrow=c(1,1))
car::avPlots(fit, ask=F, id.method="identify")

car::influencePlot(fit, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

# 13.3 회귀모형의 교정 방법

## 관측치 제거
## 변수의 변환
## 변수의 추가 또는 제거
## 다른 회귀 방법의 사용

# 13.3.1 관측치 제거(deleting observation)

# 13.3.2 변수의 변환(transforming variables)

summary(car::powerTransform(states$Murder))

car::boxTidwell(Murder~Population+Illiteracy, data=states) #p-value가 높아서 lambada 전환할 필요 없음.

car::ncvTest(fit) # 필요 없음. 귀무가설 성립.
car::spreadLevelPlot(fit) # 필요 없음

# 13.3.3 변수의 추가 또는 제거

# 13.3.4 다른 회귀 방법의 사용

# 13.4 "최선의" 회귀모형 고르기

# 13.4.1 모형의 비교 - adjusted R^2 이용

states <- as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])
fit1 <- lm(Murder ~., data=states)
summary(fit1)
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
summary(fit2)

anova(fit2, fit1)

# 13.4.2 모형의 비교 - AIC 이용

AIC(fit1, fit2) # AIC가 작으면 더 좋은 모형

# 13.4.3 다중회귀모형에서 변수의 선택

# 13.4.3.1 Backward Regression

full.model <- lm(Murder~., data=states)
reduced.model <- step(full.model, direction="backward")

summary(reduced.model)

# 13.4.3.2 Forward Regression

min.model <- lm(Murder~1, data=states)
summary(min.model)
fwd.model <- step(min.model, direction="forward",
                  scope=(Murder~Population+Illiteracy+Income+Frost), trace=0)
summary(fwd.model)

# 13.4.4. 모든 부분집합회귀(all subset regression)

require(leaps)
states <- as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])
leaps <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost, data=states, nbest=4)
plot(leaps, scale="adjr2")

require(car)
subsets(leaps, statistic="cp", main="Cp Plot for All Subsets Regression")
abline(1, 1, lty=2, col="red")

?subsets

# 13.5 회귀모형의 일반화
# 13.5.1 교차검증(Cross-Validation)

shrinkage <- function(fit, k=10) {
  require(bootstrap)
  
  theta.fit <- function(x, y){lsfit(x,y)}
  theta.predict <- function(fit, x) {cbind(1, x)%*%fit$coefficients}
  
  x <- fit$model[,2:ncol(fit$model)]
  y <- fit$model[,1]
  
  results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)
  r2 <- cor(y, fit$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2-r2cv, "\n")
}

fit1 <- lm(Murder~., data=states)
shrinkage(fit1)

fit2 <- lm(Murder~Population+Illiteracy, data=states)
shrinkage(fit2)

# 13.5.2 상대적 중요성

states <- as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])

zstates <- as.data.frame(scale(states))
zfit <- lm(Murder~Population + Income + Illiteracy + Frost, data=zstates)
coef(zfit)

relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^2)
  rawwgt <- lambdasq %*% beta ^2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import), 1, drop=F]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-square", pch=19,
           main="Relatie Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}

states <- as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
relweights(fit, col="blue")
