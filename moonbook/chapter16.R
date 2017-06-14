#16.1 Kaplan-Meier Curve

require(survival)
require(tidyverse)

data(colon)
attach(colon)

str(colon)

fit <- survfit(Surv(time, status==1)~1, data=colon)
fit

summary(fit)
plot(fit)

plot(fit, conf.int=F)

fit1 <- survfit(Surv(time, status==1)~rx, data=colon)
plot(fit1, col=1:3, lty=1:3)
legend("topright", legend=levels(rx), col=1:3, lty=1:3)

plot(fit1, col=1:3, lty=1:3, time=F)
legend("topright", legend=levels(rx), col=1:3, lty=1:3)

plot(fit1, col=1:3, lty=1:3, fun="cumhaz", mark.time=F,
     ylab="Cumulative hazard", xlab="Days")
legend("topleft", legend=levels(rx), col=1:3, lty=1:3)

#16.2 Log-rank test

survdiff(Surv(time, status==1)~rx, data=colon) #치료법에 따른 생존 확률

# 16.3 Cox Regression

out <- coxph(Surv(time, status==1)~rx, data=colon)
out
summary(out)

# 16.4 Plot for survival curve: Better methods

# 16.4.1 GGally
require(GGally)
fit <- survfit(Surv(time, status)~rx, data=colon)
fit

ggsurv(fit)
?ggsurv

ggsurv(fit, plot.cens=F)

# 16.4.2 ggkm
library(plyr)

data(colon)
fit <- survfit(Surv(time, status)~rx, data=colon)
plot(fit, xlab="Time", ylab="Survival Probability", main="Kaplan-Meier plot")

source("ggkm.R")
ggkm(fit, timeby=500)

p <- ggkm(fit, timeby=500, return=T)
ggsave("Survival Analysis - Kaplan Meier plot.png", p)

fit <- survfit(Surv(time, status)~1, data=colon)
p <- ggkm(fit, timeby=500, return=T)

#16.4.3 rms packages
require(rms)
S <- Surv(time, status==1)
f <- npsurv(S~rx)
survplot(f, xlab="Days")

survplot(f, conf="none", xlab="Days", col=3:1)

survplot(f, conf="none", label.curve=F, label.curves=list(keys="lines"),
         xlab="", col=3:1)

survplot(f, conf="none", n.risk=T, col=3:1, xlab="")

survplot(f, conf="none", n.risk=T, y.n.risk=-0.25, cex.n.risk=0.6,
         col=3:1, xlab="")

#16.5 Cox 비례위험 모형의 자동화

data(colon)
attach(colon)
colon$TS <- Surv(time, status==1)
out <- coxph(TS~rx, data=colon)
out

summary(out)

require(moonBook)
out <- mycph(TS~.-id-study-time-status-etype, data=colon)
out

HRplot(out, type=2, show.CI = T,
       main="Hazard ratios of all individual variables")
HRplot(out, type=3, show.CI = T, sig.level=0.05,
       main="Hazard ratios of significant variables")

out2 <- coxph(TS~., data=colon)
HRplot(out2, show.CI=T, main="Cox Model with All Variables")

colon1 <- na.omit(colon)
out <- coxph(TS~.-id-study-time-status-etype-nodes, data=colon1)
final <- step(out, directin="backward")
summary(final)
HRplot(final, type=3, show.CI=T,
       main="Final Model Selected by Backward Elimination")

detach(colon)

# 16.6 rms 패키지를 이용한 Cox 비례위험 모형의 시각화

require(survival)
require(rms)
data(lung)

lung$sex <- factor(lung$sex, levels=1:2, labels=c("Male", "Female"))

TS <- with(lung, Surv(time, status))

ddist <- datadist(lung)
options(datadist="ddist")

fit <- coxph(TS ~ rcs(age, 4)+sex, lung, x=T, y=T)
par(mfrow=c(1,2))
termplot(fit, se=T, rug=T, ylab=rep("Hazard Ratio", times=2),
         main=rep("cph() plot", times=2),
         col.se=rgb(.2, .2, 1, .4), col.term="black")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
termplot2(fit, se=T, rug.type="density", rug=T, density.proportion=.05,
          se.type="polygon",
          ylab=rep("Hazard Ratio", times=2),
          main=rep("cph() plot", times=2),
          col.se=rgb(.2, .2, 1, .4), col.term="black")

fit1 <- cph(TS~rcs(age, 4)+sex, data=lung, x=T, y=T)
fit1
result <- Predict(fit1, age, sex, fun=exp)
plot(result)
plot(result, ~ age | sex)

#16.7 Cox 비례위험 모형 자동화: mycph() 함수 만들기
#16.7.1 Cox 비례위험 모형: 전통적인 방법

require(survival)
data(colon)
attach(colon)
colon$TS <- Surv(time, status==1)

out1 <- coxph(TS~rx, data=colon)
summary(out1)

#16.7.2 mycph() 함수의 필요성과 사용 예
require(moonBook)
out <- mycph(TS~., data=colon)
out

#16.7.3 함수가 하는 일
#16.7.4 함수 제작

mycph2 <- function(formula, data, digits=2) {
  call=paste(deparse(formula), ", ", "data= ", substitute(data), sep="")
  cat("\n mycph : perform coxph of individual expecting variables\n")
  cat("\n Call:", call, "\n\n")
  f=formula
  myt=terms(f, data=data)
  y=as.character(f[[2]])
  if(class(data[[y]])!="Surv") {
    cat(y, "is not a object of class Surv")
    return(invisible())
  }
  myvar=attr(myt, "term.labels")
  count=length(myvar)
  var <-HR<-lcl<-ucl<-p.value<-c()
  for(i in 1:count) {
    s=paste(y, myvar[i], sep="-")
    suppressWarnings(out<-summary(coxph(as.formula(s), data)))
    if(any(is.infinite(out$conf.int))){
      cat(dimnames(out$conf.int)[[1]], " was excluded : infinite\n")
      next
    }
    if(any(is.nan(out$coef))) {
      cat(dimnames(out$conf.int)[[1]], " was excluded : Nan\n")
      next
    }
    var=c(var,dimnames(out$conf.int)[[1]])
    HR=c(HR, out$coef[,2])
    lcl=c(lcl, out$conf.int[,3])
    ucl=c(ucl, out$conf.int[,4])
    p.value=c(p.value,out$coef[,5])
  }
  if(length(HR)<1) return(invisible())
  result = round(data.frame(HR, lcl, ucl), digits)
  rownames(result) = var
  result = cbind(result, round(p.value, max(3, digits)))
  colnames(result)[4]="p"
  result
}

out2 <- mycph2(TS~., data=colon)
