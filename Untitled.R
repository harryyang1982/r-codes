require(xtable)
print(xtable(head(mtcars[c(1:6)]), caption="xtable: Caption size discrepancy and misplacement"),
      size="large", latex.environments = "flushright",
      caption.placement="top")

require(ztable)
options(ztable.type="latex")
options(ztable.zebra=1)
options(ztable.zebra.color="platinum")

.Options

ztable(head(mtcars[1:6]), size=6, zebra=NULL, position="r", caption="ztable: Caption size matching and within the table position")

#19.1 기본 사용법: 데이터 프레임
#19.2.1 zebra-striping table 만들기

ztable(head(mtcars[1:5]), zebra=1)

z <- ztable(head(mtcars[1:5]), zebra=2, zebra.color="peach")

print(z, type="html")

head(zcolors)

#19.2.2 미리 지정된 색 사용
ztable(head(iris, 12), zebra=0, zebra.color=NULL)

#19.2.3 사용자가 지정한 색 사용
ztable(head(iris, 12), zebra=0, zebra.color=c("peach", "platinum", "snow"))

# 19.3 표 모양의 사용자화
# 19.3.1 표제목의 placement, position

ztable(head(mtcars[1:8]), caption="Table 18-1. Caption with default placement and position")
ztable(head(mtcars[1:8]), caption="Table 18-2. Right-sided caption with default placement",
       caption.position="r")

ztable(head(mtcars[1:8]),
       caption="Table 18-3. Left-sided caption with bottom placement",
       caption.placement="bottom",
       caption.position="l")

ztable(head(mtcars[1:8]),
       caption="Table 18-4. Caption with bold font",
       caption.bold=T)

# 19.3.2 표에 사용되는 폰트의 크기

ztable(head(mtcars[1:9]), size=3,
       caption="Table 18-5. size=3")

ztable(head(mtcars[1:5], size=7,
            caption="Table 18-6. size=7"))

# 19.3.3 표의 placement, 위치(position)

ztable(head(mtcars[1:3]), caption="Default position")
ztable(head(mtcars[1:3]), caption="Left-sided table", position='l')
ztable(head(mtcars[1:3]), caption="Right-sided table", position="r")

# 19.3.4 열이름, 행이름 표시하기

ztable(head(mtcars[1:5], 3), caption="include.rownames=FALSE",
       include.rownames=F)
ztable(head(mtcars[1:5], 3), caption="include.colnames=FALSE",
       include.colnames=F)

# 19.3.5 소수점 아래 자릿수: digits 인수

ztable(head(mtcars[1:5], caption="digits: default"))
ztable(head(mtcars[1:5]), caption="digits: c(0,1,2,3,0,1)", 
       digits=c(0,1,2,3,0,1))

# 19.3.6 열 데이터의 배치, 세로줄 넣기
ztable(head(iris))
ztable(head(iris), caption="align=\"llccrr\"", align="llccrr")
ztable(head(iris), caption="align=\"|r|rrrrr|\"", align="|r|rrrrr|")

ztable(head(iris), caption="align=\"||r|rrrr|r||\"", align="||r|rrrr|r||")

# 19.3.7 보다 나은 표 모양을 위한 booktabs 사용하기

ztable(head(mtcars[1:5], 3), caption="booktabs=TRUE", 
       booktabs=T)

ztable(head(mtcars[1:3]), caption="wraptable=TRUE",
       wraptable=TRUE, wraptablewidth=7, position="l")

# 19.3.9 표의 회전
ztable(head(mtcars[1:3]), caption="turn=TRUE, angle=30",
       turn=TRUE, angle=30)

# 19.3.10 세로로 표 출력
ztable(head(mtcars, 15), caption="sidewaystable==TRUE",
       sidewaystable=TRUE)

# 19.3.11 longtable출력
ztable(head(iris, 50), caption="Example of longtable",
       longtable=TRUE)

# 19.3.12 표 모양 옵션을 한꺼번에

options(ztable.include.rownames=T)
options(ztable.include.colnames=T)
options(ztable.type="latex")
options(ztable.show.heading=T)
options(ztable.show.footer=T)
options(ztable.caption.placement="top")
options(ztable.caption.position="c")
options(ztable.caption.bold=F)
options(ztable.booktabs=F)
options(ztable.zebra=NULL)
options(ztable.zebra.color=NULL)
options(ztable.colnames.bold=F)

getOption("ztable.booktabs")

## 모두다 TRUE로 하려면
options(ztable.booktabs=TRUE)
getOption("ztable.booktabs")

# 19.4 aov 결과물에 사용
out <- aov(mpg~., data=mtcars)
ztable(out)

# 19.5 선형회귀모형: 'lm' 객체에 사용
fit <- lm(mpg~cyl+disp+wt+drat+am, data=mtcars)
ztable(fit)

# 19.6 ANOVA표 출력: 'anova' 객체에 이용
a <- anova(fit)
ztable(a)

fit2 <- lm(mpg~cyl+wt, data=mtcars)
b <- anova(fit2, fit)
ztable(b)

ztable(b, show.heading=F)

# 19.7 일반화선형모형: 'glm' 객체에 사용

require(survival)
data(colon)
attach(colon)
out <- glm(status ~ rx+obstruct+adhere+nodes,
           data=colon, family=binomial)
ztable(out)

ztable(anova(out))

# 19.8 다른 'aov', 'lm', 'glm' 객체에 사용
# 19.8.1 다른 'aov' 객체 출력 예

op <- options(contrasts = c("contr.helmert", "contr.poly"))
npk.aov <- aov(yield ~ block + N*P*K, npk)
ztable(npk.aov, zebra=1)

# 19.8.2 다른 'lm' 객체 출력 예
ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
group <- gl(2, 10, 20, labels=c("Ctl", "Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
ztable(lm.D9)
ztable(anova(lm.D9), booktabs=F, align="|c|rrrr|r|")

# 19.8.3 다른 'glm'객체 출력 예
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- gl(3, 1, 9)
treatment <- gl(3,3)
d.AD <- data.frame(treatment, outcome, counts)
glm.D93 <- glm(counts ~ outcome + treatment, family= poisson())
ztable(glm.D93)

# 19.9 주성분분석: 'prcomp' 객체에 사용

data("USArrests")
pr1 <- prcomp(USArrests)
ztable(pr1)

ztable(summary(pr1))

# 19.10 생존분석: 'coxph' 객체에 사용
colon$TS <- Surv(time, status==1)
out <- coxph(TS~rx+obstruct+adhere+differ+extent, data=colon)
ztable(out)

# 19.11 두 개 이상의 ztable 또는 그림을 나란히 출력

z <- ztable(head(mtcars[1:2]), tabular=T, zebra.color="peach-orange")
z1 <- ztable(tail(mtcars[1:2], tabular=T, zebra=2))

parallelTables(width=c(0.5, 0.5), list(z, z1))

z <- ztable(head(mtcars[1:2]), turn=T, angle=15, zebra=2)
z1 <- ztable(head(mtcars[1:2]), turn=T, angle=-15, zebra=2)
parallelTables(width=c(0.5, 0.5), list(z, z1))
