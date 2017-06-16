#17.1 명목형 변수에 대한 방법

data(iris)
str(iris)

head(iris)
table(iris$Species)

require(tree)
set.seed(2)
train <- sample(1:nrow(iris), nrow(iris) * 0.7)
training <- iris[train,]
testing <- iris[-train,]
itree <- tree(Species~., training)
itree

plot(itree)
text(itree, pretty=0)

ipredict <- predict(itree, testing, type="class")
table(ipredict, testing$Species)

require(caret)
confusionMatrix(ipredict, testing$Species)

#17.1.2 caret 패키지의 rpart를 이용하는 방법

inTrain <- createDataPartition(y=iris$Species, p=0.7, list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training);dim(testing)

modFit <- train(Species~., method="rpart", data=training)
print(modFit$finalModel)

plot(modFit$finalModel, uniform=T, main="Classification Tree")
text(modFit$finalModel, use.n=T, all=T, cex=0.8)

install.packages("rattle")

predict(modFit, newdata=testing)

confusionMatrix(predict(modFit, newdata=testing), testing$Species)

# 17.1.3 Party 패키지의 ctree를 이용하는 방법

require(party)
gtree <- ctree(Species~., data=iris)
plot(gtree)

plot(gtree, inner_panel = node_barplot, edge_panel = function(...) invisible(), tnex=1)

confusionMatrix(Predict(gtree), iris$Species)

plot(Petal.Length~Petal.Width, col="black", bg=Species, pch=21, data=iris)
abline(h=1.9, lwd=2, lty="dotted")
abline(v=1.7, lwd=2, col="red", lty="dotted")
abline(h=4.8, lwd=2, col="blue", lty="dotted")
legend(0.2, 6.7, legend=levels(iris$Species), pt.bg=1:3, pch=21)
text(1,2.05, "Petal.Length > 1.9")
text(2.05, 4, "Petal.Width>1.7", col="red")
text(0.7, 4.95, "Petal.Length > 4.8", col="blue")

# 17.2 연속형 변수(Continuous Variables)

tail(mtcars)
str(mtcars)

cartree <- ctree(mpg~., data=mtcars)
plot(cartree)

Node2 <- mtcars[mtcars$wt<=2.32,]
Node2
median(Node2$mpg)
summary(Node2$mpg)

Node4 <- mtcars[(mtcars$disp <=258) & (mtcars$wt > 2.32),]
nrow(Node4)
summary(Node4$mpg)

Node5 <- mtcars[(mtcars$disp>258),]
summary(Node5$mpg)

library(mycor)
res <- mycor(mtcars[, c(3,1,4,6)])
plot(res, type=4)
res

plot(res, type=2)

# 17.3 생존나무(survival tree) 분석을 통한 생존기간의 예측

data("GBSG2", package="TH.data")
head(GBSG2)

require(survival)
stree <- ctree(Surv(time, cens) ~ ., data=GBSG2)
plot(stree)

GBSG2[c(1,2,6,8), c(1,6,7)]

treeresponse(stree, newdata=GBSG2[c(1,2,6,8),])
