#7.1 한글이 포함된 간단한 데이터 만들고 읽기

names <- c("문건웅", "나미녀", "김대중")
sex <- c("M", "F", "M")
data <- data.frame(이름=names, 성별=sex)
data

write.csv(data, "test.csv")

test <- read.csv("test.csv")
test

write.csv(data, "text.csv", row.names = FALSE)
test <- read.csv("test.csv")
test

test1 <- read.csv("test.csv", fileEncoding = "euc-kr")
test1
test2 <- read.csv("test.csv", fileEncoding = "utf-8")
test2

colnames(data) <- c("name", "sex")
data

write.csv(data, "test.csv", row.names=F)
test1 <- read.csv("test.csv", fileEncoding = "euc-kr")
test1

test2 <- read.csv("test.csv", fileEncoding = "utf-8")
test2

# 7.2 인터넷에 있는 엑셀 데이터 읽기

library(readxl)
library(foreign)
library(tidyverse)

if (!file.exists("data")) {
  dir.create("data")
}

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./data/cameras.xlsx", method="curl")
dateDownloaded <- data()

cameraData <- read.csv("./data/cameras.csv")
head(cameraData)

# 7.3 엑셀 파일의 특정 열과 특정 행 읽기

colIndex <- 2:3
rowIndex <- 1:4

cameraDataSubset <- cameraData[rowIndex, colIndex]

# 7.4 엑셀 파일로 저장하기
require(moonBook)

data(acs)
write.xlsx(acs, "./data/acs.xlsx", row.names=F)
acs1 <- read.xlsx(",/data/acs.xlsx", sheetIndex=1, header=T)
head(acs1)

# 7.5 엑셀 파일을 csv 형식으로 저장하기
# 7.6 엑셀 파일과 csv 파일의 성능 비교
# 7.7 spss의 sav 파일 읽기
library(foreign)
# 7.8 자료를 파일에서 읽어들인 후 변화

str(acs)

# 7.9 문자열 변수/숫자로 입력된 변수를 범주형 변수로

str(acs$Dx)
unique(acs$Dx)
length(unique(acs$Dx))

str(acs)

select <- sapply(acs, function(x) length(unique(x)) <=3)
select

acs[, select] <- lapply(acs[, select], factor)
str(acs)

str(mtcars)

data(mtcars)

select <- sapply(mtcars, function(x) length(unique(x)) <=4)
mtcars[, select] <- lapply(mtcars[, select], factor)
str(mtcars)

?sapply

# 7.10 범주형 변수를 문자열/숫자형 변수로
str(acs)

i <- sapply(acs, is.factor)
acs[i] <- lapply(acs[i], as.character)
str(acs)

str(mtcars)



i <- sapply(mtcars, is.factor)
mtcars[i] <- lapply(mtcars[i], as.numeric)
str(mtcars)
