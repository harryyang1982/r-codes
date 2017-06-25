library(readxl)
library(tidyverse)

mileage <- read_excel("mileage_update4.xlsx", sheet=3, skip=2)
mileage <- mileage[-c(121:128),]
mileage <- mileage[,-1]

mileage %>% 
  filter(마일리지합계 != 0) %>% 
  summarise(SUM=sum(마일리지합계), AVERAGE=mean(마일리지합계), N=n())

mean(mileage$마일리지합계)

m_summary <- mileage %>% 
  summarise(SUM=sum(마일리지합계), SUM_T=sum(시간합계), AVERAGE_M=mean(마일리지합계), AVERAGE_T=mean(시간합계), AVERAGE_NT=mean(비교과시간), SD_M=sd(마일리지합계), SD_T=sd(시간합계), SD_NT=sd(비교과시간),N=n())

m_summary2 <- mileage[mileage$학년 %in% c(2,3,4),] %>% 
  summarise(SUM=sum(마일리지합계), SUM_T=sum(시간합계), AVERAGE_M=mean(마일리지합계), AVERAGE_T=mean(시간합계), AVERAGE_NT=mean(비교과시간), SD_M=sd(마일리지합계), SD_T=sd(시간합계), SD_NT=sd(비교과시간),N=n())


m_year <- mileage %>% 
  group_by(학년) %>% 
  summarise(SUM=sum(마일리지합계), SUM_T=sum(시간합계), AVERAGE_M=mean(마일리지합계), AVERAGE_T=mean(시간합계), AVERAGE_NT=mean(비교과시간), SD_M=sd(마일리지합계), SD_T=sd(시간합계), SD_NT=sd(비교과시간),N=n())

m_gender <- mileage %>% 
  group_by(성별) %>% 
  summarise(SUM=sum(마일리지합계), AVERAGE_M=mean(마일리지합계), AVERAGE_T=mean(시간합계), N=n())

mileage$비교과시간 <- mileage$시간합계 - mileage$교과이수
mileage[is.na(mileage$비교과시간),]$비교과시간 <- 0

ggplot(mileage) +
  geom_bar(aes(학년, 마일리지합계, fill=학년), stat="identity")

nonacademy <- mileage %>% 
  select(`특강 및 멘토링 참가`:`자격증 취득`)
summary(nonacademy)

length(nonacademy)

cols <- c(1:9)

nonac <- data.frame(apply(nonacademy[, cols], 2, function(x) as.numeric(x)))
str(nonac)

nonsum <- data.frame(colSums(nonac, na.rm=T))
nonavg <- data.frame(colMeans(nonac, na.rm=T))

nonacad <- cbind(nonsum, nonavg)
colnames(nonacad) <- c("합계", "평균")
nonacad$항목 <- rownames(nonacad)
rownames(nonacad) <- NULL
nonacad$항목 <- factor(nonacad$항목, levels =nonacad$항목[order(nonacad$합계, decreasing = T)])

nonacad$N <- nonacad$합계 / nonacad$평균
sum(nonacad[-7,]$N)

# graph

ggplot(m_year) +
  geom_bar(aes(학년, AVERAGE_M, fill=학년), stat="identity") +
  geom_abline(intercept=mean(mileage$마일리지합계), slope=0, color="pink") +
  geom_text(aes(x=학년, y=AVERAGE_M+10, label=round(AVERAGE_M, 2))) +
  ggtitle("학년별 마일리지 획득 평균 비교 (전체 평균 = 160.49점)")

ggplot(m_year) +
  geom_bar(aes(학년, AVERAGE_T, fill=학년), stat="identity") +
  geom_abline(intercept=mean(mileage$시간합계), slope=0, color="pink") +
  geom_text(aes(x=학년, y=AVERAGE_T+10, label=round(AVERAGE_T, 2))) +
  ggtitle("학년별 학습시간 평균 비교 (전체 평균 = 96.90시간)")

ggplot(m_year) +
  geom_bar(aes(학년, AVERAGE_NT, fill=학년), stat="identity") +
  geom_abline(intercept=mean(mileage$비교과시간), slope=0, color="pink") +
  geom_text(aes(x=학년, y=AVERAGE_NT+2, label=round(AVERAGE_NT, 2))) +
  ggtitle("학년별 비교과 학습시간 비교 (전체 평균 = 18.72점)")

ggplot(m_year) +
  geom_bar(aes(학년, AVERAGE_NT, fill=학년), stat="identity") +
  geom_abline(intercept=mean(mileage$비교과시간), slope=0, color="pink") +
  geom_text(aes(x=학년, y=AVERAGE_NT+2, label=round(AVERAGE_NT, 2))) +
  ggtitle("학년별 비교과 학습시간 비교 (전체 평균 = 18.72점)")

ggplot(nonacad) +
  geom_bar(aes(항목, 합계, fill=항목), stat="identity") +
  geom_text(aes(항목, 합계+15, label=합계)) +
  geom_point(aes(항목, 평균)) +
  geom_boxplot(aes(항목, 평균)) +
  geom_text(aes(항목, 평균+15, label=round(평균,2)), color="white") +
  ggtitle("사업/프로그램별 참여도 분포(시간)")

# total graph

ggplot(m_year) +
  geom_bar(aes(학년, SUM, fill=학년), stat="identity") +
  geom_text(aes(x=학년, y=SUM+200, label=SUM)) +
  ggtitle("학년별 마일리지 비교") +
  geom_segment(aes(x = 학년, xend=학년,
                   y= AVERAGE_M-SD_M, yend = AVERAGE_M+SD_M), color="skyblue1", lwd=2) +
  geom_point(aes(학년, AVERAGE_M), size=3)  + 
  theme(axis.text=element_text(size=10)) +
  xlab("학년") + ylab("마일리지") +
  geom_text(aes(x=학년, y=AVERAGE_M+300, 
                label=paste0("mean= ",round(AVERAGE_M,2),", ", "s.d= ",round(SD_M,2))), color="white") +
  geom_abline(slope=0, intercept=mean(mileage$마일리지합계), lwd=1, color="pink") +
  annotate("text", label="mean=160.49", x=0.7, y=6100, size=5, color="blue")

ggplot(m_year) +
  geom_bar(aes(학년, SUM_T, fill=학년), stat="identity") +
  geom_text(aes(x=학년, y=SUM_T+200, label=SUM_T)) +
  ggtitle("학년별 학습시간 비교") +
  geom_segment(aes(x = 학년, xend=학년,
                   y= AVERAGE_T-SD_T, yend = AVERAGE_T+SD_T), color="skyblue1", lwd=2) +
  geom_point(aes(학년, AVERAGE_T), size=3)  + 
  theme(axis.text=element_text(size=10)) +
  xlab("학년") + ylab("시간") +
  geom_text(aes(x=학년, y=AVERAGE_T+150, 
                label=paste0("mean= ",round(AVERAGE_T,2),", ", "s.d= ",round(SD_T,2))), color="white") +
  geom_abline(slope=0, intercept=mean(mileage$시간합계), lwd=1, color="pink") +
  annotate("text", label="mean=160.49", x=0.7, y=3500, size=5, color="blue")

ggplot(mileage[mileage$마일리지합계 < 50,], aes(마일리지합계, fill=factor(학년))) +
  geom_histogram()

mileage %>% 
  count(마일리지합계<50)

# marinemap

marinemap <- read_excel("marinemap.xlsx")

mmap <- marinemap %>% 
  summarise(M=mean(Final_M), A=mean(Final_A), R=mean(Final_R), I=mean(Final_I), N=mean(Final_N), E=mean(Final_E))

devtools::install_github("ricardo-bion/ggradar", 
                         dependencies=TRUE)

library(ggradar)
ggradar(mmap)


mtcars %>%
  add_rownames( var = "group" ) %>%
  mutate_each(funs(rescale), -group) %>%
  tail(4) %>% select(1:10) -> mtcars_radar
