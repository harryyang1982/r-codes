library(foreign)
library(tidyverse)

#2016

y2016 <- read.spss("./tourstat/y2016_1.sav", to.data.frame = T)
y2016_2 <- read.spss("./tourstat/y2016_2.sav", to.data.frame = T)
y2016_3 <- read.spss("./tourstat/y2016_3.sav", to.data.frame = T)
y2016_4 <- read.spss("./tourstat/y2016_4.sav", to.data.frame = T)
y2016_5 <- read.spss("./tourstat/y2016_5.sav", to.data.frame = T)

y2016_t <- y2016 %>% 
  left_join(y2016_2, by=c("hid", "pid", "type1", "type2", "month", "m_id")) %>% 
  left_join(y2016_3, by=c("hid", "pid"))

sum_2016 <- y2016_t %>% 
  mutate(age_cut = cut(age, c(-Inf, 18, 28, 38, 48, 58, 68, Inf),
                       labels=c("g10", "g20", "g30", "g40", "g50", "g60", "g_over60"))) %>% 
  group_by(type1, sido, sex, age_cut, q6_1) %>% 
  count()

str(sum_2016)

gene_2016 <- sum_2016 %>% 
  filter(type1=="해외여행") %>% 
  group_by(age_cut, sex, q6_1) %>% 
  count()

table(gene_2016$q6_1)
write.csv(gene_2016, "./tourstat/2016t.csv")

#2015

y2015_1 <- read.spss("./tourstat/y2015_01.sav", to.data.frame = T)
y2015_2 <- read.spss("./tourstat/y2015_02.sav", to.data.frame = T)
y2015_4 <- read.spss("./tourstat/y2015_04.sav", to.data.frame = T, reencode = "UTF-8")

y2015_4 <- y2015_4 %>% 
  rename(PID=pid)

y2015 <- y2015_1 %>% 
  left_join(y2015_2, by=c("type1", "type2", "HID", "PID", "month", "count")) %>% 
  left_join(y2015_4, by=c("HID", "PID"))

sum_2015 <- y2015 %>% 
  mutate(age_cut = cut(age, c(-Inf, 18, 28, 38, 48, 58, 68, Inf),
                       labels=c("g10", "g20", "g30", "g40", "g50", "g60", "g_over60"))) %>% 
  group_by(type1, sido, sex, age_cut, q6_1) %>% 
  count()

gene_2015 <- sum_2015 %>% 
  filter(type1=="해외여행") %>% 
  group_by(age_cut, sido, q6_1) %>% 
  count()
write.csv(gene_2015, "./tourstat/2015t.csv")

#2014

y2014_1 <- read.spss("./tourstat/y2014_1.sav", to.data.frame = T)
y2014_2 <- read.spss("./tourstat/y2014_2.sav", to.data.frame = T)
y2014_3 <- read.spss("./tourstat/y2014_3.sav", to.data.frame = T)

y2014_1$pid <- as.numeric(y2014_1$pid)
y2014_2$pid <- as.numeric(y2014_2$pid)
y2014_3$pid <- as.numeric(y2014_3$pid)
y2014_1$psid <- as.numeric(y2014_1$psid)
y2014_2$psid <- as.numeric(y2014_2$psid)
y2014_3$psid <- as.numeric(y2014_3$psid)

y2014 <- y2014_1 %>% 
  left_join(y2014_2, by=c("type1", "type2", "pid", "psid", "month", "count")) %>% 
  left_join(y2014_3, by=c("pid", "psid"))

sum_2014 <- y2014 %>% 
  mutate(age_cut = cut(age, c(-Inf, 18, 28, 38, 48, 58, 68, Inf),
                       labels=c("g10", "g20", "g30", "g40", "g50", "g60", "g_over60"))) %>% 
  group_by(type1, sido1, sex, age_cut, q6_1) %>% 
  count()

table(y2014$age)

gene_2014 <- sum_2014 %>% 
  filter(type1=="해외여행") %>% 
  group_by(age_cut, sido1, q6_1) %>% 
  count() 

gene_2014 <- gene_2014[1:284,]
write.csv(gene_2014, "./tourstat/2014t.csv")


#2013

y2013_1 <- read.spss("./tourstat/y2013_1.sav", to.data.frame = T)
y2013_2 <- read.spss("./tourstat/y2013_2.sav", to.data.frame = T)
y2013_3 <- read.spss("./tourstat/y2013_3.sav", to.data.frame = T)

y2013 <- y2013_1 %>% 
  left_join(y2013_2, by=c("type1", "type2", "pid", "psid", "month", "count")) %>% 
  left_join(y2013_3, by=c("pid", "psid"))

sum_2013 <- y2013 %>% 
  mutate(age_cut = cut(age, c(-Inf, 18, 28, 38, 48, 58, 68, Inf),
                       labels=c("g10", "g20", "g30", "g40", "g50", "g60", "g_over60"))) %>% 
  group_by(type1, sido, sex, age_cut, q6_1) %>% 
  count()

gene_2013 <- sum_2013 %>% 
  filter(type1=="해외여행") %>% 
  group_by(age_cut, sido, q6_1) %>% 
  count()
write.csv(gene_2013, "./tourstat/2013t.csv")

#2012

y2012_1 <- read.spss("./tourstat/y2012_1.sav", to.data.frame = T)
y2012_2 <- read.spss("./tourstat/y2012_2.sav", to.data.frame = T)
y2012_3 <- read.spss("./tourstat/y2012_3.sav", to.data.frame = T)

y2012_1$pid <- as.numeric(y2012_1$pid)
y2012_2$pid <- as.numeric(y2012_2$pid)
y2012_3$pid <- as.numeric(y2012_3$pid)
y2012_1$psid <- as.numeric(y2012_1$psid)
y2012_2$psid <- as.numeric(y2012_2$psid)
y2012_3$psid <- as.numeric(y2012_3$psid)

y2012 <- y2012_1 %>% 
  left_join(y2012_2, by=c("type1", "type2", "pid", "psid", "month", "count")) %>% 
  left_join(y2012_3, by=c("pid", "psid"))

sum_2012 <- y2012 %>% 
  mutate(age_cut = cut(age, c(-Inf, 18, 28, 38, 48, 58, 68, Inf),
                       labels=c("g10", "g20", "g30", "g40", "g50", "g60", "g_over60"))) %>% 
  group_by(type1, sido, sex, age_cut, q6_1) %>% 
  count()

gene_2012 <- sum_2012 %>% 
  filter(type1=="해외여행") %>% 
  group_by(age_cut, sido, q6_1) %>% 
  count()
write.csv(gene_2012, "./tourstat/2012t.csv")

#2011

y2011_1 <- read.spss("./tourstat/y2011_1.sav", to.data.frame = T)
y2011_2 <- read.spss("./tourstat/y2011_2.sav", to.data.frame = T)
y2011_3 <- read.spss("./tourstat/y2011_3.sav", to.data.frame = T)

y2011 <- y2011_1 %>% 
  left_join(y2011_2, by=c("type1", "type2", "pid", "psid", "month", "count")) %>% 
  left_join(y2011_3, by=c("pid", "psid"))

y2011$sido

sum_2011 <- y2011 %>% 
  mutate(age_cut = cut(age, c(-Inf, 18, 28, 38, 48, 58, 68, Inf),
                       labels=c("g10", "g20", "g30", "g40", "g50", "g60", "g_over60"))) %>% 
  group_by(type1, sido, sex, age_cut, q6_1) %>% 
  count()

gene_2011 <- sum_2011 %>% 
  filter(type1=="해외여행") %>% 
  group_by(age_cut, sido, q6_1) %>% 
  count()
write.csv(gene_2011, "./tourstat/2011t.csv")

#2010

y2010_1 <- read.spss("./tourstat/y2010_1.sav", to.data.frame = T)
y2010_2 <- read.spss("./tourstat/y2010_2.sav", to.data.frame = T)
y2010_3 <- read.spss("./tourstat/y2010_3.sav", to.data.frame = T)

y2010_1$pid <- as.numeric(y2010_1$pid)
y2010_2$pid <- as.numeric(y2010_2$pid)
y2010_3$pid <- as.numeric(y2010_3$pid)
y2010_1$psid <- as.numeric(y2010_1$psid)
y2010_2$psid <- as.numeric(y2010_2$psid)
y2010_3$psid <- as.numeric(y2010_3$psid)

y2010 <- y2010_2 %>% 
  left_join(y2010_3, by=c("pid", "psid", "month", "count")) %>% 
  left_join(y2010_1, by=c("pid", "psid"))

sum_2010 <- y2010 %>% 
  mutate(age_cut = cut(age, c(-Inf, 18, 28, 38, 48, 58, 68, Inf),
                       labels=c("g10", "g20", "g30", "g40", "g50", "g60", "g_over60"))) %>% 
  group_by(q1, sido, sex, age_cut, q7_1_2) %>% 
  count()

gene_2010 <- sum_2010 %>% 
  filter(q1=="해외여행") %>% 
  group_by(age_cut, sido, q7_1_2) %>% 
  count()
write.csv(gene_2010, "./tourstat/2010t.csv")

#2009

y2009_1 <- read.spss("./tourstat/y2009_1.sav", to.data.frame = T)
y2009_2 <- read.spss("./tourstat/y2009_2.sav", to.data.frame = T)
y2009_3 <- read.spss("./tourstat/y2009_3.sav", to.data.frame = T)

y2009_1$pid <- as.numeric(y2009_1$pid)
y2009_2$pid <- as.numeric(y2009_2$pid)
y2009_3$pid <- as.numeric(y2009_3$pid)
y2009_1$psid <- as.numeric(y2009_1$psid)
y2009_2$psid <- as.numeric(y2009_2$psid)
y2009_3$psid <- as.numeric(y2009_3$psid)

y2009 <- y2009_1 %>% 
  left_join(y2009_2, by=c("num", "pid", "psid", "month", "count")) %>% 
  left_join(y2009_3, by=c("pid", "psid"))

sum_2009 <- y2009 %>% 
  mutate(age_cut = cut(age, c(-Inf, 18, 28, 38, 48, 58, 68, Inf),
                       labels=c("g10", "g20", "g30", "g40", "g50", "g60", "g_over60"))) %>% 
  group_by(q1, sido, sex, age_cut, q7_1_2) %>% 
  count()

gene_2009 <- sum_2009 %>% 
  filter(q1==2) %>% 
  group_by(age_cut, sido, q7_1_2) %>% 
  count()
write.csv(gene_2009, "./tourstat/2009t.csv")
