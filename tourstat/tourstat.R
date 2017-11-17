library(data.table)
library(tidyverse)
library(readxl)

#import

y2009 <- fread(file.choose())
y2010 <- fread(file.choose())
y2011 <- fread(file.choose())
y2012 <- fread(file.choose())
y2013 <- fread(file.choose())
y2014 <- fread(file.choose())
y2015 <- fread(file.choose())
y2016_1 <- fread(file.choose())
y2016_2 <- fread(file.choose())

#preprocessing
y2009 <- left_join(y2009, y2009_2, by=c("num","pid","psid", "month", "count"))
write.csv(y2009, "./tourstat/y2009.csv")

#tidying_GUBUN

y2010_t <- y2010 %>% 
  gather(gubun, g_type, GUBUN.1:GUBUN.31)
#
head(y2010_t)

y2010_t %>% 
  select(HID_10, PID_10, Year, gubun, g_type)

prac <- y2010_t %>% 
  select(HID_10, PID_10, Year, gubun, g_type) %>% 
  group_by(HID_10, PID_10, g_type) %>% 
  count() %>% 
  na.omit()
         
prac <- prac %>% 
  spread(g_type, n)

colnames(prac) <- c("HID_10", "PID_10", "local_count", "global_count")

y2010_t1 <- y2010 %>% 
  left_join(prac, by=c("HID_10", "PID_10"))

y2010_t1 <- y2010_t1 %>% 
  select(-(GUBUN.1:GUBUN.31))

y2010_t <- y2010_t1

# tidying_type

y2010_t1 <- y2010_t %>% 
  gather(type, t_value, type1.1:type1.31)

y2010_t1 %>% 
  select(HID_10, PID_10, Year, type, t_value)

prac <- y2010_t1 %>% 
  select(HID_10, PID_10, Year, type, t_value) %>% 
  group_by(HID_10, PID_10, t_value) %>% 
  count() %>% 
  na.omit()

prac <- prac %>% 
  spread(t_value, n)

colnames(prac) <- c("HID_10", "PID_10", "local_tour", "global_tour", "no_tour", "north_korea_tour")
y2010_t1 <- y2010_t %>% 
  left_join(prac, by=c("HID_10", "PID_10"))

y2010_t <- y2010_t1
y2010_t <- y2010_t %>% 
  rename(family_tour=local_count) %>% 
  rename(individual_tour=global_count)

y2010_t <- y2010_t %>% 
  select(-(type1.1:type1.31))

y2010_t %>% 
  select(HID_10, PID_10, family_tour, individual_tour)

#tidying_months

y2010_t %>% 
  select(Month.1:Month.31) %>% 
  rowSums(na.rm=T) -> y2010_t$total_tour_days

y2010_t %>% 
  select(HID_10, PID_10, total_tour_days)

y2010_t <- y2010_t %>% 
  select(-(Month.1:Month.31)) 

y2010_t <- y2010_t %>% 
  select(-(Count.1:Count.31))

#tidying_questions_1(당일/숙박)

y2010_t1 <- y2010_t %>% 
  gather(type, q1_value, q1.1:q1.31)

y2010_t1 %>% 
  select(HID_10, PID_10, Year, type, q1_value) %>% 
  head(20)

prac <- y2010_t1 %>% 
  select(HID_10, PID_10, Year, type, q1_value) %>% 
  group_by(HID_10, PID_10, q1_value) %>% 
  count() %>% 
  na.omit()

prac <- prac %>% 
  spread(q1_value, n)

colnames(prac) <- c("HID_10", "PID_10", "daily_tour", "days_tour", "nk_tour")

y2010_t1 <- y2010_t %>% 
  left_join(prac, by=c("HID_10", "PID_10"))

y2010_t1 <- y2010_t1 %>% 
  select(-(q1.1:q1.31))

y2010_t <- y2010_t1

# deleting

y2010_t1 <- y2010_t1 %>% 
  select(-(q2_c_2.10:q2_c_2.31))

y2010_t <- y2010_t1

# tidying q3

y2010_t1 <- y2010_t %>% 
  gather(type, q3_value, q3.1:q3.31)

y2010_t1 %>% 
  select(HID_10, PID_10, Year, type, q3_value) %>% 
  head(20)

prac <- y2010_t1 %>% 
  select(HID_10, PID_10, Year, type, q3_value) %>% 
  group_by(HID_10, PID_10, q3_value) %>% 
  count() %>% 
  na.omit()

prac <- prac %>% 
  spread(q3_value, n)

colnames(prac) <- c("HID_10", "PID_10", "vacation", "visit", "cure", "religion", "edu_less_6", "edu_more_6", "business_trip", "etc_trip")

y2010_t1 <- y2010_t %>% 
  left_join(prac, by=c("HID_10", "PID_10"))

y2010_t1 <- y2010_t1 %>% 
  select(-(q3.1:q3.31))

y2010_t <- y2010_t1

y2010_t <- y2010_t %>% 
  select(-(q4_a.1:q4_1_b.31))

# tidying q5

y2010_t1 <- y2010_t %>% 
  gather(type, q5_value, q5.1:q5.31)

y2010_t1 %>% 
  select(HID_10, PID_10, Year, type, q5_value) %>% 
  head(20)

prac <- y2010_t1 %>% 
  select(HID_10, PID_10, Year, type, q5_value) %>% 
  group_by(HID_10, PID_10, q5_value) %>% 
  count() %>% 
  na.omit()

table(prac$q5_value)

prac <- prac %>% 
  spread(q5_value, n)

colnames(prac) <- c("HID_10", "PID_10", "team", "single")

y2010_t1 <- y2010_t %>% 
  left_join(prac, by=c("HID_10", "PID_10"))

y2010_t1 <- y2010_t1 %>% 
  select(-(q5.1:q5.31))

y2010_t <- y2010_t1

y2010_t <- y2010_t %>% 
  select(-(q5_1.1:q5_1.31))

# tidying q5.2.1.1

y2010_t1 <- y2010_t %>% 
  gather(type, q5_value, q5_2_1.1:q5_2_4.31)

y2010_t1 %>% 
  select(HID_10, PID_10, Year, type, q5_value) %>% 
  head(20)

prac <- y2010_t1 %>% 
  select(HID_10, PID_10, Year, type, q5_value) %>% 
  group_by(HID_10, PID_10, q5_value) %>% 
  count() %>% 
  na.omit()

table(prac$q5_value)

prac <- prac %>% 
  spread(q5_value, n)

colnames(prac) <- c("HID_10", "PID_10", "with_friend", "with_colleague", "with_neighbor", "with_family", "with_relatives", "etc_people")

y2010_t1 <- y2010_t %>% 
  left_join(prac, by=c("HID_10", "PID_10"))

y2010_t1 <- y2010_t1 %>% 
  select(-(q5_2_1.1:q5_2_4.31))

y2010_t <- y2010_t1

tail(colnames(y2010_t), 30)

y2010_t <- y2010_t %>% 
  select(-(q9.1:q6_8.14.31))

write.csv(y2010_t, "./tourstat/y2010.csv")

remove(y2010)
remove(y2010_t1)
remove(prac)
