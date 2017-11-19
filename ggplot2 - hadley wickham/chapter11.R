library(tidyverse)

#11.2 Removing Trend

diamonds2 <- diamonds %>% 
  filter(carat <= 2) %>% 
  mutate(
    lcarat = log2(carat),
    lprice = log2(price)
  )
  
diamonds2

diamonds2 %>% 
  ggplot(aes(lcarat, lprice)) +
  geom_bin2d() +
  geom_smooth(method = "lm", se = F, size = 2, color = "yellow")

mod <- lm(lprice ~ lcarat, data = diamonds2)
coef(summary(mod))

diamonds2 <- diamonds2 %>%
  mutate(rel_price = resid(mod))

diamonds2 %>% 
  ggplot(aes(carat, rel_price)) +
  geom_bin2d()

xgrid <- seq(-2, 1, by = 1/3)
tibble(logx = xgrid, x = round(2 ^ xgrid, 2))

color_cut <- diamonds2 %>% 
  group_by(color, cut) %>% 
  summarise(
    price = mean(price), rel_price = mean(rel_price)
  )

color_cut %>% 
  ggplot(aes(color, price)) +
  geom_line(aes(group=cut), color="grey80") +
  geom_point(aes(color=cut))

color_cut %>% 
  ggplot(aes(color, rel_price)) +
  geom_line(aes(group=cut), color="grey80") +
  geom_point(aes(color=cut))

# 11.2.1 Exercises

#1
diamonds3 <- diamonds %>% 
  mutate(lcarat = log2(carat),
         lprice = log2(price))

mod <- lm(lprice ~ lcarat, data= diamonds3)
diamonds3 <- diamonds3 %>% 
  mutate(rel_price = resid(mod))

diamonds3 %>% 
  ggplot(aes(carat, rel_price)) +
  geom_bin2d()

color_cut2 <- diamonds3 %>% 
  group_by(color, cut) %>% 
  summarise(
    price=mean(price), rel_price=mean(rel_price)
  )

color_cut2 %>% 
  ggplot(aes(color, price)) +
  geom_line(aes(group=cut), color = "grey80") +
  geom_point(aes(color=cut))

color_cut2 %>% 
  ggplot(aes(color, rel_price)) +
  geom_line(aes(group=cut), color = "grey80") +
  geom_point(aes(color=cut))

# 2

diamonds3 %>% 
  group_by(cut) %>% 
  summarise(mean_size = mean(carat)) %>% 
  ggplot(aes(cut, mean_size)) +
  geom_line(aes(group = 1), color = "grey80") +
  geom_point(aes(color=cut))

# 3

diamonds3 %>% 
  group_by(cut, color) %>% 
  summarise(carat = mean(carat), rel_price = mean(rel_price)) %>% 
  ggplot(aes(carat, rel_price)) +
  geom_line(aes(group=1), color="grey80") +
  geom_point(aes(color=cut)) +
  facet_wrap(~color)

# 4
diamonds3 %>% 
  group_by(depth) %>% 
  ggplot(aes(depth, rel_price)) +
  geom_bin2d()

diamonds3 %>% 
  group_by(table) %>% 
  summarise(rel_price = mean(rel_price), depth = mean(depth)) %>% 
  ggplot(aes(table, rel_price)) +
  geom_point(aes(color=depth))

# 11.3 Texas Housing Data

txhousing

ggplot(txhousing, aes(date, sales)) +
  geom_line(aes(group=city), alpha = 1/2)

txhousing %>% 
  ggplot(aes(date, log(sales))) +
  geom_line(aes(group = city), alpha = 1/2)

abilene <- txhousing %>% 
  filter(city == "Abilene")

abilene %>% 
  ggplot(aes(date, log(sales))) +
  geom_line()

mod <- lm(log(sales) ~ factor(month), data= abilene)

abilene <- abilene %>% 
  mutate(rel_sales = resid(mod))

abilene %>% 
  ggplot(aes(date, rel_sales)) +
  geom_line()

deseas <- function(x, month) {
  resid(lm(x ~ factor(month), na.action = na.exclude))
}

txhousing <- txhousing %>% 
  group_by(city) %>% 
  mutate(rel_sales = deseas(log(sales), month))

txhousing %>% 
  ggplot(aes(date, rel_sales)) +
  geom_line(aes(group = city), alpha = 1/5) +
  geom_line(stat = "summary", fun.y = "mean", color = "red")

txhousing %>% 
  ggplot(aes(date, rel_sales)) +
  geom_line(aes(group = city), alpha = 1/5) +
  geom_line(stat = "summary", fun.y = "mean", color = "red") +
  xlim(2008, 2012)

# 11.4

models <- txhousing %>% 
  group_by(city) %>% 
  do(mod = lm(
    log2(sales) ~ factor(month),
    data = .,
    na.action = na.exclude
  ))
models

library(broom)

# 11.5 Model-Level Summaries

model_sum <- models %>% glance(mod)

ggplot(model_sum, aes(r.squared, reorder(city, r.squared))) +
         geom_point()

top3 <- c("Bryan-College Station", "Lubbock", "NE Tarrant County")
bottom3 <- c("McAllen", "Brownsville", "Harlingen")
extreme <- txhousing %>%  ungroup() %>% 
  filter(city %in% c(top3, bottom3), !is.na(sales)) %>% 
  mutate(city = factor(city, c(top3, bottom3)))

# 11.5.1 Exercises

ggplot(extreme, aes(month, log(sales))) +
  geom_line(aes(group = year)) +
  facet_wrap(~city)

# 3

models2 <- txhousing %>% 
  group_by(city) %>% 
  do(mod = lm(
    log(sales) ~ factor(month) + year,
    data = .,
    na.action = na.exclude
  ))

model_sum2 <- models2 %>% glance(mod)
ggplot(model_sum2, aes(r.squared, reorder(city, r.squared))) +
  geom_point()

extreme <- txhousing %>%  ungroup() %>% 
  filter(city %in% c(top3, bottom3), !is.na(sales)) %>% 
  mutate(city = factor(city, c(top3, bottom3)))

ggplot(txhousing, aes(month, log(sales))) +
  geom_line(aes(group = year)) +
  facet_wrap(~city)

# 11.6. Coefficient Model Summaries

library(broom)

coefs <- models %>% 
  tidy(mod)
coefs

months <- coefs %>% 
  filter(grepl("factor", term)) %>% 
  extract(term, "month", "(\\d+)", convert=T)
months

months %>% 
  ggplot(aes(month, 2 ^ estimate)) +
  geom_line(aes(group=city))

coef_sum <- months %>% 
  group_by(city) %>% 
  summarise(max = max(estimate))

coef_sum %>% 
  ggplot(aes(2 ^ max, reorder(city, max))) +
  geom_point()

##11.6.1 Exercises

coef_sum_min <- coef_sum %>% 
  arrange(max^2) %>% 
  head(3)

coef_sum_max <- coef_sum %>% 
  arrange(desc(max^2)) %>% 
  head(3)

coef_sum_tot <- rbind(coef_sum_max, coef_sum_min)
coef_sum_tot

coef_sum_tot %>% 
  ggplot(aes(2^max, reorder(city, max))) +
  geom_point()

# 11.7 Observation Data

obs_sum <- models %>% 
  augment(mod)

obs_sum

obs_sum %>% 
  ggplot(aes(.std.resid)) +
  geom_histogram(binwidth = 0.1)

obs_sum %>% 
  ggplot(aes((abs(.std.resid)))) +
  geom_histogram(binwidth = 0.1)

obs_sum %>% 
  filter(abs(.std.resid) > 2) %>% 
  group_by(city) %>% 
  summarise(n = n(), avg = mean(abs(.std.resid))) %>% 
  arrange(desc(n))

models

obs_sum2 <- models2 %>% 
  augment(mod)
obs_sum2

obs_sum2 %>% 
  ggplot(aes((abs(.std.resid)))) +
  geom_histogram(binwidth = 0.1)

obs_sum2 %>% 
  filter(abs(.std.resid) > 2) %>% 
  group_by(city) %>% 
  summarise(n = n(), avg = mean(abs(.std.resid))) %>% 
  arrange(desc(n))
