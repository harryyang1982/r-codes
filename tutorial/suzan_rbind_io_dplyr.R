# Part I - basics

library(tidyverse)

data(msleep)
glimpse(msleep)

# Selecting columns

# Selecting columns basic

msleep %>% 
  select(name, genus, sleep_total, awake) %>% 
  glimpse()

msleep %>% 
  select(name:order, sleep_total:sleep_cycle) %>% 
  glimpse()

msleep %>% 
  select(-conservation, -(sleep_total:awake)) %>% 
  glimpse()

msleep %>% 
  select(-(name:awake), conservation) %>% 
  glimpse()

classification_info <- c("name", "genus", "vore", "order", "conservation")
sleep_cols <- c("sleep_total", "sleep_rem", "sleep_cycle")
weight_cols <- c("brainwt", "bodywt")

msleep %>% 
  select(one_of(sleep_cols)) %>% 
  glimpse()

## selecting columns based on partial column names

msleep %>% 
  select(name, starts_with("sleep")) %>% 
  glimpse()

msleep %>% 
  select(contains("eep"), ends_with("wt")) %>% 
  glimpse()

## selecting columns based on regular expressions

msleep %>% 
  select(matches("o.+er")) %>% 
  glimpse()

## selecting columns based on their data type

### only numeric

msleep %>% 
  select_if(is.numeric) %>% 
  glimpse()

### not numeric

msleep %>% 
  select_if(~!is.numeric(.)) %>% 
  glimpse()

## selecting columns by logical expressions

msleep %>% 
  select_if(is.numeric) %>% 
  select_if(~mean(., na.rm = TRUE) > 10)

### as same as the above

msleep %>% 
  select_if(~is.numeric(.) & mean(., na.rm = TRUE) > 10)

### distinct numbers

msleep %>% 
  select_if(~n_distinct(.) < 10)

# reordering columns

## original

msleep %>% 
  select(conservation, sleep_total, name) %>% 
  glimpse()

## columns which have been selected and everything else

msleep %>% 
  select(conservation, sleep_total, everything()) %>% 
  glimpse()

# Column names

## Renaming columns

msleep %>% 
  select(animal = name, sleep_total, extinction_threat = conservation) %>% 
  glimpse()

msleep %>% 
  rename(animal = name, extinction_threat = conservation) %>% 
  glimpse()

## Reformatting all column names

msleep %>% 
  select_all(toupper)

msleep2 <- select(msleep, name, sleep_total, brainwt)
colnames(msleep2) <- c("name", "sleep total", "brain weight")

msleep2 %>% 
  select_all(~str_replace(., " ", "_"))

## Row names to column

mtcars %>% 
  head()

mtcars %>% 
  rownames_to_column("car_model") %>% 
  glimpse()

# Part II - Transforming your columns into the right shape

## Mutating columns : The basics

msleep %>% 
  select(name, sleep_total) %>% 
  mutate(sleep_total_min = sleep_total * 60)

msleep %>% 
  select(name, sleep_total) %>% 
  mutate(sleep_total_vs_AVG = sleep_total - round(mean(sleep_total), 1),
         sleep_total_vs_MIN = sleep_total - min(sleep_total))

#alternative to using the actual arithmetics:
msleep %>%
  select(name, contains("sleep")) %>%
  rowwise() %>%
  mutate(avg = mean(c(sleep_rem, sleep_cycle)))

msleep %>%
  select(name, brainwt) %>%
  mutate(brainwt2 = ifelse(brainwt > 4, NA, brainwt)) %>%
  arrange(desc(brainwt))

msleep %>%
  select(name) %>%
  mutate(name_last_word = tolower(str_extract(name, pattern = "\\w+$")))

msleep %>%
  mutate_all(tolower)

## make messy data

msleep_ohno <- msleep %>%
  mutate_all(~paste(., "  /n  "))
msleep_ohno

msleep_ohno[,1:4]

## making them tidy

msleep_corr <- msleep_ohno %>%
  mutate_all(~str_replace_all(., "/n", "")) %>%
  mutate_all(str_trim)
msleep_corr
