library(tidyverse)
library(readxl)
library(data.table)

y2005to2006 <- fread("./tourstat/2005to2006.csv")

y2005_t <- y2005to2006 %>% 
  gather(generation, value, under_10:over_70)

glimpse(y2005_t)

y2005_t %>% 
  ggplot(aes(x=generation, y=value)) +
  geom_col() +
  scale_x_discrete(limits=c("under_10", "g10", "g20", "g30", "g40", "g50", "g60", "over_70"))

names(y2005_t) <- c("country", "sex", "region", "generation", "value")

glimpse(y2005_t)
y2005_t$country <- factor(y2005_t$country)
y2005_t$sex <- factor(y2005_t$sex)
y2005_t$region <- factor(y2005_t$region)
y2005_t$generation <- factor(y2005_t$generation)

y2005_t <- y2005_t %>% 
  select(generation, sex, country, region, value)

gene_2016 <- read_excel("./tourstat/y2016_xlsx.xlsx")

gene_2016 <- gene_2016 %>% 
  select(generation, sex, country, region, sido, value)

region_pop <- read_excel("./tourstat/region_pop_index.xls")

nation_pop <- read_excel("./tourstat/국가별대륙일람표.xlsx")

nation_pop <- nation_pop %>% 
  rename(country=`국가`) %>% 
  rename(region=`대륙`) %>% 
  select(region, country)

nation_pop <- nation_pop %>% 
  rename(q6_1=country)

gene_2016$q6_1 <- as.character(gene_2016$q6_1)

gene_2016 <- gene_2016 %>% 
  left_join(nation_pop, by="q6_1")

gene_2016 <- gene_2016 %>% 
  rename(generation=age_cut)

gene_2016 <- gene_2016 %>% 
  rename(value = nn)

#by_gender


y2005_t %>% 
  ggplot(aes(generation, value, fill=sex)) +
  geom_col() +
  scale_x_discrete(limits=c("under_10","g10", "g20", "g30", "g40", "g50", "g60", "over_70")) +
  ggtitle("2005~2006 by generation")

gene_2016 %>% 
  ggplot(aes(generation, value, fill=sex)) +
  geom_col() +
  scale_x_discrete(limits=c("under_10","g10", "g20", "g30", "g40", "g50", "g60", "over_70")) +
  ggtitle("2017 by generation")

gene_2016 %>% 
  filter(generation %in% c("g30","g20") & region == "유럽") %>% 
  ggplot(aes(sido, value, fill=sex)) +
  geom_col()

# europe by generation

y2005_t %>% 
  filter(region %in% c("북미", "유럽")) %>% 
  ggplot(aes(generation, value, fill=sex)) +
  geom_col() +
  scale_x_discrete(limits=c("under_10","g10", "g20", "g30", "g40", "g50", "g60", "over_70")) +
  ggtitle("2005~2006 by generation")

u40_2005 <- y2005_t %>%
  filter(generation == c("under_10","g10", "g20", "g30")) %>% 
  summarise(under_40 = sum(value))

o40_2005 <- y2005_t %>%
  filter(generation == c("g40", "g50", "g60", "over_70")) %>% 
  summarise(over_40 = sum(value))

(u40_2005 / (o40_2005 + u40_2005))

u40_2016 <- gene_2016 %>% 
  filter(generation %in% c("under_10","g10", "g20", "g30")) %>% 
  summarise(under_40 = sum(value))

u40_2016 <- sum(u40_2016$value)

o40_2016 <- gene_2016 %>% 
  filter(generation %in% c("g40", "g50", "g60", "over_70")) %>% 
  summarise(over_40 = sum(value))

o40_2016 <- sum(o40_2016$value)

(u40_2016 / (o40_2016 + u40_2016))

gene_2016 %>% 
  filter(region %in% c("북미", "유럽")) %>% 
  ggplot(aes(generation, value, fill=sex)) +
  geom_col() +
  scale_x_discrete(limits=c("under_10","g10", "g20", "g30", "g40", "g50", "g60", "over_70")) +
  ggtitle("2017 by generation")


# by region in twenties & thirties

y2005_t %>% 
  filter(generation == "g20") %>% 
  ggplot(aes(region, value, fill=sex)) +
  geom_col()

y2005_t %>% 
  filter(generation == "g30") %>% 
  ggplot(aes(region, value, fill=sex)) +
  geom_col()

gene_2016 %>% 
  filter(generation == "g20") %>% 
  ggplot(aes(region, value, fill=sex)) +
  geom_col()

gene_2016 %>% 
  filter(generation == "g30") %>% 
  group_by(region, sex) %>% 
  count()

gene_2016 %>% 
  filter(generation == "g30") %>% 
  ggplot(aes(region, value, fill=sex)) +
  geom_col()


#twenties
y2005_t %>% 
  filter(generation=="g20" | generation=="g30") %>% 
  ggplot(aes(generation, value, fill=sex)) +
  geom_col(position="dodge")

gene_2016 %>% 
  filter(generation=="g20" | generation=="g30") %>% 
  ggplot(aes(generation, value, fill=sex)) +
  geom_col()

y2005_t %>% 
  filter(generation == "g30" & region %in% c("아시아", "유럽")) %>% 
  ggplot(aes(region, value, fill=sex)) +
  geom_col()

gene_2016 %>% 
  filter(generation == "g30" & region %in% c("아시아", "유럽")) %>% 
  ggplot(aes(region, value, fill=sex)) +
  geom_col()


y2005_t %>% 
  filter(generation=="g20") %>% 
  ggplot(aes(region, value, fill=sex)) +
  geom_col()

y2005_t %>% 
  filter(generation=="g20" & region %in% c("아시아")) %>% 
  ggplot(aes(country, value, fill=sex)) +
  geom_col()

gene_2016 %>% 
  filter(generation=="g20" & region %in% c("아시아")) %>% 
  ggplot(aes(country, value, fill=sex)) +
  geom_col()


y2005_t %>% 
  filter(generation=="g20" & region %in% c("유럽") & value > 100) %>% 
  ggplot(aes(country, value, fill=sex)) +
  geom_col()

gene_2016 %>% 
  filter(generation=="g20" & region %in% c("유럽")) %>% 
  ggplot(aes(country, value, fill=sex)) +
  geom_col()

gene_2016 %>% 
  filter(generation=="g20" & region %in% c("아시아", "유럽")) %>% 
  ggplot(aes(country, value, fill=sex)) +
  geom_col()

# by region in twenties

y2005_t %>% 
  filter(generation=="g20" & region %in% c("유럽", "아시아")) %>% 
  ggplot(aes(region, value, fill=sex)) +
  geom_col()

gene_2016 %>% 
  filter(generation=="g20" & region %in% c("유럽", "아시아")) %>% 
  ggplot(aes(region, value, fill=sex)) +
  geom_col()

# by countries in asia in twenties

y2005_t %>% 
  filter(generation=="g20" & region %in% c("아시아") & value > 5000) %>% 
  ggplot(aes(country, value, fill=sex)) +
  geom_col()

gene_2016 %>% 
  filter(generation=="g20" & region %in% c("아시아")) %>% 
  ggplot(aes(country, value, fill=sex)) +
  geom_col()

# by countries in asia in thirties

y2005_t %>% 
  filter(generation=="g30" & region %in% c("아시아")) %>% 
  ggplot(aes(country, value, fill=sex)) +
  geom_col()

gene_2016 %>% 
  filter(generation=="g30" & region %in% c("아시아")) %>% 
  ggplot(aes(country, value, fill=sex)) +
  geom_col()

# by countries in europe in twenties

y2005_t %>% 
  filter(generation=="g20" & region %in% c("유럽") & value > 10) %>% 
  ggplot(aes(country, value, fill=sex)) +
  geom_col()

gene_2016 %>% 
  filter(generation=="g20" & region %in% c("유럽")) %>% 
  ggplot(aes(country, value, fill=sex)) +
  geom_col()

# by countries in europe in thirties

y2005_t %>% 
  filter(generation=="g30" & region %in% c("유럽")) %>% 
  ggplot(aes(country, value, fill=sex)) +
  geom_col()

gene_2016 %>% 
  filter(generation=="g30" & region %in% c("유럽")) %>% 
  ggplot(aes(country, value, fill=sex)) +
  geom_col()
