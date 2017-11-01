library(tidyverse)
library(tidytext)
library(rvest)
library(lubridate)
library(knitr)
library(stringr)
library(ggridges)
library(wordcloud)
library(wesanderson)

feed_items <- read_xml("http://twiv.microbeworld.libsynpro.com/twiv") %>% 
  xml_nodes("item")

feed_items %>% 
  xml_nodes("pubDate") %>% 
  xml_text()

feed_items %>% 
  xml_nodes("itunes\\:duration") %>% 
  xml_text()

feed_to_df <- function(rss) {
  feed_items <- read_xml(rss) %>% 
    xml_nodes("item")
  
  feed_df <- data_frame(pubDate = feed_items %>% xml_nodes("pubDate") %>% xml_text(),
                        title = feed_items %>% xml_nodes("title") %>% xml_text(),
                        encLength = feed_items %>% xml_nodes("enclosure") %>% xml_attr("length"),
                        duration = feed_items %>% xml_nodes("itunes\\:duration") %>% xml_text()) %>% 
    mutate(pubDate = dmy_hms(pubDate)) %>% 
    mutate(encLength = as.numeric(encLength),
           duration = ifelse(grepl(":\\d+:", duration), duration, paste0("00:", duration)),
           duration_seconds = as.numeric(hms(duration)))
  
  feed_df
}

jdny_df <- feed_to_df("http://api.podty.me/api/v1/share/cast/390937d3e5c758aa6f4005b63542cc83695b4d5e6925fe6a2d4d488d1d05d748/146364")

jdny_df %>% 
  top_n(5, wt = pubDate) %>% 
  kable()

jdny_df %>% 
  mutate(duration_minutes = duration_seconds / 60) %>% 
  ggplot(aes(pubDate, duration_minutes)) +
  geom_point() +
  geom_smooth() +
  labs(x = "날짜", y = "방송시간 (분)",
       title = "지적 대화를 위한 넓고 얕은 지식") +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")

jdny_df %>% 
  mutate(duration_minutes = duration_seconds / 60,
         Year = year(pubDate)) %>% 
  group_by(Year) %>% 
  summarise(mean_duration = mean(duration_minutes, na.rm = TRUE)) %>% 
  ggplot(aes(Year, mean_duration)) +
  geom_col(fill = wes_palette("Royal1")) +
  labs(x = "연도", y = "평균 방송시간 (분)",
       title = "지적 대화를 위한 넓고 얕은 지식",
       subtitle = "평균 방송시간") +
  scale_x_continuous(breaks = 2014:2017)

jdny_df %>% 
  mutate(duration_minutes = duration_seconds / 60,
         Year = factor(year(pubDate), levels = 2017:2014)) %>% 
  filter(!is.na(duration_minutes)) %>% 
  ggplot(aes(x = duration_minutes, y = Year, fill = Year)) +
  geom_density_ridges(scale = 4) +
  scale_fill_cyclical(values = wes_palette("Moonrise1")) +
  labs(x = "방송시간 (분)", y = "연도",
       title = "지적 대화를 위한 넓고 얕은 지식",
       subtitle = "연도별 방송시간 분포") +
  scale_x_continuous(breaks = seq(0, 120, 30), limits = c(0, 120))

grs_df <- feed_to_df("http://www.xsfm.co.kr/xml/idwk.xml") # 그알싫 RSS
ms_df <- feed_to_df("http://www.phnara.com/gtv7/feed.xml") # 망치부인 RSS

jdny_df %>% 
  mutate(show = "지대넓얕") %>% 
  bind_rows(mutate(grs_df, show = "그알싫")) %>% 
  mutate(duration_minutes = duration_seconds / 60,
         Year = factor(year(pubDate), levels = 2017:2014)) %>% 
  filter(!is.na(duration_minutes)) %>% 
  ggplot(aes(x = duration_minutes, y = Year)) +
  geom_density_ridges(aes(fill = show), alpha = 0.4) +
  theme(legend.title = element_blank()) +
  labs(x = "방송시간 (분)", y = "연도",
       title = "연도별 방송시간 분포") +
  scale_x_continuous(breaks = seq(0, 300, 30)) +
  scale_fill_manual(values = c("blue", "red"))

jdny_df %>% 
  mutate(show = "지대넓얕") %>% 
  bind_rows(mutate(grs_df, show = "그알싫")) %>% 
  mutate(duration_minutes = duration_seconds / 60,
         Year = factor(year(pubDate), levels = 2017:2014),
         show = factor(show, levels = c("지대넓얕", "그알싫"))) %>% 
  filter(!is.na(duration_minutes)) %>% 
  ggplot(aes(x = duration_minutes, y = Year, fill = show)) +
  geom_density_ridges(scale = 4, alpha = 0.4) +
  theme(legend.title = element_blank()) +
  labs(x = "방송시간 (분)", y = "연도",
       title = "연도별 방송시간 분포") +
  scale_x_continuous(breaks = seq(0, 180, 60), limits = c(0, 180)) +
  facet_wrap(~ show)

jdny_df %>% 
  mutate(show = "지대넓얕") %>% 
  bind_rows(mutate(grs_df, show = "그알싫")) %>% 
  bind_rows(mutate(ms_df, show = "망치부인")) %>% 
  mutate(duration_minutes = duration_seconds / 60,
         Year = factor(year(pubDate), levels = 2017:2014),
         show = factor(show, levels = c("지대넓얕", "그알싫", "망치부인"))) %>% 
  filter(!is.na(duration_minutes)) %>% 
  ggplot(aes(x = duration_minutes, y = show, fill = show)) +
  geom_density_ridges(scale = 4) +
  theme(legend.title = element_blank()) +
  labs(x = "방송시간 (분)", y = "방송",
       title = "방송별 방송시간 분포") +
  scale_x_continuous(breaks = seq(0, 300, 60)) +
  scale_fill_manual(values = wes_palette("GrandBudapest2", 3))

data_frame(keywords = read_xml("http://api.podty.me/api/v1/share/cast/390937d3e5c758aa6f4005b63542cc83695b4d5e6925fe6a2d4d488d1d05d748/146364") %>% 
             xml_nodes("item itunes\\:keywords") %>% 
             xml_text()) %>% 
  unnest_tokens(words, keywords) %>% 
  count(words) %>% 
  filter(!words %in% c("지적", "지적대화", "대화를"
                       , "위한", "넓고", "얕은", "지식", "지대넓얕", "jdny", "팟티", "채사장", "김도인", "깡선생", "독실이", "깡쌤", "이독실", "덕실이", "이덕실", "2")) %>% 
  with(wordcloud(words, n, scale = c(5, .3), min.freq = 8, random.order = FALSE, colors = wes_palette("Royal1"), family = "Apple SD Gothic Neo"))

