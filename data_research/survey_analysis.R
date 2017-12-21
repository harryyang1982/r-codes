library(tidyverse)

raw_data <- read_excel("data_research/raw_data.xlsx")

survey <- raw_data

survey <- as.tibble(map(survey, factor))
str(survey)

# factorize variables

survey_sum <- survey %>% 
  mutate(연령 = fct_recode(연령, 
                         "18세미만" = "1",
                         "20대" = "2",
                         "30대" = "3",
                         "40대" = "4",
                         "50대" = "5",
                         "60대 이상" = "6"),
           성별 = fct_recode(성별,
                         "남성" = "1",
                         "여성" = "2")) %>% 
  mutate(지역 = fct_recode(지역,
                           "서울" = "1",
                           "경기인천" = "2",
                           "대전충청세종" = "3",
                           "부산경남울산" = "4",
                           "대구경북" = "5",
                           "광주전라" = "6",
                           "강원제주" = "7")) %>% 
  mutate(q1 = fct_recode(q1,
                         "매우 긍정적이다" = "1",
                         "긍정적으로 보지만 개선해야 한다" = "2",
                         "예전과 차이를 모르겠다" = "3",
                         "부정적으로 보며 제도가 많이 바뀌어야 한다" = "4",
                         "시행한 이유를 모르겠다" = "5",
                         "잘 모르겠음" = "6")) %>% 
  mutate(q2 = fct_recode(q2,
                         "지방자치 강화를 위해 매우 긍정적" = "1",
                         "대통령제를 유지한다는 전제 하에 찬성" = "2",
                         "내각책임제로 전환한다는 전제 하에 찬성" = "3",
                         "개헌에 찬성하나 지방자치 강화를 원하지는 않음" = "4",
                         "개헌을 반대함" = "5",
                         "잘 모르겠음" = "6")) %>% 
  mutate(q3 = fct_recode(q3,
                         "선출직 공무원이 늘어나면서 민주주의 강화" = "1",
                         "중앙정치에서 비교적 자유롭게 지역민심과 민원을 대변하는 계기가 됨" = "2",
                         "신예 정치인을 육성하고 평가하는 기준이 됨" = "3",
                         "잘 모르겠음" = "4")) %>% 
  mutate(q4 = fct_recode(q4,
                         "자치단체가 충분한 세수를 확보하지 못함" = "1",
                         "중앙정당이 지방 정치인들을 줄세우기 하는 폐해" = "2",
                         "자치단체 출마 정치인들의 역량 부족" = "3",
                         "지역주의 등 유권자들의 공익적이지 않은 투표 행태" = "4",
                         "잘 모르겠음" = "5")) %>% 
  mutate(q5 = fct_recode(q5,
                         "지방선거 모든 분야에서 정당 공천을 하는 것이 바람직함" = "1",
                         "광역자치단체장 및 의원 선거에선 정당 공천을 하되, 기초자치단체장 및 의원 선거에선 공천 않고 정당 표기 하지 않는 선거가 바람직함" = "2",
                         "지방선거 모든 분야에서 정당 공천과 정당 표기 없이 선거를 하는 것이 바람직함" = "3",
                         "잘 모르겠음" = "4")) %>% 
  mutate(q6 = fct_recode(q6,
                         "광역의원 및 기초의원 모두 비례대표 비중을 늘리는 것이 바람직함" = "1",
                         "광역의원에 대해서만 비례대표 비중을 늘리는 것이 바람직함" = "2",
                         "지역의원에 대해서만 비례대표 비중을 늘리는 것이 바람직함" = "3",
                         "지방의회에서 정당 비례대표제를 강화하는 것이 찬성하지 않음" = "4",
                         "잘 모르겠음" = "5")) %>% 
  mutate(q7 = fct_recode(q7,
                         "교육감 직선제 유지하되, 교육감 권한 강화" = "1",
                         "교육감 직선제 유지하되, 광역자치단체장과 함께 선출하는 러닝메이트제로 전환" = "2",
                         "교육감 직선제 유지하되, 교육감이 광역자치단체장과 충돌하지 않도록 권한 축소 조정" = "3",
                         "교육감 직선제는 폐지하는 것이 바람직함" = "4",
                         "잘 모르겠음" = "5")) %>% 
  mutate(q8 = fct_recode(q8,
                         "직선 교육감에 대한 시민 통제 위해 교육의원 직선제 부활이 바람직함" = "1",
                         "지방의원들로 교육위원회 구성하는 현 체제가 바람직함" = "2",
                         "잘 모르겠음" = "3"))

# Make list for iterating variables to draw graphs and make tables

map_ggplot <- list(filler = rep(paste0("q", 1:8), each= 3),
     xer = rep(c("연령", "성별", "지역"), 8),
     titles = rep(c("1995년부터 실시된 민선 지방자치에 대해 어떻게 생각하십니까?", 
                "지방자치단체 권한을 강화하는 소위 분권형 개헌 방안에 대해 어떻게 생각하십니까?",
                "지방자치가 긍정적인 효과를 만들었다면, 그 이유는 뭐라 생각하십니까?",
                "지방자치가 부정적인 효과를 만들었다면다면, 그 이유는 뭐라 생각하십니까?",
                "지방선거에서 정당 공천을 하는 문제에 대해 어떻게 생각하십니까?",
                "지방의회에서 정당 비례대표제를 강화해야 한단 주장에 대해선 어떻게 생각하십니까?",
                "교육감 직선제를 개선해야 한다는 문제에 대해 어떻게 생각하십니까?",
                "2010년 직선제로 선출되었다가 2014년 지방의원들로 대체된 교육의원 문제에 대해선 어떻게 생각하십니까?"), each = 3),
     subtitles = rep(c("연령별 비율(%)", "성별 비율(%)", "지역별 비율(%)"), 8))

mapp <- transpose(map_ggplot)

# Generating graphs using for loops

plots <- vector("list")
for (i in seq_along(mapp)) {
  plots[[i]] <- ggplot(data=survey_sum, aes_string(x=mapp[[i]][["xer"]], fill=mapp[[i]][["filler"]])) + 
    geom_bar(position = "fill", color = "black") +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous(labels = scales::percent, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
    labs(title = mapp[[i]][["titles"]], subtitle = mapp[[i]][["subtitles"]], y = "비율") +
    coord_flip() +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
}

plots

# Generating tables and writing csv, supporting Excel

tables <- vector("list")
for (i in seq_along(mapp)) {
  tables[[i]] <- survey_sum %>% 
    group_by_(mapp[[i]][["xer"]], mapp[[i]][["filler"]]) %>% 
    summarise(n=n()) %>% 
    mutate(tot_group = sum(n)) %>% 
    mutate(pct = round(n/tot_group*100, 1))
  write_excel_csv(tables[[i]], str_c("tables", i, ".csv", sep = ""))
}


