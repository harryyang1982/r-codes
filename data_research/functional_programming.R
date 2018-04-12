# Make list for iterating variables to draw graphs and make tables

map_ggplot <- list(filler = rep(paste0("q", 1:8), each= 3 ),
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

# Generating graphs without for loops

plots2<-mapp %>% map(~ggplot(data=survey_sum,aes_string(x=.$xer, fill=.$filler)) +
                       geom_bar(position = "fill", color = "black")+
                       scale_fill_brewer(palette = "Set3") +
                       scale_y_continuous(labels = scales::percent, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
                       labs(title = mapp[[i]][["titles"]], subtitle = mapp[[i]][["subtitles"]], y = "비율") +
                       coord_flip() +
                       guides(fill = guide_legend(ncol=2)) +
                       theme_gray(base_family = "AppleGothic")+
                       theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)

plots2

# Generating tables and writing csv, supporting Excel without for loop

tables2<-mapp %>% map(~group_by_(survey_sum,.$xer,.$filler) %>%
                        summarise(n=n()) %>% 
                        mutate(tot_group = sum(n)) %>% 
                        mutate(pct = round(n/tot_group*100, 1)))

paths<-str_c("tables", 1:length(tables2), ".csv", sep = "")

pwalk(list(tables2,paths),write_excel_csv)