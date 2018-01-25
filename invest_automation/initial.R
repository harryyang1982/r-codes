install.packages("googlesheets")
library(googlesheets)
library(tidyverse)

gs_auth(new_user = T)
gs_ls()

invest <- gs_title("investment")
invest_sheet <- gs_read(invest)

invest_sheet <- invest_sheet %>% 
  mutate(일시 = parse_datetime(일시, "%Y.%m.%d %H:%M"), 
    총액 = 매매가격 * 매매수량)

invest_sheet

# import stock data from open big data

library(jsonlite)

apibase <- "http://datastore.or.kr:5000/api/action/datastore_search_sql?sql="
query <- URLencode("SELECT * from \"3080255f-f7d8-43fd-ac1f-2b80e9341ff8\" limit 5")
document <- fromJSON(paste0(apibase, query))
fields <- document$result$fields
data <- document$result$records

summary(data)
head(data)
