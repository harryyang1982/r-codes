library(tidyverse)

df=data.frame(matrix(1:27,3,9))
df

df_new <- data.frame(NULL)

for (i in seq_along(combn(27,2))/2) {
  df_new <- cbind(df_new, as.matrix(combn(27, 2)[1,i] * combn(27, 2)[2,i], nrow = 3))
}


df_new
length(combn(27,2))
seq_along(combn(27,2))

df <- tribble(~a, ~b, ~c,
              1, 4, 7,
              2, 5, 8)

df
solver <- df %>% 
  mutate(new1 = a*b,
         new2 = b*c,
         new3 = new1 * new2)

df <- data.frame(matrix(1:900, 30, 30))










k <- NULL
for (i in seq_along(combn(30, 2))) {
  k <- c(k, str_c("NEW", i))
}

df <- combn(30, 2)[1,] * combn(30, 2)[2, ]
df1 <- data.frame(matrix(1:900, 30, 30))

for (i in seq_along(combn(30, 2))) {
  df1 %>% 
    mutate_(k[i] = df1[[i]])
}
