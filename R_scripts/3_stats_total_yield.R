# obtain general statistic metrics of total yield per year 

library(metan)
library(tidyverse)

a1 <- read.csv("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/Sum_yield.csv")

a2 <- a1[1:(length(a1)-3)] %>% column_to_rownames("gen")

head(a2)
stat1 <- desc_stat(.data = a2, stats = "main", hist = F)

stat1 <- stat1 %>% mutate_at(vars(-variable), funs(round(., 2))) %>% separate(1, c("st", "loc", "year"), sep = "_", remove = T, convert = FALSE, extra = "merge")
