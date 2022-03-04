# import multi-stage analysis and correlate data

rm(list = ls())
library(tidyverse)
library(data.table)

#################
# 1 stage results

a1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/4_Yield/1_stage/predictions_2stage_ASReml_mrbean.csv")
a2 <- a1 %>% dplyr::select(1:3) %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
list_5 <- gsub("BLUE_", "ST1_", colnames(a2))
colnames(a2) <- list_5
a2 <- a2 %>% rownames_to_column(var = "gen")

#################
# Augmented results

b1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/4_Yield_1stage.csv")
b2 <- b1 %>% dplyr::select(1:3) %>% spread(merged, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
list_6 <- gsub("BLUE_", "AUG_", colnames(b2))
colnames(b2) <- list_6
b2 <- b2 %>% rownames_to_column(var = "gen")

################
# 2 stage results (six measures)

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/4_Yield/2_stage/")
data_2st <- list.files(pattern = "mrbean.csv", full.names = T)
list_4 <- gsub(".csv", "", gsub("./", "", data_2st))

c1 <- list()
for (i in 1:length(data_2st)) {
  data <- read.csv(data_2st[i])
  data <- data[,c(1:3)]
  c1[[length(c1)+1]] = data
}
names(c1) <- list_4
c1 <-rbindlist(c1, use.names=TRUE, fill=TRUE, idcol="merged")
c2 <- c1 %>% dplyr::select(2:4) %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
list_7 <- gsub("BLUE_", "ST2_", colnames(c2))
colnames(c2) <- list_7
c2 <- c2 %>% rownames_to_column(var = "gen")

################
# 3 stage results (three locations)

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/4_Yield/3_stage/")
data_3st <- list.files(pattern = "mrbean.csv", full.names = T)
list_4 <- gsub(".csv", "", gsub("./", "", data_3st))

d1 <- list()
for (i in 1:length(data_3st)) {
  data <- read.csv(data_3st[i])
  data <- data[,c(1:3)]
  d1[[length(d1)+1]] = data
}
names(d1) <- list_4
d1 <-rbindlist(d1, use.names=TRUE, fill=TRUE, idcol="merged")
d2 <- d1 %>% dplyr::select(2:4) %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
list_8 <- gsub("BLUE_", "ST3_", colnames(d2))
colnames(d2) <- list_8
d2 <- d2 %>% rownames_to_column(var = "gen")

################

e1 <- inner_join(b2, a2, by = "gen") %>% inner_join(., c2, by = "gen") %>% inner_join(., d2, by = "gen") %>% remove_rownames() %>% column_to_rownames(var = "gen")
colnames(e1)
e2 <- e1[,c(22,44,66,88)]
e2 <- e1[,c(1,23,45,67)]

library(corrplot)
e2 <- cor(e2, use = "complete.obs")
corrplot(e2, type="upper", method = 'number')

