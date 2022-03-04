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

# Augmented results

b1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/4_Yield_1stage.csv")
b2 <- b1 %>% dplyr::select(1:3) %>% spread(merged, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
list_6 <- gsub("BLUE_", "AUG_", colnames(b2))
colnames(b2) <- list_6

# 2 stage results (six measures)

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/4_Yield/2_stage/")
data_2st <- list.files(pattern = "mrbean.csv", full.names = T)
list_4 <- gsub(".csv", "", gsub("./", "", data_2st))

Yield_2 <- list()
for (i in 1:length(data_2st)) {
  data <- read.csv(data_2st[i])
  data <- data[,c(1:3)]
  Yield_2[[length(Yield_2)+1]] = data
}
