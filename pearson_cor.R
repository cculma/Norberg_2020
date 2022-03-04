# import multi-stage analysis and correlate data

rm(list = ls())
library(tidyverse)
library(data.table)

#################
# 1 stage

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/4_Yield/1_stage/")
data_Yield <- list.files(pattern = ".csv", full.names = T)
list_4 <- gsub(".csv", "", gsub("./", "", data_Yield))

Yield_1 <- list()
for (i in 1:length(data_Yield)) {
  data <- read.csv(data_Yield[i])
  data <- data[,c(1:3)]
  Yield_1[[length(Yield_1)+1]] = data
}

a1 <- read.csv("predictions_2stage_ASReml_mrbean.csv")
a1 <- a1[,c(1:3)]
spread()

names(Yield_1) <- list_4
Yield_2 <-rbindlist(Yield_1, use.names=TRUE, fill=TRUE, idcol="merged")
write.csv(Yield_2, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/Yield_1stage.csv", row.names = F, quote = F)
