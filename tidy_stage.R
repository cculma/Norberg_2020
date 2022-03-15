# import multi-stage analysis and correlate data

rm(list = ls())
library(tidyverse)
library(data.table)
library(corrplot)
#################
# 1 stage results
# MS = 1_MSC
# DM = 2_DM
# He = 3_Height
# Yi = 4_Yield
# FD = 5_FD

###################
# data FA

rm(data_3st)
setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/7_FA_scores/4_stage/")
data_FA4 <- list.files(pattern = ".csv", full.names = T)
list_5 <- c("FA_MS", "FA_DM", "FA_He", "FA_Yi", "FA_FD")
FA_4 <- list()
for (i in 1:length(data_FA4)) {
  data <- read.csv(data_FA4[i])
  data <- data[,c(3,1)]
  FA_4[[length(FA_4)+1]] = data
}
names(FA_4) <- list_5
FA_4 <-rbindlist(FA_4, use.names=TRUE, fill=TRUE, idcol="trial")
FA_4 <- FA_4 %>% spread(trial, score) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")

f2 <- inner_join(FA_4, PCA, by = "gen")
write.csv(f2, "~/Documents/Cesar/git/big_files/pheno_fa.csv", quote = F, row.names = F)
####################
PCA <- PCA %>% dplyr::select(c(109:111)) %>% remove_rownames() %>% column_to_rownames(var = "gen")
colnames(f1)
f2 <- f1[,c(92:107)]
f2 <- cor(f2, use = "complete.obs")
corrplot(f2, type="upper", method = 'number')

####################
# 3 stage results (three locations)


fa1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE3_ar/ST1_He_FA1.csv")
fa1 <- fa1 %>% dplyr::select(1:3) %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
list_5 <- gsub("^", "FA1_He_", colnames(fa1))
colnames(fa1) <- list_5
fa1 <- fa1 %>% rownames_to_column(var = "gen")


fa3 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE3_ar/ST1_He_FA3.csv")
fa3 <- fa3 %>% dplyr::select(1:3) %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
list_5 <- gsub("^", "FA3_He_", colnames(fa3))
colnames(fa3) <- list_5
fa3 <- fa3 %>% rownames_to_column(var = "gen")

a_1_4 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE3_ar/ST3_OR_He.csv")
a_1_4 <- a_1_4 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
colnames(a_1_4)[2] <- "ST3_He_OR"

a_1_5 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE3_ar/ST3_OR_He1.csv")
a_1_5 <- a_1_5 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
colnames(a_1_5)[2] <- "ST3_He_OR1"

fa1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE3_ar/3_Height_1stage.csv")
fa1 <- fa1 %>% dplyr::select(1:3) %>% spread(merged, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
list_5 <- gsub("^", "BLUE_He_", colnames(fa1))
colnames(fa1) <- list_5
fa1 <- fa1 %>% rownames_to_column(var = "gen")
f1 <- inner_join(fa1, PCA, by = "gen")

##~~~~~~~~~~~~~
PCA <- read.csv("~/Documents/Cesar/git/big_files/pheno.csv")
colnames(PCA)
PCA <- PCA %>% dplyr::select(c(1,109:111))
str(PCA)
PCA$gen <- as.character(PCA$gen)

f1 <- inner_join(fa1, fa3, by = "gen") %>% inner_join(., a_1_4, by = "gen") %>% inner_join(., a_1_5, by = "gen") %>% inner_join(., PCA, by = "gen")
colnames(f1)
write.csv(f1, "~/Documents/Cesar/git/big_files/pheno_fa.csv", quote = F, row.names = F)


