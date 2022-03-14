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

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA_1/1_MSC/3_stage/")

a_1_3 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA_1/1_MSC/3_stage/3_Overall_predictions_2stage.csv")
a_1_3 <- a_1_3 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
colnames(a_1_3)[2] <- "ST3_MS_WA"

a_1_4 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA_1/1_MSC/4_stage/Overall_predictions_2stage.csv")
a_1_4 <- a_1_4 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
colnames(a_1_4)[2] <- "ST4_MS"

a_2_4 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA_1/2_DM/4_stage/Overall_predictions_2stage.csv")
a_2_4 <- a_2_4 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
colnames(a_2_4)[2] <- "ST4_DM"

a_3_2 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA_1/3_Height/3_stage/2_Overall_predictions_2stage.csv")
a_3_2 <- a_3_2 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
colnames(a_3_2)[2] <- "ST3_He_OR"

a_3_3 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA_1/3_Height/3_stage/3_Overall_predictions_2stage.csv")
a_3_3 <- a_3_3 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
colnames(a_3_3)[2] <- "ST3_He_WA"

a_4_1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA_1/4_Yield/3_stage/1_Overall_predictions_2stage.csv")
a_4_1 <- a_4_1 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
colnames(a_4_1)[2] <- "ST3_Yi_ID"

a_4_2 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA_1/4_Yield/3_stage/2_Overall_predictions_2stage.csv")
a_4_2 <- a_4_2 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
colnames(a_4_2)[2] <- "ST3_Yi_OR"

a_4_3 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA_1/4_Yield/3_stage/3_Overall_predictions_2stage.csv")
a_4_3 <- a_4_3 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
colnames(a_4_3)[2] <- "ST3_Yi_WA"

##~~~~~~~~~~~~~
PCA <- read.csv("~/Documents/Cesar/git/big_files/pheno.csv")
colnames(PCA)
PCA <- PCA %>% dplyr::select(c(1,109:111))
str(PCA)
PCA$gen <- as.character(PCA$gen)

f1 <- inner_join(a_1_3, a_1_4, by = "gen") %>% inner_join(., a_2_4, by = "gen") %>% inner_join(., a_3_2, by = "gen") %>% inner_join(., a_3_3, by = "gen") %>% inner_join(., a_4_1, by = "gen") %>% inner_join(., a_4_2, by = "gen") %>% inner_join(., a_4_3, by = "gen") %>% inner_join(., PCA, by = "gen")
colnames(f1)
write.csv(f1, "~/Documents/Cesar/git/big_files/pheno_fa.csv", quote = F, row.names = F)


