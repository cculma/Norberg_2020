# import multi-stage analysis and correlate data

rm(list = ls())
library(tidyverse)
library(data.table)
library(corrplot)
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
data_2st <- list.files(pattern = "2stage.csv", full.names = T)
# list_4 <- gsub(".csv", "", gsub("./", "ST2_", data_2st))
list_4 <- c("ST2_ID_2018", "ST2_ID_2019", "ST2_OR_2018", "ST2_OR_2019", "ST2_WA_2018", "ST2_WA_2019")

c1 <- list()
for (i in 1:length(data_2st)) {
  data <- read.csv(data_2st[i])
  data <- data[,c(1,2)]
  c1[[length(c1)+1]] = data
}
names(c1) <- list_4
c1 <-rbindlist(c1, use.names=TRUE, fill=TRUE, idcol="trial")
c2 <- c1 %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
c2 <- c2 %>% rownames_to_column(var = "gen")

################
# 3 stage results (three locations)

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/4_Yield/3_stage/")
data_3st <- list.files(pattern = "2stage.csv", full.names = T)
# list_4 <- gsub(".csv", "", gsub("./", "ST3_", data_3st))
list_5 <- c("ST3_ID", "ST3_OR", "ST3_WA")

d1 <- list()
for (i in 1:length(data_3st)) {
  data <- read.csv(data_3st[i])
  data <- data[,c(1,2)]
  d1[[length(d1)+1]] = data
}
names(d1) <- list_5
d1 <-rbindlist(d1, use.names=TRUE, fill=TRUE, idcol="trial")
d2 <- d1 %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
d2 <- d2 %>% rownames_to_column(var = "gen")

################

# 4 stage results (all yield)

e1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/4_Yield/4_stage/Overall_predictions_2stage.csv")
# e2 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/4_Yield/4_stage/FA_scores_mrbean.csv")

e1 <- e1 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen")
# e2 <- e2 %>% dplyr::select(1:3) %>% spread(component, score) %>% remove_rownames() %>% column_to_rownames(var = "gen")
colnames(e1) <- "ST4_Overall"
# colnames(e2) <- c("ST4_FA1", "ST4_FA2")

e1 <- e1 %>% rownames_to_column(var = "gen")
# e2 <- e2 %>% rownames_to_column(var = "gen")

################

f1 <- inner_join(a2, c2, by = "gen") %>% inner_join(., d2, by = "gen") %>% inner_join(., e1, by = "gen") %>% remove_rownames() %>% column_to_rownames(var = "gen")

colnames(f1)
f2 <- f1[,c(23:32)]
f2 <- f1[,c(1:7,23,24,29,32)]
colnames(f2)

f2 <- cor(f2, use = "complete.obs")
corrplot(f2, type="upper", method = 'number')

f3 <- cor(f1, use = "complete.obs")
corrplot(f3, type="upper", method = 'number')

f1 <- inner_join(a2, c2, by = "gen") %>% inner_join(., d2, by = "gen") %>% inner_join(., e1, by = "gen") %>% inner_join(., PCA4, by = "gen")

write.csv(f1, "~/Documents/Cesar/git/big_files/yield.csv", quote = F, row.names = F)
