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

#################
# start
a1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/1_MSC/1_stage/predictions_2stage_ASReml_mrbean.csv")
a1 <- a1 %>% dplyr::select(1:3) %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
list_5 <- gsub("BLUE_", "ST1_MS_", colnames(a1))
colnames(a1) <- list_5
a1 <- a1 %>% rownames_to_column(var = "gen")

a2 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/2_DM/1_stage/predictions_2stage_ASReml_mrbean.csv")
a2 <- a2 %>% dplyr::select(1:3) %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
list_5 <- gsub("BLUE_", "ST1_DM_", colnames(a2))
colnames(a2) <- list_5
a2 <- a2 %>% rownames_to_column(var = "gen")

a3 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/3_Height/1_stage/predictions_2stage_ASReml_mrbean.csv")
a3 <- a3 %>% dplyr::select(1:3) %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
list_5 <- gsub("BLUE_", "ST1_He_", colnames(a3))
colnames(a3) <- list_5
a3 <- a3 %>% rownames_to_column(var = "gen")

a4 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/4_Yield/1_stage/predictions_2stage_ASReml_mrbean.csv")
a4 <- a4 %>% dplyr::select(1:3) %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
list_5 <- gsub("BLUE_", "ST1_Yi_", colnames(a4))
colnames(a4) <- list_5
a4 <- a4 %>% rownames_to_column(var = "gen")

a5 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/5_FD/1_stage/predictions_2stage_ASReml_mrbean.csv")
a5 <- a5 %>% dplyr::select(1:3) %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
list_5 <- gsub("BLUE_", "ST1_FD_", colnames(a5))
colnames(a5) <- list_5
a5 <- a5 %>% rownames_to_column(var = "gen")

################
# 2 stage results (six measures) year: 2018, 2019, 2020

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/1_MSC/2_stage/")
data_2st <- list.files(pattern = "2stage.csv", full.names = T)
list_4 <- c("ST2_MS_WA_2019", "ST2_MS_WA_2020")

c1 <- list()
for (i in 1:length(data_2st)) {
  data <- read.csv(data_2st[i])
  data <- data[,c(1,2)]
  c1[[length(c1)+1]] = data
}
names(c1) <- list_4
c1 <-rbindlist(c1, use.names=TRUE, fill=TRUE, idcol="trial")
c1 <- c1 %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/3_Height/2_stage/")
data_2st <- list.files(pattern = "2stage.csv", full.names = T)
list_4 <- c("ST2_He_OR_2018", "ST2_He_OR_2019", "ST2_He_OR_2020", "ST2_He_WA_2018", "ST2_He_WA_2019", "ST2_He_WA_2020")

c3 <- list()
for (i in 1:length(data_2st)) {
  data <- read.csv(data_2st[i])
  data <- data[,c(1,2)]
  c3[[length(c3)+1]] = data
}
names(c3) <- list_4
c3 <-rbindlist(c3, use.names=TRUE, fill=TRUE, idcol="trial")
c3 <- c3 %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/4_Yield/2_stage/")
data_2st <- list.files(pattern = "2stage.csv", full.names = T)
# list_4 <- gsub(".csv", "", gsub("./", "ST2_", data_2st))
list_4 <- c("ST2_Yi_ID_2018", "ST2_Yi_ID_2019", "ST2_Yi_OR_2018", "ST2_Yi_OR_2019", "ST2_Yi_OR_2020", "ST2_Yi_WA_2018", "ST2_Yi_WA_2019", "ST2_Yi_WA_2020")

c4 <- list()
for (i in 1:length(data_2st)) {
  data <- read.csv(data_2st[i])
  data <- data[,c(1,2)]
  c4[[length(c4)+1]] = data
}
names(c4) <- list_4
c4 <-rbindlist(c4, use.names=TRUE, fill=TRUE, idcol="trial")
c4 <- c4 %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
# c4$gen <- as.character(c4$gen)

################
# 3 stage results (three locations)

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/1_MSC/3_stage/")
data_3st <- list.files(pattern = "2stage.csv", full.names = T)
# list_4 <- gsub(".csv", "", gsub("./", "ST3_", data_3st))
list_5 <- c("ST3_MS_ID", "ST3_MS_OR", "ST3_MS_WA")

d1 <- list()
for (i in 1:length(data_3st)) {
  data <- read.csv(data_3st[i])
  data <- data[,c(1,2)]
  d1[[length(d1)+1]] = data
}
names(d1) <- list_5
d1 <-rbindlist(d1, use.names=TRUE, fill=TRUE, idcol="trial")
d1 <- d1 %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
# d1$gen <- as.character(d1$gen)

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/2_DM/3_stage/")
data_3st <- list.files(pattern = "2stage.csv", full.names = T)
# list_4 <- gsub(".csv", "", gsub("./", "ST3_", data_3st))
list_5 <- c("ST3_DM_ID", "ST3_DM_OR", "ST3_DM_WA")
d2 <- list()
for (i in 1:length(data_3st)) {
  data <- read.csv(data_3st[i])
  data <- data[,c(1,2)]
  d2[[length(d2)+1]] = data
}
names(d2) <- list_5
d2 <-rbindlist(d2, use.names=TRUE, fill=TRUE, idcol="trial")
d2 <- d2 %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
# d2$gen <- as.character(d2$gen)

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/3_Height/3_stage/")
data_3st <- list.files(pattern = "2stage.csv", full.names = T)
# list_4 <- gsub(".csv", "", gsub("./", "ST3_", data_3st))
list_5 <- c("ST3_He_OR", "ST3_He_WA")
d3 <- list()
for (i in 1:length(data_3st)) {
  data <- read.csv(data_3st[i])
  data <- data[,c(1,2)]
  d3[[length(d3)+1]] = data
}
names(d3) <- list_5
d3 <-rbindlist(d3, use.names=TRUE, fill=TRUE, idcol="trial")
d3 <- d3 %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
# d3$gen <- as.character(d3$gen)

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/4_Yield/3_stage/")
data_3st <- list.files(pattern = "2stage.csv", full.names = T)
# list_4 <- gsub(".csv", "", gsub("./", "ST3_", data_3st))
list_5 <- c("ST3_Yi_ID", "ST3_Yi_OR", "ST3_Yi_WA")
d4 <- list()
for (i in 1:length(data_3st)) {
  data <- read.csv(data_3st[i])
  data <- data[,c(1,2)]
  d4[[length(d4)+1]] = data
}
names(d4) <- list_5
d4 <-rbindlist(d4, use.names=TRUE, fill=TRUE, idcol="trial")
d4 <- d4 %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")

################

# 4 stage results (all yield)

e1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/1_MSC/4_stage/Overall_predictions_2stage.csv")
e1 <- e1 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
colnames(e1)[2] <- "ST4_MS_Overall"

e2 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/2_DM/4_stage/Overall_predictions_2stage.csv")
e2 <- e2 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen") 
colnames(e2)[2] <- "ST4_DM_Overall"

e3 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/3_Height/4_stage/Overall_predictions_2stage.csv")
e3 <- e3 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
colnames(e3)[2] <- "ST4_He_Overall"

e4 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/4_Yield/4_stage/Overall_predictions_2stage.csv")
e4 <- e4 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen") 
colnames(e4)[2] <- "ST4_Yi_Overall"

e5 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/5_FD/4_stage/Overall_predictions_2stage.csv")
e5 <- e5 %>% dplyr::select(1,2) %>% remove_rownames() %>% column_to_rownames(var = "gen") %>% rownames_to_column(var = "gen")
colnames(e5)[2] <- "ST4_FD_Overall"

rm(data)

##~~~~~~~~~~~~~
PCA <- read.csv("~/Documents/Cesar/git/big_files/pheno.csv")
colnames(PCA)
PCA <- PCA %>% dplyr::select(c(1,109:111))
str(PCA)
PCA$gen <- as.character(PCA$gen)

f1 <- inner_join(a1, a2, by = "gen") %>% inner_join(., a3, by = "gen") %>% inner_join(., a4, by = "gen") %>% inner_join(., a5, by = "gen") %>% inner_join(., c3, by = "gen") %>% inner_join(., c4, by = "gen") %>% inner_join(., d1, by = "gen") %>% inner_join(., d2, by = "gen") %>% inner_join(., d3, by = "gen")%>% inner_join(., d4, by = "gen") %>% inner_join(., e1, by = "gen") %>% inner_join(., e2, by = "gen") %>% inner_join(., e3, by = "gen") %>% inner_join(., e4, by = "gen") %>% inner_join(., e5, by = "gen") %>% inner_join(., PCA, by = "gen")
colnames(f1)
write.csv(f1, "~/Documents/Cesar/git/big_files/pheno.csv", quote = F, row.names = F)


##~~~~~~~~~~~~~



