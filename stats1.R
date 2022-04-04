
rm(list = ls())
library(tidyverse)
library(data.table)

install.packages("goeveg")
library(goeveg)

# 1 stage results
# MS = 1_MSC
# DM = 2_DM
# He = 3_Height
# Yi = 4_Yield
# FD = 5_FD

# model in for loop
setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/")
setwd("~/Documents/git/Norberg_2020/BLUE_values/split_data/")
data_ar <- list.files(pattern = ".csv", full.names = T)
data_ar1 <- data_ar[c(1,4,8,11,15,19,22,23,27:31)] # 1_MSC
data_ar2 <- data_ar[c(1,4,8,11,15,19,22,27)] # 2_DM
data_ar3 <- data_ar[c(4,8,10:14,16:19,21:31)] # 3_Height
data_ar4 <- data_ar # 4_Yield
data_ar5 <- data_ar[c(7,21,26)] # 5_FD

list_1 <- gsub(".csv", "", gsub("./", "", data_ar1))
list_2 <- gsub(".csv", "", gsub("./", "", data_ar2))
list_3 <- gsub(".csv", "", gsub("./", "", data_ar3))
list_4 <- gsub(".csv", "", gsub("./", "", data_ar4))
list_5 <- gsub(".csv", "", gsub("./", "", data_ar5))

lev1 <- c("block", "gen", "row", "col")
lev2 <- c("env", "loc", "year", "cut", "block", "gen", "row", "col")

#################
# MS = 1_MSC
R_MS <- list()
for (i in 1:length(data_ar1)) {
  data <- read.csv(data_ar1[i])
  data <- data[,c(3,6,7,8,11,16,21)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  R_MS[[length(R_MS)+1]] = data
}
names(R_MS) <- list_1
R_MS <-rbindlist(R_MS, use.names=TRUE, fill=TRUE, idcol="env")

# DM = 2_DM
R_DM <- list()
for (i in 1:length(data_ar2)) {
  data <- read.csv(data_ar2[i])
  data <- data[,c(3,6,7,8,12,17,22)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  R_DM[[length(R_DM)+1]] = data
}
names(R_DM) <- list_2
R_DM <-rbindlist(R_DM, use.names=TRUE, fill=TRUE, idcol="env")

# He = 3_Height
R_He <- list()
for (i in 1:length(data_ar3)) {
  data <- read.csv(data_ar3[i])
  data <- data[,c(3,6,7,8,13,18,23)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  R_He[[length(R_He)+1]] = data
}
names(R_He) <- list_3
R_He <-rbindlist(R_He, use.names=TRUE, fill=TRUE, idcol="env")

# Yi = 4_Yield
R_Yi <- list()
for (i in 1:length(data_ar4)) {
  data <- read.csv(data_ar4[i])
  data <- data[,c(3,6,7,8,14,19,24)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  R_Yi[[length(R_Yi)+1]] = data
}
names(R_Yi) <- list_4
R_Yi <-rbindlist(R_Yi, use.names=TRUE, fill=TRUE, idcol="env")

# FD = 5_FD
R_FD <- list()
for (i in 1:length(data_ar5)) {
  data <- read.csv(data_ar5[i])
  data <- data[,c(3,6,7,8,15,20,25)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  R_FD[[length(R_FD)+1]] = data
}
names(R_FD) <- list_5
R_FD <-rbindlist(R_FD, use.names=TRUE, fill=TRUE, idcol="env")


###########
# summarize

J1 <- list(R_MS,R_DM,R_He,R_Yi,R_FD)
lev3 <- c("MS","DM","He","Yi","FD")
names(J1) <- lev3

J2_new <- list()
J2_201 <- list()
J2_202 <- list()
for (i in 1:length(J1)) {
  data_new <- J1[[i]] %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")
  data_201 <- J1[[i]] %>% dplyr::filter(gen %in% c(201)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")
  data_202 <- J1[[i]] %>% dplyr::filter(gen %in% c(202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")
  
  data_new <- as.data.frame(data_new)
  data_201 <- as.data.frame(data_201)
  data_202 <- as.data.frame(data_202)
  
  data_new[,lev2] <- lapply(data_new[,lev2], factor)
  data_201[,lev2] <- lapply(data_201[,lev2], factor)
  data_202[,lev2] <- lapply(data_202[,lev2], factor)
  
  data_new <- data_new %>% group_by(env) %>% summarise_at(vars(resp), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)
  data_201 <- data_201 %>% group_by(env) %>% summarise_at(vars(resp), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)
  data_202 <- data_202 %>% group_by(env) %>% summarise_at(vars(resp), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)
  
  J2_new[[length(J2_new)+1]] <- data_new
  J2_201[[length(J2_201)+1]] <- data_201
  J2_202[[length(J2_202)+1]] <- data_202
}

names(J2_new) <- lev3
names(J2_201) <- lev3
names(J2_202) <- lev3
J2_new <-rbindlist(J2_new, use.names=TRUE, fill=TRUE, idcol="trait")
J2_201 <-rbindlist(J2_201, use.names=TRUE, fill=TRUE, idcol="trait")
J2_202 <-rbindlist(J2_202, use.names=TRUE, fill=TRUE, idcol="trait")

J2_new$geno <- "new"
J2_201$geno <- "201"
J2_202$geno <- "202"


J3 <- rbind(J2_new, J2_201, J2_202)
J3 <- J3 %>% dplyr::select(trait, env, geno, min, max, mean, sd, cv)  %>% separate(2, c("loc", "year", "cut"), sep = "_", remove = T, convert = FALSE, extra = "merge") 
J3 <- J3[order(J3$trait, J3$loc, J3$year, J3$cut), ]
head(J3)

write.csv(J3, "~/Documents/git/Norberg_2020/BLUE_values/stats1.csv", quote = F, row.names = F)
