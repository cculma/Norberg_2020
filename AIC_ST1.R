# stagewise
rm(list = ls())
library(StageWise)
library(asreml)
library(data.table)
library(tidyverse)
library(asremlPlus)

#################
# 1 stage results
# MS = 1_MSC
# DM = 2_DM
# He = 3_Height
# Yi = 4_Yield
# FD = 5_FD

# model in for loop
setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/")
data_ar <- list.files(pattern = ".csv", full.names = T)
data_ar1 <- data_ar[c(1,4,8,11,15,19,22,23,27:31)] # 1_MSC
data_ar2 <- data_ar[c(1,4,8,11,15,19,22,27)] # 2_DM
# data_ar3 <- data_ar[c(4,8,10:14,16:19,21:31)] # 3_Height
data_ar3 <- data_ar[c(4,8,11,12,14,16,17,19,21:31)] # 3_Height removing OR FD
data_ar4 <- data_ar # 4_Yield
data_ar5 <- data_ar[c(7,21,26)] # 5_FD

list_1 <- gsub(".csv", "", gsub("./", "", data_ar1))
list_2 <- gsub(".csv", "", gsub("./", "", data_ar2))
list_3 <- gsub(".csv", "", gsub("./", "", data_ar3))
list_4 <- gsub(".csv", "", gsub("./", "", data_ar4))
list_5 <- gsub(".csv", "", gsub("./", "", data_ar5))

lev1 <- c("block", "gen", "row", "col")
clnames <- c("cov1","cov2")
####################

M_MS <- list()
BLUE_MS <- list()
for (i in 1:length(data_ar1)) {
  data <- read.csv(data_ar1[i])
  data <- data[,c(3,6,7,8,11,16,21)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
  
  m1 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, residual = ~sar(row):sar(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, residual = ~ar1(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  info1 <- infoCriteria.asreml(m1)
  info2 <- infoCriteria.asreml(m2)
  info3 <- infoCriteria.asreml(m3)

  info1$model <- "sar_sar"
  info2$model <- "ar1_id"
  info3$model <- "ar1_ar1"

  data1 <- rbind(info1, info2, info3)
  M_MS[[length(M_MS)+1]] = data1
  
  ifelse(info1$AIC < info2$AIC && info1$AIC < info3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(info2$AIC < info1$AIC && info2$AIC < info3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(info3$AIC < info1$AIC && info3$AIC < info2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
  BLUE_MS[[length(BLUE_MS)+1]] = blue
}
names(M_MS) <- list_1
M_MS <-rbindlist(M_MS, use.names=TRUE, fill=TRUE, idcol="trait")
M_MS[ , .SD[which.min(AIC)], by = trait]

names(BLUE_MS) <- list_1
BLUE_MS <-rbindlist(BLUE_MS, use.names=TRUE, fill=TRUE, idcol="trait")
BLUE_MS1 <- BLUE_MS %>% dplyr::filter(!gen %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = trait, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
BLUE_MS2 <- BLUE_MS %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")

colnames(BLUE_MS1)[2:length(BLUE_MS1)] <- gsub("^", "ST0_MS_", colnames(BLUE_MS1)[2:length(BLUE_MS1)])

data2$trait <- "MS"
data2 <- data2[,c(8,1:7)]
M_MS <- rbind(M_MS, data2)
write.csv(M_MS, "~/Documents/Cesar/git/big_files/MSC_AIC.csv", quote = F, row.names = F)


#################

M_DM <- list()
BLUE_DM <- list()
for (i in 1:length(data_ar2)) {
  data <- read.csv(data_ar2[i])
  data <- data[,c(3,6,7,8,12,17,22)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
  
  m1 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block,
                       residual = ~sar(row):sar(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, 
                       residual = ~ar1(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, 
                       residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))

  
  info1 <- infoCriteria.asreml(m1)
  info2 <- infoCriteria.asreml(m2)
  info3 <- infoCriteria.asreml(m3)

  info1$model <- "sar_sar"
  info2$model <- "ar1_id"
  info3$model <- "ar1_ar1"

  data1 <- rbind(info1, info2, info3)
  M_DM[[length(M_DM)+1]] = data1
  
  ifelse(info1$AIC < info2$AIC && info1$AIC < info3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(info2$AIC < info1$AIC && info2$AIC < info3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(info3$AIC < info1$AIC && info3$AIC < info2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
  BLUE_DM[[length(BLUE_DM)+1]] = blue
}

names(M_DM) <- list_2
M_DM <-rbindlist(M_DM, use.names=TRUE, fill=TRUE, idcol="trait")
M_DM[, .SD[which.min(AIC)], by = trait]


names(BLUE_DM) <- list_2
BLUE_DM <-rbindlist(BLUE_DM, use.names=TRUE, fill=TRUE, idcol="trait")
BLUE_DM1 <- BLUE_DM %>% dplyr::filter(!gen %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = trait, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
BLUE_DM2 <- BLUE_DM %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")

colnames(BLUE_DM1)[2:length(BLUE_DM1)] <- gsub("^", "ST0_DM_", colnames(BLUE_DM1)[2:length(BLUE_DM1)])


#################
M_He <- list()
BLUE_He <- list()
for (i in 1:length(data_ar3)) {
  data <- read.csv(data_ar3[i])
  data <- data[,c(3,6,7,8,13,18,23)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
  
  m1 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, residual = ~sar(row):sar(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, residual = ~ar1(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  info1 <- infoCriteria.asreml(m1)
  info2 <- infoCriteria.asreml(m2)
  info3 <- infoCriteria.asreml(m3)
  
  info1$model <- "sar_sar"
  info2$model <- "ar1_id"
  info3$model <- "ar1_ar1"

  data1 <- rbind(info1, info2, info3)
  M_He[[length(M_He)+1]] = data1
  
  ifelse(info1$AIC < info2$AIC && info1$AIC < info3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(info2$AIC < info1$AIC && info2$AIC < info3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(info3$AIC < info1$AIC && info3$AIC < info2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
  BLUE_He[[length(BLUE_He)+1]] = blue
}
names(M_He) <- list_3
M_He <-rbindlist(M_He, use.names=TRUE, fill=TRUE, idcol="trait")
M_He[ , .SD[which.min(AIC)], by = trait]

names(BLUE_He) <- list_3
BLUE_He <-rbindlist(BLUE_He, use.names=TRUE, fill=TRUE, idcol="trait")
BLUE_He1 <- BLUE_He %>% dplyr::filter(!gen %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = trait, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
BLUE_He2 <- BLUE_He %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")

colnames(BLUE_He1)[2:length(BLUE_He1)] <- gsub("^", "ST0_PH_", colnames(BLUE_He1)[2:length(BLUE_He1)])

data2$trait <- "PH"
data2 <- data2[,c(8,1:7)]
M_He <- rbind(M_He, data2)
write.csv(M_He, "~/Documents/Cesar/git/big_files/PH_AIC.csv", quote = F, row.names = F)


######################

M_Yi <- list()
BLUE_Yi <- list()
for (i in 1:length(data_ar4)) {
  data <- read.csv(data_ar4[i])
  data <- data[,c(3,6,7,8,14,19,24)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
    
  m1 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2,  
                       random = ~ + block + spl(row), 
                       residual = ~sar(row):sar(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block + spl(row), 
                       residual = ~ar1(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block + spl(row), 
                       residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  info1 <- infoCriteria.asreml(m1)
  info2 <- infoCriteria.asreml(m2)
  info3 <- infoCriteria.asreml(m3)
  
  info1$model <- "sar_sar"
  info2$model <- "ar1_id"
  info3$model <- "ar1_ar1"

  data1 <- rbind(info1, info2, info3)
  M_Yi[[length(M_Yi)+1]] = data1
  
  ifelse(info1$AIC < info2$AIC && info1$AIC < info3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(info2$AIC < info1$AIC && info2$AIC < info3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(info3$AIC < info1$AIC && info3$AIC < info2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
  BLUE_Yi[[length(BLUE_Yi)+1]] = blue
}

names(M_Yi) <- list_4
M_Yi <-rbindlist(M_Yi, use.names=TRUE, fill=TRUE, idcol="trait")
M_Yi[ , .SD[which.min(AIC)], by = trait]

names(BLUE_Yi) <- list_4
BLUE_Yi <-rbindlist(BLUE_Yi, use.names=TRUE, fill=TRUE, idcol="trait")
BLUE_Yi1 <- BLUE_Yi %>% dplyr::filter(!gen %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = trait, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
BLUE_Yi2 <- BLUE_Yi %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")

colnames(BLUE_Yi1)[2:length(BLUE_Yi1)] <- gsub("^", "ST0_Yi_", colnames(BLUE_Yi1)[2:length(BLUE_Yi1)])

data2$trait <- "Yi"
data2 <- data2[,c(8,1:7)]
M_Yi <- rbind(M_Yi, data2)
write.csv(M_Yi, "~/Documents/Cesar/git/big_files/Yi_AIC.csv", quote = F, row.names = F)

save.image("~/Documents/Cesar/git/big_files/ST0_Yi.RData")
load("~/Documents/Cesar/git/big_files/ST0_Yi.RData")
####################

M_FD <- list()
BLUE_FD <- list()
for (i in 1:length(data_ar5)) {
  data <- read.csv(data_ar5[i])
  data <- data[,c(3,6,7,8,15,20,25)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
  
  m1 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block + row:col, residual = ~sar(row):sar(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block + row:col, residual = ~ar1(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block + row:col, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  
  info1 <- infoCriteria.asreml(m1)
  info2 <- infoCriteria.asreml(m2)
  info3 <- infoCriteria.asreml(m3)

  info1$model <- "sar_sar"
  info2$model <- "ar1_id"
  info3$model <- "ar1_ar1"

  data1 <- rbind(info1, info2, info3)
  M_FD[[length(M_FD)+1]] = data1
  
  ifelse(info1$AIC < info2$AIC && info1$AIC < info3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(info2$AIC < info1$AIC && info2$AIC < info3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(info3$AIC < info1$AIC && info3$AIC < info2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
  BLUE_FD[[length(BLUE_FD)+1]] = blue
}

names(M_FD) <- list_5
M_FD <-rbindlist(M_FD, use.names=TRUE, fill=TRUE, idcol="trait")
M_FD[ , .SD[which.min(AIC)], by = trait]

names(BLUE_FD) <- list_5
BLUE_FD <-rbindlist(BLUE_FD, use.names=TRUE, fill=TRUE, idcol="trait")
BLUE_FD1 <- BLUE_FD %>% dplyr::filter(!gen %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = trait, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
BLUE_FD2 <- BLUE_FD %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")

#######################


M_DM[ , .SD[which.min(AIC)], by = trait]
M_He[ , .SD[which.min(AIC)], by = trait]
M_MS[ , .SD[which.min(AIC)], by = trait]
M_Yi[ , .SD[which.min(AIC)], by = trait]
M_FD[ , .SD[which.min(AIC)], by = trait]

M_DM$trait1 <- "DM"
M_He$trait1 <- "He"
M_MS$trait1 <- "MS"
M_Yi$trait1 <- "Yi"
M_FD$trait1 <- "FD"

J1 <- rbind(M_DM,M_He,M_MS,M_Yi,M_FD)
write.csv(J1, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/ST1_AIC.csv", quote = F, row.names = F)
save.image("~/Documents/Cesar/git/big_files/AIC_ST1.RData")
load("~/Documents/Cesar/git/big_files/AIC_ST1.RData")


J1 <- read.csv("~/Documents/git/Norberg_2020/BLUE_values/ST1_AIC.csv")
J1$trait1 <- as.factor(J1$trait1)
head(J1)
J3 <- J1[,c(1,5,8,9)]
J3 <- J3 %>% spread(key = trait1, value = AIC, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
write.csv(J3, "~/Documents/git/Norberg_2020/BLUE_values/ST1_AIC_1.csv", quote = F, row.names = F)

levels(J1$trait1)
J1.1 <- J1 %>% dplyr::filter(trait1 %in% c("DM")) %>% dplyr::select(1,5,8) %>% spread(key = model, value = AIC, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

J1.2 <- J1 %>% dplyr::filter(trait1 %in% c("FD")) %>% dplyr::select(1,5,8) %>% spread(key = model, value = AIC, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

J1.3 <- J1 %>% dplyr::filter(trait1 %in% c("He")) %>% dplyr::select(1,5,8) %>% spread(key = model, value = AIC, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

J1.4 <- J1 %>% dplyr::filter(trait1 %in% c("MS")) %>% dplyr::select(1,5,8) %>% spread(key = model, value = AIC, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

J1.5 <- J1 %>% dplyr::filter(trait1 %in% c("Yi")) %>% dplyr::select(1,5,8) %>% spread(key = model, value = AIC, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

colnames(J1.1)[2:length(J1.1)] <- gsub("^", "DM_", colnames(J1.1)[2:length(J1.1)])
colnames(J1.2)[2:length(J1.2)] <- gsub("^", "FD_", colnames(J1.2)[2:length(J1.2)])
colnames(J1.3)[2:length(J1.3)] <- gsub("^", "He_", colnames(J1.3)[2:length(J1.3)])
colnames(J1.4)[2:length(J1.4)] <- gsub("^", "MS_", colnames(J1.4)[2:length(J1.4)])
colnames(J1.5)[2:length(J1.5)] <- gsub("^", "Yi_", colnames(J1.5)[2:length(J1.5)])

J2 <- full_join(J1.1, J1.2, by = "trait") %>% full_join(., J1.3, by = "trait") %>% full_join(., J1.4, by = "trait") %>% full_join(., J1.5, by = "trait")

write.csv(J2, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/ST1_AIC_1.csv", quote = F, row.names = F)

