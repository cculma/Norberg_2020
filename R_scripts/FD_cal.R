# stagewise
rm(list = ls())
library(StageWise)
library(asreml)
library(data.table)
library(tidyverse)
library(asremlPlus)


setwd("~/Documents/git/Norberg_2020/BLUE_values/split_data/")
data_ar <- list.files(pattern = ".csv", full.names = T)
data_ar5 <- data_ar[c(7,21,26,31)] # 5_FD
list_5 <- gsub(".csv", "", gsub("./", "", data_ar5))
lev1 <- c("block", "gen", "row", "col")

S_FD4 <- read.csv("~/Documents/git/big_files/FD_scores.csv")
S_FD1 <- S_FD4 %>% dplyr::filter(!gen %in% c(201,202))
S_FD4$gen <- as.factor(S_FD4$gen)

setwd("~/Documents/git/Norberg_2020/BLUE_values/split_data/")
data_ar <- list.files(pattern = ".csv", full.names = T)
data_ar5 <- data_ar[c(7,21,26,31)] # 5_FD
data_ar6 <- data_ar[c(10,14,18)] # OR Height to FD

# raw_data

He_FD <- list()
M_He_FD <- list()
for (i in 1:length(data_ar6)) {
  data <- read.csv(data_ar6[i])
  data <- data[,c(3,6,7,8,13,18,23)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data$resp <- cm(data$resp)
  data$cov1 <- cm(data$cov1)
  data$cov2 <- cm(data$cov2)
  data[,lev1] <- lapply(data[,lev1], factor)
  
  data <- data[order(data$row, data$col), ]
  str(data)
  m1 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block , 
                       residual = ~ar1(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, 
                       residual = ~sar(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  i1 <- infoCriteria.asreml(m1)
  i2 <- infoCriteria.asreml(m2)
  i3 <- infoCriteria.asreml(m3)
  
  i1$model <- "ar1_id"
  i2$model <- "sar_id"
  i3$model <- "ar1_ar1"
  
  data3 <- rbind(i1, i2, i3)
  M_He_FD[[length(M_He_FD)+1]] = data3
  
  ifelse(i1$AIC < i2$AIC && i1$AIC < i3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(i2$AIC < i1$AIC && i2$AIC < i3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(i3$AIC < i1$AIC && i3$AIC < i2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
  He_FD[[length(He_FD)+1]] <- blue
  
}

names(He_FD) <- gsub(".csv", "", gsub("./", "", data_ar6))
names(M_He_FD) <- gsub(".csv", "", gsub("./", "", data_ar6))

M_He_FD <-rbindlist(M_He_FD, use.names=TRUE, fill=TRUE, idcol="trait")
M_He_FD[ , .SD[which.min(AIC)], by = trait]

He_FD <-rbindlist(He_FD, use.names=TRUE, fill=TRUE, idcol="trait")
He_FD1 <- He_FD %>% dplyr::filter(!gen %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = trait, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
He_FD2 <- He_FD %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")

M_FD <- list()
BLUE_FD <- list()
for (i in 1:length(data_ar5)) {
  data <- read.csv(data_ar5[i])
  data <- data[,c(3,6,7,8,15,20,25)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
  
  m1 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, 
                       residual = ~ar1(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, 
                       residual = ~sar(row):id(col), 
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
  
  i1$model <- "ar1_id"
  i2$model <- "sar_id"
  i3$model <- "ar1_ar1"
  
  data3 <- rbind(i1, i2, i3)
  M_FD[[length(M_FD)+1]] = data3
  
  ifelse(i1$AIC < i2$AIC && i1$AIC < i3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(i2$AIC < i1$AIC && i2$AIC < i3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(i3$AIC < i1$AIC && i3$AIC < i2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
  BLUE_FD[[length(BLUE_FD)+1]] = blue
}

list_5 <- gsub(".csv", "", gsub("./", "", data_ar5))
names(M_FD) <- list_5
M_FD <-rbindlist(M_FD, use.names=TRUE, fill=TRUE, idcol="trait")
M_FD[ , .SD[which.min(AIC)], by = trait]

M_FD <- rbind(M_FD, M_He_FD)


data2$trait <- "FD"
data2 <- data2[,c(8,1:7)]
M_FD <- rbind(M_FD, data2)

names(BLUE_FD) <- list_5
BLUE_FD <-rbindlist(BLUE_FD, use.names=TRUE, fill=TRUE, idcol="trait")
BLUE_FD1 <- BLUE_FD %>% dplyr::filter(!gen %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = trait, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
BLUE_FD2 <- BLUE_FD %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")

BLUE_FD4 <- rbind(BLUE_FD2, He_FD2)
BLUE_FD3 <- inner_join(BLUE_FD1, He_FD1)

colnames(BLUE_FD3)[2:length(BLUE_FD3)] <- gsub("^", "ST0_R_FD_", colnames(BLUE_FD3)[2:length(BLUE_FD3)])



# data in score 1-11
FD1 <- list()
FD2 <- list()
for (i in 1:length(data_ar6)) {
  data <- read.csv(data_ar6[i])
  data <- data[,c(3,6,7,8,13,18,23)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data$resp <- cm(data$resp)
  data$cov1 <- cm(data$cov1)
  data$cov2 <- cm(data$cov2)
  
  data[,lev1] <- lapply(data[,lev1], factor)
  data1 <- inner_join(data, S_FD1, by = "gen")
  data2 <- lm(FD ~ resp, data = data1)
  data$resp <- data2$coefficients[2] * data$resp + data2$coefficients[1]
  data$cov1 <- data2$coefficients[2] * data$cov1 + data2$coefficients[1]
  data$cov2 <- data2$coefficients[2] * data$cov2 + data2$coefficients[1]
  
  data <- data[order(data$row, data$col), ]
  str(data)
  m1 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block , 
                       residual = ~ar1(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, 
                       residual = ~sar(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  i1 <- infoCriteria.asreml(m1)
  i2 <- infoCriteria.asreml(m2)
  i3 <- infoCriteria.asreml(m3)
  
  i1$model <- "ar1_id"
  i2$model <- "sar_id"
  i3$model <- "ar1_ar1"
  
  data3 <- rbind(i1, i2, i3)
  FD1[[length(FD1)+1]] = data3
  
  ifelse(i1$AIC < i2$AIC && i1$AIC < i3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(i2$AIC < i1$AIC && i2$AIC < i3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(i3$AIC < i1$AIC && i3$AIC < i2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
  FD2[[length(FD2)+1]] <- blue
  
}

names(FD1) <- gsub(".csv", "", gsub("./", "", data_ar6))
names(FD2) <- gsub(".csv", "", gsub("./", "", data_ar6))

FD1 <-rbindlist(FD1, use.names=TRUE, fill=TRUE, idcol="trait")
FD1[ , .SD[which.min(AIC)], by = trait]


FD2 <-rbindlist(FD2, use.names=TRUE, fill=TRUE, idcol="trait")
FD2.1 <- FD2 %>% dplyr::filter(!gen %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = trait, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
FD2.2 <- FD2 %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")

FD3 <- list()
FD4 <- list()
for (i in 1:length(data_ar5)) {
  data <- read.csv(data_ar5[i])
  data <- data[,c(3,6,7,8,15,20,25)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  
  data1 <- inner_join(data, S_FD4, by = "gen")
  head(data1)
  str(data1)
  data2 <- lm(FD ~ resp, data = data1)
  
  data$resp <- data2$coefficients[2] * data$resp + data2$coefficients[1]
  data$cov1 <- data2$coefficients[2] * data$cov1 + data2$coefficients[1]
  data$cov2 <- data2$coefficients[2] * data$cov2 + data2$coefficients[1]
  
  data <- data[order(data$row, data$col), ]
  
  m1 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, 
                       residual = ~ar1(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, 
                       residual = ~sar(row):id(col), 
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
  
  i1$model <- "ar1_id"
  i2$model <- "sar_id"
  i3$model <- "ar1_ar1"
  
  data3 <- rbind(i1, i2, i3)
  FD3[[length(FD3)+1]] = data3
  
  ifelse(i1$AIC < i2$AIC && i1$AIC < i3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(i2$AIC < i1$AIC && i2$AIC < i3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(i3$AIC < i1$AIC && i3$AIC < i2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
  FD4[[length(FD4)+1]] = blue
}

list_5 <- gsub(".csv", "", gsub("./", "", data_ar5))
names(FD3) <- list_5
FD3 <-rbindlist(FD3, use.names=TRUE, fill=TRUE, idcol="trait")
FD3[ , .SD[which.min(AIC)], by = trait]
FD3 <- rbind(FD1, FD3)

data2$trait <- "FD"
data2 <- data2[,c(8,1:7)]
FD3 <- rbind(FD3, data2)


names(FD4) <- list_5
FD4 <-rbindlist(FD4, use.names=TRUE, fill=TRUE, idcol="trait")
FD4.1 <- FD4 %>% dplyr::filter(!gen %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = trait, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
FD4.2 <- FD4 %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")

FD4.1 <- inner_join(FD4.1, FD2.1)
colnames(FD4.1)[2:length(FD4.1)] <- gsub("^", "ST0_S_FD_", colnames(FD4.1)[2:length(FD4.1)])

FD5 <- inner_join(FD4.1, BLUE_FD3)

FD4.2 <- rbind(FD4.2, FD2.2)

# fixed = BLUE ~ 1 + gen +  loc
Diag <- asreml::asreml(fixed = BLUE ~ 1 + gen +  loc, 
                       random = ~ + diag(env):id(gen),
                       data = data1, na.action = list(x = "include", y = "include"), 
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1), workspace="10gb")

US <- asreml::asreml(fixed = BLUE ~ 1 + env +  loc,
                     random = ~ + idv(env):id(gen),
                     data = data1, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1), workspace="10gb")

FA_1 <- asreml::asreml(fixed = BLUE ~ 1 + loc, 
                       random = ~ + fa(env, 1):id(gen),
                       data = data1, na.action = list(x = "include", y = "include"), 
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))
FA_1 <- update.asreml(FA_1)



BLUP2 <- predict.asreml(FA_2, classify='env:gen', sed = F)
BLUP2 <- BLUP2$pvals

BLUP3 <- predictPlus(classify = "loc:gen", asreml.obj = Diag, 
                     wald.tab = NULL, 
                     present = c("env", "loc", "gen"))$predictions

BLUP4 <- predictPlus(classify = "gen", asreml.obj = FA_2, 
                     wald.tab = Diag$wald.tab, 
                     present = c("env", "loc", "gen"))$predictions


BLUP2 <- BLUP2[,c(1:3)]
BLUP3 <- BLUP3[,c(1:3)]
BLUP4 <- BLUP4[,c(1:2)]
BLUP2$env <- gsub("^", "ST1_S_FD_", BLUP2$env)
BLUP3$loc <- gsub("^", "ST3_S_FD_", BLUP3$loc)
BLUP2 <- BLUP2 %>% spread(key = env, value = predicted.value, fill = NA, 
                          convert = FALSE, drop = TRUE, sep = NULL)
BLUP3 <- BLUP3 %>% spread(key = loc, value = predicted.value, fill = NA, 
                          convert = FALSE, drop = TRUE, sep = NULL)
colnames(BLUP4)[2] <- "ST4_S_FD"

# S
BLUP5.1 <- full_join(BLUP2, BLUP3, by = "gen") %>% inner_join(., BLUP4, by = "gen")
# R
BLUP5.2 <- full_join(BLUP2, BLUP3, by = "gen") %>% inner_join(., BLUP4, by = "gen")

BLUP5 <- full_join(FD5, BLUP5.2, by = "gen") %>% inner_join(., BLUP5.1, by = "gen") %>% inner_join(., PCA, by = "gen")
colnames(BLUP5)
write.csv(BLUP5, "~/Documents/Cesar/git/big_files/FD.csv", quote = F, row.names = F)

head(FD3)
head(M_FD)
FD3$trait1 <- "scale" 
M_FD$trait1 <- "raw"

FD_AIC <- rbind(FD3, M_FD)
write.csv(FD_AIC, "~/Documents/Cesar/git/big_files/FD_AIC.csv", quote = F, row.names = F)
###############


# get all variance components

varcomp <- summary(Diag)$varcomp
varcomp <- summary(US)$varcomp
varcomp <- summary(FA_1)$varcomp
varcomp <- summary(FA_2)$varcomp
varcomp <- summary(FA_3)$varcomp
hist(varcomp$component)

summary(m1)

# extract error variance

newnames <- c("V_e", "V_g")
oldnames <- c("fa1", "var")

varcomp <- varcomp  %>% rownames_to_column("comp1") %>% slice(1:(n() - 1)) %>% separate(1, c("model", "env", "var"), sep = "!", remove = T, convert = FALSE, extra = "merge") %>% dplyr::select(2:4) %>% spread(key = var, value = component, fill = NA, convert = FALSE, drop = TRUE, sep = NULL) %>% rename_with(~ newnames, all_of(oldnames))
varcomp$H2 <- varcomp$V_g/(varcomp$V_g + varcomp$V_e)
