# stagewise
rm(list = ls())
library(StageWise)
library(asreml)
library(data.table)
library(tidyverse)
library(asremlPlus)

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/")
data_ar <- list.files(pattern = ".csv", full.names = T)
data_ar5 <- data_ar[c(7,21,26)] # 5_FD

list_5 <- gsub(".csv", "", gsub("./", "", data_ar5))
lev1 <- c("block", "gen", "row", "col")

S_FD4 <- data.frame(FD = c(4,6,5,1,3,2),
                    gen = c(44,61,104,112,144,201))
S_FD4$gen <- as.factor(S_FD4$gen)

S_FD4 <- read.csv("~/Documents/Cesar/git/big_files/FD_scores.csv")
S_FD1 <- S_FD4 %>% dplyr::filter(!gen %in% c(201,202))


# names(BLUE_FD) <- list_5
# BLUE_FD <-rbindlist(BLUE_FD, use.names=TRUE, fill=TRUE, idcol="trait")

S_FD1 <- BLUE_FD %>% dplyr::filter(gen %in% c(44,61,104,112,144,201))
S_FD1 <- inner_join(S_FD1, S_FD4, by = "gen")


S_FD1.1 <- S_FD1 %>% dplyr::filter(trait %in% list_5[1])
S_FD1.2 <- S_FD1 %>% dplyr::filter(trait %in% list_5[2])
S_FD1.3 <- S_FD1 %>% dplyr::filter(trait %in% list_5[3])

S_FD2.1 <- lm(FD ~ BLUE, data = S_FD1.1)
S_FD2.2 <- lm(FD ~ BLUE, data = S_FD1.2)
S_FD2.3 <- lm(FD ~ BLUE, data = S_FD1.3)

summary(S_FD2.1)
summary(S_FD2.2)
summary(S_FD2.3)


S_FD2.1$coefficients[1]
S_FD2.1$coefficients[2]

S_FD3.1 <- BLUE_FD %>% dplyr::filter(trait %in% list_5[1])
S_FD3.2 <- BLUE_FD %>% dplyr::filter(trait %in% list_5[2])
S_FD3.3 <- BLUE_FD %>% dplyr::filter(trait %in% list_5[3])

S_FD3.1$FD1 <- S_FD2.1$coefficients[2] * S_FD3.1$BLUE + S_FD2.1$coefficients[1]
S_FD3.2$FD1 <- S_FD2.2$coefficients[2] * S_FD3.2$BLUE + S_FD2.2$coefficients[1]
S_FD3.3$FD1 <- S_FD2.3$coefficients[2] * S_FD3.3$BLUE + S_FD2.3$coefficients[1]

S_FD5 <- rbind(S_FD3.1, S_FD3.2, S_FD3.3)

S_FD5 <- S_FD5 %>% dplyr::select(1,2,7) %>% spread(key = trait, value = FD1, fill = NA, convert = FALSE, drop = TRUE, sep = NULL) %>% full_join(., PCA, by = "gen") %>% dplyr::filter(!gen %in% c(201, 202))




setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/")
data_ar <- list.files(pattern = ".csv", full.names = T)
data_ar1 <- data_ar[c(1,4,8,11,15,19,22,23,27:31)] # 1_MSC
data_ar2 <- data_ar[c(1,4,8,11,15,19,22,27)] # 2_DM
data_ar3 <- data_ar[c(4,8,10:14,16:19,21:31)] # 3_Height

data_ar6 <- data_ar[c(10,14,18)]

summary(data2)

He_FD <- list()
M_He_FD <- list()
for (i in 1:length(data_ar6)) {
  data <- read.csv(data_ar6[i])
  data <- data[,c(3,6,7,8,13,18,23)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data1 <- inner_join(data, S_FD1, by = "gen")
  data2 <- lm(FD ~ resp, data = data1)
  data$resp <- data2$coefficients[2] * data$resp + data2$coefficients[1]
  
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

  data3 <- rbind(info1, info2, info3)
  M_He_FD[[length(M_He_FD)+1]] = data3
  
  ifelse(info1$AIC < info2$AIC && info1$AIC < info3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(info2$AIC < info1$AIC && info2$AIC < info3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(info3$AIC < info1$AIC && info3$AIC < info2$AIC,
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
  
  data1 <- inner_join(data, S_FD1, by = "gen")
  data2 <- lm(FD ~ resp, data = data1)
  data$resp <- data2$coefficients[2] * data$resp + data2$coefficients[1]
  
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

  data3 <- rbind(info1, info2, info3)
  M_FD[[length(M_FD)+1]] = data3

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

BLUE_FD3 <- inner_join(BLUE_FD1, He_FD1, by = "gen")
colnames(BLUE_FD3)[2:length(BLUE_FD3)] <- gsub("^", "ST1_FD_", colnames(BLUE_FD3)[2:length(BLUE_FD3)])
summary(BLUE_FD3)

BLUE_FD4 <- rbind(BLUE_FD2, He_FD2)



data <- BLUE_FD4
class(data)
data <- as.data.frame(data)
colnames(data)[1] <- "env"
list_5 <- c("env", "loc", "year", "cut", "gen")
data[list_5] <- lapply(data[list_5], factor)
data <- data[order(data$gen, data$env), ]
data1 <- na.omit(data)
head(data1)
str(data1)
Diag <- asreml::asreml(fixed = BLUE ~ 1 + gen +  loc, 
                       random = ~ + diag(env):id(gen),
                       data = data1, na.action = list(x = "include", y = "include"), 
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1), workspace="10gb")

US <- asreml::asreml(fixed = BLUE ~ 1 + env +  loc,
                     random = ~ + idv(env):id(gen),
                     data = data1, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1), workspace="10gb")

FA_1 <- asreml::asreml(fixed = BLUE ~ 1 + gen + loc, 
                       random = ~ + fa(env, 1):id(gen),
                       data = data1, na.action = list(x = "include", y = "include"), 
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1), workspace="10gb")
FA_1 <- update.asreml(FA_1)

FA_2 <- asreml::asreml(fixed = BLUE ~ 1 + gen + loc, 
                       random = ~ + fa(env, 2):id(gen),
                       data = data1, na.action = list(x = "include", y = "include"), 
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1), workspace="10gb")
FA_2 <- update.asreml(FA_2)

i1 <- infoCriteria.asreml(Diag)
i2 <- infoCriteria.asreml(US)
i3 <- infoCriteria.asreml(FA_1)
i4 <- infoCriteria.asreml(FA_2)

i1$model <- "Diag"
i2$model <- "US"
i3$model <- "FA_1"
i4$model <- "FA_2"

data2 <- rbind(i1, i2, i3, i4)

BLUP3 <- predictPlus(classify = "loc:gen", asreml.obj = Diag, 
                     wald.tab = NULL, 
                     present = c("env", "loc", "gen"))$predictions

BLUP4 <- predictPlus(classify = "gen", asreml.obj = Diag, 
                     wald.tab = Diag$wald.tab, 
                     present = c("env", "loc", "gen"))$predictions


BLUP3 <- BLUP3[,c(1:3)]
BLUP4 <- BLUP4[,c(1:2)]


BLUP3$loc <- gsub("^", "ST3_FD_", BLUP3$loc)

BLUP3 <- BLUP3 %>% spread(key = loc, value = predicted.value, fill = NA, 
                          convert = FALSE, drop = TRUE, sep = NULL)
colnames(BLUP4)[2] <- "ST4_FD"
BLUP5 <- inner_join(BLUE_FD3, BLUP3, by = "gen") %>%  inner_join(. , BLUP4, by = "gen") %>% left_join(., PCA, by = "gen")  

write.csv(BLUP5, "~/Documents/Cesar/git/big_files/FD_cal.csv", quote = F, row.names = F)
