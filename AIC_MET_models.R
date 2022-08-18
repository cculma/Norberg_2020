# comparison of different ASReml models
# spacial enphasis on genetic variance metrix in MET.
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

# ST2
load("~/Documents/Cesar/git/big_files/data_6.RData")
load("~/Documents/Cesar/git/big_files/AIC_ST1.RData")
list_5.1 <- c("env", "loc", "year", "cut", "gen")
list_6 <- c("ST1_MS_", "ST1_DM_", "ST1_He_", "ST1_Yi_", "ST1_FD_")
list_7 <- c("ST3_MS_", "ST3_DM_", "ST3_He_", "ST3_Yi_", "ST3_FD_")
list_8 <- c("ST4_MS", "ST4_DM", "ST4_He", "ST4_Yi", "ST4_FD")
list_9 <- c("MS", "DM", "He", "Yi", "FD")

T2 <- list(BLUE_MS2, BLUE_DM2, BLUE_He2, BLUE_Yi2, BLUE_FD4)
names(T2) <- list_9
T3 <- list()
T4 <- list()

head(BLUE_Yi2)
# 1 + gen +  loc
data <- BLUE_Yi2
data <- BLUE_DM2
data <- BLUE_He2
data <- BLUE_MS2
data <- FD4.2
data <- BLUE_FD4
levels(data$loc)


hist(BLUE_Yi2$BLUE)
for (i in 1:(length(T2))) {
  data <- T2[[4]]
  class(data)
  data <- as.data.frame(data)
  colnames(data)[1] <- "env"
  str(data1)
  data[list_5.1] <- lapply(data[list_5.1], factor)
  data <- data[order(data$gen, data$env), ]
  data1 <- na.omit(data)
  head(data1)
  
  Diag <- asreml::asreml(fixed = BLUE ~ 1 + loc, 
                         random = ~ + diag(env):id(gen),
                         data = data1, na.action = list(x = "include", y = "include"), 
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  US <- asreml::asreml(fixed = BLUE ~ 1 + loc,
                       random = ~ + idv(env):id(gen),
                       data = data1, na.action = list(x = "include", y = "include"),
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  CORGH <- asreml::asreml(fixed = BLUE ~ 1 +  loc,
                       random = ~ + corgh(env):id(gen),
                       data = data1, na.action = list(x = "include", y = "include"),
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  FA_1 <- asreml::asreml(fixed = BLUE ~ 1 +  loc + gen, 
                         random = ~ + fa(env, 1):id(gen),
                         data = data1, na.action = list(x = "include", y = "include"), 
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  FA_1 <- update.asreml(FA_1)
  
  FA_2 <- asreml::asreml(fixed = BLUE ~ 1 +  loc, 
                         random = ~ + fa(env, 2):id(gen),
                         data = data1, na.action = list(x = "include", y = "include"), 
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  FA_2 <- update.asreml(FA_2)
  
  FA_3 <- asreml::asreml(fixed = BLUE ~ 1 +  loc, 
                         random = ~ + fa(env, 3):id(gen),
                         data = data1, na.action = list(x = "include", y = "include"), 
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  FA_3 <- update.asreml(FA_3)
  
  i1 <- infoCriteria.asreml(Diag)
  i2 <- infoCriteria.asreml(US)
  i3 <- infoCriteria.asreml(FA_1)
  i4 <- infoCriteria.asreml(FA_2)
  i5 <- infoCriteria.asreml(FA_3)
  i6 <- infoCriteria.asreml(CORGH)
  
  i1$model <- "Diag"
  i2$model <- "US"
  i3$model <- "FA_1"
  i4$model <- "FA_2"
  i5$model <- "FA_3"
  i6$model <- "CORGH"
  
  data2 <- rbind(i1, i2, i3, i4, i5, i6)

  BLUP2 <- predict.asreml(FA_1, classify='env:gen', sed = F)
  BLUP2 <- BLUP2$pvals
  
  # T3[[length(T3)+1]] = data2
  
  ifelse(i1$AIC < i2$AIC && i1$AIC < i3$AIC && i1$AIC < i4$AIC, 
         c(BLUP3 <- predictPlus(classify = "loc:gen", asreml.obj = Diag, 
                                wald.tab = NULL, 
                                present = c("env", "loc", "gen"))$predictions,
           BLUP4 <- predictPlus(classify = "gen", asreml.obj = Diag, 
                                wald.tab = Diag$wald.tab, 
                                present = c("env", "loc", "gen"))$predictions),
         ifelse(i2$AIC < i1$AIC && i2$AIC < i3$AIC && i2$AIC < i4$AIC,
                c(BLUP3 <- predictPlus(classify = "loc:gen", asreml.obj = US, 
                                       wald.tab = NULL, 
                                       present = c("env", "loc", "gen"))$predictions,
                  BLUP4 <- predictPlus(classify = "gen", asreml.obj = US, 
                                       wald.tab = NULL, 
                                       present = c("env", "loc", "gen"))$predictions),
                ifelse(i3$AIC < i1$AIC && i3$AIC < i2$AIC && i3$AIC < i4$AIC, 
                       c(BLUP3 <- predictPlus(classify = "loc:gen", asreml.obj = FA_1, 
                                              wald.tab = NULL, 
                                              present = c("env", "loc", "gen"))$predictions,
                         BLUP4 <- predictPlus(classify = "gen", asreml.obj = FA_1, 
                                              wald.tab = NULL, 
                                              present = c("env", "loc", "gen"))$predictions),
                       ifelse(i4$AIC < i1$AIC && i4$AIC < i2$AIC && i4$AIC < i3$AIC, 
                              c(BLUP3 <- predictPlus(classify = "loc:gen", asreml.obj = FA_2, 
                                                     wald.tab = Diag$wald.tab, 
                                                     present = c("env", "loc", "gen"))$predictions,
                                BLUP4 <- predictPlus(classify = "gen", asreml.obj = FA_2, 
                                                     wald.tab = Diag$wald.tab, 
                                                     present = c("env", "loc", "gen"))$predictions)
                       ))))
  
  BLUP3 <- BLUP3[,c(1:3)]
  BLUP4 <- BLUP4[,c(1:2)]
  
  BLUP3$loc <- gsub("^", list_7[i], BLUP3$loc)
  
  BLUP3 <- BLUP3 %>% spread(key = loc, value = predicted.value, fill = NA, 
                            convert = FALSE, drop = TRUE, sep = NULL)
  colnames(BLUP4)[2] <- list_8[i]
  BLUP5 <- full_join(BLUP3, BLUP4, by = "gen") 
  T4[[length(T4)+1]] = BLUP5
}

names(T3) <- list_9
T3 <-rbindlist(T3, use.names=TRUE, fill=TRUE, idcol="trait")
T3[ , .SD[which.min(AIC)], by = trait]

write.csv(T3, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/ST2_AIC.csv", quote = F, row.names = F)

names(T4) <- c("MS","DM","He","Yi","FD")

BLUP6 <- full_join(T4[[1]], T4[[2]], by = "gen") %>% full_join(., T4[[3]], by = "gen")  %>% full_join(., T4[[4]], by = "gen")  %>% full_join(., T4[[5]], by = "gen")  %>% left_join(., PCA, by = "gen") 
write.csv(BLUP6, "~/Documents/Cesar/git/big_files/pheno_fa1.csv", quote = F, row.names = F)

FA_1
summary(FA_1)$varcomp
BLUP3$p.differences
