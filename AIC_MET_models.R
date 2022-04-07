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
list_5 <- c("env", "loc", "year", "cut", "gen")
list_6 <- c("ST1_MS_", "ST1_DM_", "ST1_He_", "ST1_Yi_", "ST1_FD_")
list_7 <- c("ST3_MS_", "ST3_DM_", "ST3_He_", "ST3_Yi_", "ST3_FD_")
list_8 <- c("ST4_MS", "ST4_DM", "ST4_He", "ST4_Yi", "ST4_FD")
list_9 <- c("MS", "DM", "He", "Yi", "FD")

T2 <- list(BLUE_MS2, BLUE_DM2, BLUE_He2, BLUE_Yi2, BLUE_FD2)
T3 <- list()
T4 <- list()
for (i in 1:(length(T2))) {
  data <- T2[[i]]
  class(data)
  data <- as.data.frame(data)
  colnames(data)[1] <- "env"
  str(data1)
  data[list_5] <- lapply(data[list_5], factor)
  data <- data[order(data$gen, data$env), ]
  data1 <- na.omit(data)
  head(data1)
  
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
  T3[[length(T3)+1]] = data2
  
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
write.csv(T3, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/ST2_AIC.csv", quote = F, row.names = F)

BLUP6 <- full_join(T4[[1]], T4[[2]], by = "gen") %>% full_join(., T4[[3]], by = "gen")  %>% full_join(., T4[[4]], by = "gen")  %>% full_join(., T4[[5]], by = "gen")

colnames(BLUE_MS1)[2:length(BLUE_MS1)] <- gsub("^", "ST1_MS_", colnames(BLUE_MS1)[2:length(BLUE_MS1)])
colnames(BLUE_DM1)[2:length(BLUE_DM1)] <- gsub("^", "ST1_DM_", colnames(BLUE_DM1)[2:length(BLUE_DM1)])
colnames(BLUE_He1)[2:length(BLUE_He1)] <- gsub("^", "ST1_He_", colnames(BLUE_He1)[2:length(BLUE_He1)])
colnames(BLUE_Yi1)[2:length(BLUE_Yi1)] <- gsub("^", "ST1_Yi_", colnames(BLUE_Yi1)[2:length(BLUE_Yi1)])
colnames(BLUE_FD1)[2:length(BLUE_FD1)] <- gsub("^", "ST1_FD_", colnames(BLUE_FD1)[2:length(BLUE_FD1)])

BLUP7 <- full_join(BLUE_MS1, BLUE_DM1, by = "gen") %>% full_join(., BLUE_He1, by = "gen") %>% full_join(., BLUE_Yi1, by = "gen") %>% full_join(., BLUE_FD1, by = "gen") 

BLUP8 <- full_join(BLUP7, BLUP6, by = "gen") %>% full_join(., PCA, by = "gen") %>% dplyr::filter(!gen %in% c(201, 202))
write.csv(BLUP8, "~/Documents/Cesar/git/big_files/pheno_fa2.csv", quote = F, row.names = F)


data <- BLUE_Yi2
class(data)
data <- as.data.frame(data)
head(data)
dim(data)
colnames(data)[1] <- "env"
data[list_5] <- lapply(data[list_5], factor)
data <- data[order(data$gen, data$env), ]
data1 <- na.omit(data)
head(data1)
str(data1)

i1$AIC < i2$AIC && i1$AIC < i3$AIC && i1$AIC < i4$AIC
i2$AIC < i1$AIC && i2$AIC < i3$AIC && i2$AIC < i4$AIC
i3$AIC < i1$AIC && i3$AIC < i2$AIC && i3$AIC < i4$AIC
i4$AIC < i1$AIC && i4$AIC < i2$AIC && i4$AIC < i3$AIC


BLUP6 <- full_join(T4[1], T4[2], by = "gen") %>% full_join(., T4[3], by = "gen")  %>% full_join(., T4[5], by = "gen")  %>% full_join(., T4[5], by = "gen")  %>% full_join(., PCA, by = "gen") 

# names(T4) <- c("MS","DM","He","Yi","FD")
# T4 <-rbindlist(T4, use.names=TRUE, fill=TRUE, idcol="trait")
# write.csv(T4, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/ST2_AIC.csv", quote = F, row.names = F)


IDV <- asreml::asreml(fixed = BLUE ~ 1 + gen + loc, 
                      random = ~ + env + idv(env):idv(gen),
                      data = data1, na.action = list(x = "include", y = "include"), 
                      weights = weight, family = asreml::asr_gaussian(dispersion = 1))

infoCriteria.asreml(IDV)

#  Cross-classified model with GE 
mm2 <- asreml::asreml(fixed = BLUE ~ 1 ++ gen + env,
                      random = ~ gen + env:gen, 
                      residual = ~ dsum(~ env|gen), 
                      data = data1, na.action = list(x = "include", y = "include"), 
                      weights = weight, family = asreml::asr_gaussian(dispersion = 1))



summary(FA_2)$varcomp
FA1_sum <- as.data.frame(summary(FA_1)$varcomp)
FA2_sum <- as.data.frame(summary(FA_2)$varcomp)
head(FA1_sum)
param <- FA_1$vparameters
vpredict(FA_1, rb ~ V2 / (sqrt(V1)*sqrt(V3)))

varcomps8 <- summary(FA_1)$varcomp
head(varcomps8)
fa_comps <- varcomps8[grep("fa", rownames(varcomps8)),"component", drop = F]
head(fa_comps)
#remove the leading characters
rownames(fa_comps) <- sub(".*env.", "", rownames(fa_comps))
#split into var, fa1, and fa2 parameter estimates
fa_vars <- fa_comps[grep("var", rownames(fa_comps)), ,drop = F]
rownames(fa_vars) = sub(".var", "", rownames(fa_vars))
colnames(fa_vars) = "var"

#use this for rotated loadings
fa_vars$fa1 = L.star[,1]
fa_vars$fa2 = L.star[,2]
fa_vars$site = rownames(fa_vars)
fa_vars = mutate(fa_vars, Vg = var + (fa1^2) + (fa2^2))
fa_vars

varcomps8$component
head(varcomps8)
error.vars = varcomps8[grep("variance", rownames(varcomps8)),"component", drop = F] 
error.vars$site = substr(rownames(error.vars), start = 6, stop = 8)
error.vars$site = gsub("!", "", error.vars$site)

### Get the within-site heritability 
Env.stats = merge(fa_vars, error.vars, by = "site")
colnames(Env.stats)[colnames(Env.stats)=="component"] = "Ve"
Env.stats = mutate(Env.stats, h2 = Vg*4/(Vg + Ve))

### Get the overall mean height at each site
Env.means = predict(FA_1, classify = "env")
Env.means = Env.means$pvals[,1:2]
colnames(Env.means) = c("site", "height")

Env.stats = merge(Env.stats, Env.means, by = "site")

#get the fa1 scores for all females 
fa1_scores = coef(FA_1)$random[grep("Comp1:female", rownames(coef(mm8)$random)),]
names(fa1_scores) = sub("fa(site, 2)_Comp1:female_", "", names(fa1_scores), fixed = T)


BLUP5 <- predict.asreml(FA_1, classify='env:gen', sed = T)$pvals
BLUP6 <- predict.asreml(FA_1, classify='loc:gen', sed = T)$pvals
BLUP7 <- predict.asreml(FA_1, classify='gen', sed = T)$pvals
