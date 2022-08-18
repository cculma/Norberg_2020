# stagewise
rm(list = ls())
library(StageWise)
library(asreml)
library(data.table)
library(tidyverse)
library(asremlPlus)

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/")
data_ar4 <- data_ar # 4_Yield
list_4 <- gsub(".csv", "", gsub("./", "", data_ar4))
lev1 <- c("block", "gen", "row", "col")


a1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/spatial_distribution/cols_rows1.csv", check.names = F)
head(a1)
b1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/original_data/Guojie_2020.csv")
a3 <- inner_join(a1, b1, by = c("Location", "Block", "Position", "ID", "Treatment"))

colnames(a3)
head(a3)
a3 <- a3[,c(1:9,13)]
a3 <- a3 %>% unite("env", c(Location, Year), sep = "_", remove = F) 
lev2 <- c("Location","Year","Cut","Block","Position","ID","Treatment","row","col","env")
a3[,lev2] <- lapply(a3[,lev2], factor)
str(a3)
levels(a3$env)
a4 <- split(a3[-1], a3$env)

a1$ID <- as.factor(a1$ID)
names(a4)

Y1 <- list()
for (i in 1:(length(a4))) {
  a5 <- a4[[i]] %>% group_by(ID) %>% summarise_at(vars(Yield_MgHa), list(yield = sum), na.rm = T)
  a5 <- inner_join(a1, a5, by = "ID")
  b1_201 <- a5 %>% dplyr::filter(Treatment %in% c(201))
  head(b1_201)
  b1_201 <- b1_201[,c(1,2,8)]
  colnames(b1_201)[3] <- "C_201"
  b1_202 <- a5 %>% dplyr::filter(Treatment %in% c(202))
  head(b1_202)
  b1_202 <- b1_202[,c(1,2,8)]
  colnames(b1_202)[3] <- "C_202"
  a5 <- a5 %>% inner_join(., b1_201, by = c("Location", "Block")) %>% inner_join(., b1_202, by = c("Location", "Block"))
  a5$yield [a5$yield == 0] <- NA
  Y1[[length(Y1)+1]] = a5
}
names(Y1) <- names(a4)
head(Y1[[1]])
str(data)
dim(data)

J2_sum <- list()
for (i in 1:length(Y1)) {
  data <- Y1[[i]]
  colnames(data) <- c("loc","block","pos","ID","gen","row","col","resp","cov1","cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data_new <- data %>% dplyr::filter(!gen %in% c(201, 202))
  data_201 <- data %>% dplyr::filter(gen %in% c(201)) 
  data_202 <- data %>% dplyr::filter(gen %in% c(202))
  
  data_new <- data_new %>% summarise_at(vars(resp), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)
  data_201 <- data_201 %>% summarise_at(vars(resp), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)
  data_202 <- data_202 %>% summarise_at(vars(resp), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)
  data_new$check <- "T"
  data_201$check <- "201"
  data_202$check <- "202"
  data_new <- rbind(data_new, data_201, data_202)
  J2_sum[[length(J2_sum)+1]] <- data_new
}
names(J2_sum) <- names(Y1)
J2_sum <-rbindlist(J2_sum, use.names=TRUE, fill=TRUE, idcol="env")
write.csv(J2_sum, "~/Documents/Cesar/git/big_files/Sum_Stat.csv", quote = F, row.names = F)


M_Y2 <- list()
B_Y2 <- list()
lev1 <- c("loc","block","pos","ID","gen","row","col")

for (i in 1:length(Y1)) {
  data <- Y1[[1]]
  colnames(data) <- c("loc","block","pos","ID","gen","row","col","resp","cov1","cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
  head(data)
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
  M_Y2[[length(M_Y2)+1]] = data1
  
  ifelse(info1$AIC < info2$AIC && info1$AIC < info3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(info2$AIC < info1$AIC && info2$AIC < info3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(info3$AIC < info1$AIC && info3$AIC < info2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
  B_Y2[[length(B_Y2)+1]] = blue
}
names(B_Y2) <- names(Y1)
names(M_Y2) <- names(Y1)

M_Y2 <-rbindlist(M_Y2, use.names=TRUE, fill=TRUE, idcol="env")
B_Y3 <-rbindlist(B_Y2, use.names=TRUE, fill=TRUE, idcol="env")
head(B_Y3)
head(M_Y2)
B_Y3 <- B_Y3 %>% separate(1, c("loc", "year"), sep = "_", remove = F, convert = FALSE, extra = "merge")


M_Y2[ , .SD[which.min(AIC)], by = env]

list_5.1 <- c("env","loc","year","gen")

class(B_Y3)
data <- as.data.frame(B_Y3)
colnames(data)[1] <- "env"
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
                     c(BLUP2 <- predictPlus(classify = "env:gen", asreml.obj = FA_1, 
                                            wald.tab = NULL, 
                                            present = c("env", "loc", "gen"))$predictions,
                       BLUP3 <- predictPlus(classify = "loc:gen", asreml.obj = FA_1, 
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

BLUP2 <- BLUP2[,c(1:3)]
BLUP3 <- BLUP3[,c(1:3)]
BLUP4 <- BLUP4[,c(1:2)]

head(BLUE1)
head(BLUP2) 
head(BLUP3)
head(BLUP4)

BLUP2 <- BLUP2 %>% spread(key = env, value = predicted.value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
BLUP3 <- BLUP3 %>% spread(key = loc, value = predicted.value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

colnames(BLUP2)[c(2:9)] <- paste0("ST1_", colnames(BLUP2)[c(2:9)])
colnames(BLUP3)[c(2:4)] <- paste0("ST2_", colnames(BLUP3)[c(2:4)])
colnames(BLUP4)[2] <- "ST3_All"

head(B_Y3)
BLUE1 <- B_Y3 %>% select(1,4,5) %>%  spread(key = env, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
colnames(BLUE1)[c(2:9)] <- paste0("ST0_", colnames(BLUE1)[c(2:9)])

BLUP5 <- inner_join(BLUE1, BLUP2, by = "gen") %>% inner_join(., BLUP3, by = "gen") %>% inner_join(., BLUP4, by = "gen") %>% inner_join(., PCA, by = "gen")
write.csv(BLUP5, "~/Documents/Cesar/git/big_files/Sum_yield.csv", quote = F, row.names = F)

head(M_Y2)
M_Y2 <- as.data.frame(M_Y2)
class(M_Y2)
M_Y3 <- M_Y2[,c(1,5,8)] %>% spread(key = model, value = AIC, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

data2$env <- "Stagewise"
data3 <- rbind(M_Y2, data2)
write.csv(M_Y3, "~/Documents/Cesar/git/big_files/AIC_Sum_yield1.csv", quote = F, row.names = F)
write.csv(data3, "~/Documents/Cesar/git/big_files/AIC_Sum_yield.csv", quote = F, row.names = F)


###########
PCA <- read.csv("~/Documents/Cesar/git/big_files/pheno.csv")
PCA <- PCA[,c(1,109:111)]
head(PCA)
PCA$gen <- as.factor(PCA$gen)
##########