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
data_ar3 <- data_ar[c(4,8,10:14,16:19,21:31)] # 3_Height
data_ar4 <- data_ar # 4_Yield
data_ar5 <- data_ar[c(7,21,26)] # 5_FD

data_ar1.1 <- data_ar[c(1,4)] 
data_ar1.2 <- data_ar[c(8,11,15)] 
data_ar1.3 <- data_ar[c(19,22,23,27:31)] 

data_ar2.1 <- data_ar[c(1,4)]
data_ar2.2 <- data_ar[c(8,11,15)]
data_ar2.3 <- data_ar[c(19,22,27)]


data_ar3.2 <- data_ar[c(8,10:14,16:18)]
data_ar3.3 <- data_ar[c(19,21:31)]

data_ar4.1 <- data_ar[c(1:7)]
data_ar4.2 <- data_ar[c(8:18)]
data_ar4.3 <- data_ar[c(19:31)]

list_1 <- gsub(".csv", "", gsub("./", "", data_ar1))
list_2 <- gsub(".csv", "", gsub("./", "", data_ar2))
list_3 <- gsub(".csv", "", gsub("./", "", data_ar3))
list_4 <- gsub(".csv", "", gsub("./", "", data_ar4))
list_5 <- gsub(".csv", "", gsub("./", "", data_ar5))

list_1.1 <- gsub(".csv", "", gsub("./", "", data_ar1.1))
list_1.2 <- gsub(".csv", "", gsub("./", "", data_ar1.2))
list_1.3 <- gsub(".csv", "", gsub("./", "", data_ar1.3))

list_2.1 <- gsub(".csv", "", gsub("./", "", data_ar2.1))
list_2.2 <- gsub(".csv", "", gsub("./", "", data_ar2.2))
list_2.3 <- gsub(".csv", "", gsub("./", "", data_ar2.3))

list_3.2 <- gsub(".csv", "", gsub("./", "", data_ar3.2))
list_3.3 <- gsub(".csv", "", gsub("./", "", data_ar3.3))

list_4.1 <- gsub(".csv", "", gsub("./", "", data_ar4.1))
list_4.2 <- gsub(".csv", "", gsub("./", "", data_ar4.2))
list_4.3 <- gsub(".csv", "", gsub("./", "", data_ar4.3))

###################
# 1_MSC

P1 <- list()
for (i in 1:length(data_ar1)) {
  data <- read.csv(data_ar1[i])
  data <- data[,c(6,3,4,7,8,11,16,21)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P1[[length(P1)+1]] = data
}
names(P1) <- list_1
P1 <-rbindlist(P1, use.names=TRUE, fill=TRUE, idcol="env")

a1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/ID_2018_1.csv")
colnames(a1)

# 2_DM
P2 <- list()
for (i in 1:length(data_ar2)) {
  data <- read.csv(data_ar2[i])
  data <- data[,c(6,3,4,7,8,12,17,22)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P2[[length(P2)+1]] = data
}
names(P2) <- list_2
P2 <-rbindlist(P2, use.names=TRUE, fill=TRUE, idcol="env")

# 3_Height
P3 <- list()
for (i in 1:length(data_ar3)) {
  data <- read.csv(data_ar3[i])
  data <- data[,c(6,3,4,7,8,13,18,23)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P3[[length(P3)+1]] = data
}
names(P3) <- list_3
P3 <-rbindlist(P3, use.names=TRUE, fill=TRUE, idcol="env")

# 4_Yield
P4 <- list()
for (i in 1:length(data_ar4)) {
  data <- read.csv(data_ar4[i])
  data <- data[,c(6,3,4,7,8,14,19,24)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P4[[length(P4)+1]] = data
}
names(P4) <- list_4
P4 <-rbindlist(P4, use.names=TRUE, fill=TRUE, idcol="env")

# 5_FD
P5 <- list()
for (i in 1:length(data_ar5)) {
  data <- read.csv(data_ar5[i])
  data <- data[,c(6,3,4,7,8,15,20,25)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P5[[length(P5)+1]] = data
}
names(P5) <- list_5
P5 <-rbindlist(P5, use.names=TRUE, fill=TRUE, idcol="env")

#############
# ST3

P1.1 <- list()
for (i in 1:length(data_ar1.1)) {
  data <- read.csv(data_ar1.1[i])
  data <- data[,c(6,3,4,7,8,11,16,21)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P1.1[[length(P1.1)+1]] = data
}
names(P1.1) <- list_1.1
P1.1 <-rbindlist(P1.1, use.names=TRUE, fill=TRUE, idcol="env")

P1.2 <- list()
for (i in 1:length(data_ar1.2)) {
  data <- read.csv(data_ar1.2[i])
  data <- data[,c(6,3,4,7,8,11,16,21)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P1.2[[length(P1.2)+1]] = data
}
names(P1.2) <- list_1.2
P1.2 <-rbindlist(P1.2, use.names=TRUE, fill=TRUE, idcol="env")

P1.3 <- list()
for (i in 1:length(data_ar1.3)) {
  data <- read.csv(data_ar1.3[i])
  data <- data[,c(6,3,4,7,8,11,16,21)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P1.3[[length(P1.3)+1]] = data
}
names(P1.3) <- list_1.3
P1.3 <-rbindlist(P1.3, use.names=TRUE, fill=TRUE, idcol="env")

P2.1 <- list()
for (i in 1:length(data_ar2.1)) {
  data <- read.csv(data_ar2.1[i])
  data <- data[,c(6,3,4,7,8,12,17,22)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P2.1[[length(P2.1)+1]] = data
}
names(P2.1) <- list_2.1
P2.1 <-rbindlist(P2.1, use.names=TRUE, fill=TRUE, idcol="env")

P2.2 <- list()
for (i in 1:length(data_ar2.2)) {
  data <- read.csv(data_ar2.2[i])
  data <- data[,c(6,3,4,7,8,12,17,22)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P2.2[[length(P2.2)+1]] = data
}
names(P2.2) <- list_2.2
P2.2 <-rbindlist(P2.2, use.names=TRUE, fill=TRUE, idcol="env")

P2.3 <- list()
for (i in 1:length(data_ar2.3)) {
  data <- read.csv(data_ar2.3[i])
  data <- data[,c(6,3,4,7,8,12,17,22)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P2.3[[length(P2.3)+1]] = data
}
names(P2.3) <- list_2.3
P2.3 <-rbindlist(P2.3, use.names=TRUE, fill=TRUE, idcol="env")

P3.2 <- list()
for (i in 1:length(data_ar3.2)) {
  data <- read.csv(data_ar3.2[i])
  data <- data[,c(6,3,4,7,8,13,18,23)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P3.2[[length(P3.2)+1]] = data
}
names(P3.2) <- list_3.2
P3.2 <-rbindlist(P3.2, use.names=TRUE, fill=TRUE, idcol="env")

P3.3 <- list()
for (i in 1:length(data_ar3.3)) {
  data <- read.csv(data_ar3.3[i])
  data <- data[,c(6,3,4,7,8,13,18,23)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P3.3[[length(P3.3)+1]] = data
}
names(P3.3) <- list_3.3
P3.3 <-rbindlist(P3.3, use.names=TRUE, fill=TRUE, idcol="env")

P4.1 <- list()
for (i in 1:length(data_ar4.1)) {
  data <- read.csv(data_ar4.1[i])
  data <- data[,c(6,3,4,7,8,14,19,24)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P4.1[[length(P4.1)+1]] = data
}
names(P4.1) <- list_4.1
P4.1 <-rbindlist(P4.1, use.names=TRUE, fill=TRUE, idcol="env")

P4.2 <- list()
for (i in 1:length(data_ar4.2)) {
  data <- read.csv(data_ar4.2[i])
  data <- data[,c(6,3,4,7,8,14,19,24)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P4.2[[length(P4.2)+1]] = data
}
names(P4.2) <- list_4.2
P4.2 <-rbindlist(P4.2, use.names=TRUE, fill=TRUE, idcol="env")

P4.3 <- list()
for (i in 1:length(data_ar4.3)) {
  data <- read.csv(data_ar4.3[i])
  data <- data[,c(6,3,4,7,8,14,19,24)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P4.3[[length(P4.3)+1]] = data
}
names(P4.3) <- list_4.3
P4.3 <-rbindlist(P4.3, use.names=TRUE, fill=TRUE, idcol="env")

a5 <- list(P1,P2,P3,P4,P5, 
           P1.1, P1.2, P1.3,
           P2.1, P2.2, P2.3,
           P3.2, P3.3,
           P4.1, P4.2, P4.3)

list_6 <- c("ST4_1MS","ST4_2DM","ST4_3He","ST4_4Yi","ST4_5FD",
            "ST3_1MS_ID","ST3_1MS_OR","ST3_1MS_WA",
            "ST3_2DM_ID","ST3_2DM_OR","ST3_2DM_WA",
            "ST3_3He_OR","ST3_3He_WA",
            "ST3_3Yi_ID","ST3_3Yi_OR","ST3_3Yi_WA")

# MS = 1_MSC
# DM = 2_DM
# He = 3_Height
# Yi = 4_Yield
# FD = 5_FD


list_8 <- c("ST3_MS_ID","ST3_MS_OR","ST3_MS_WA",
            "ST3_DM_ID","ST3_DM_OR","ST3_DM_WA",
            "ST3_He_OR","ST3_He_WA",
            "ST3_Yi_ID","ST3_Yi_OR","ST3_Yi_WA",
            "ST4_MS","ST4_DM","ST4_He","ST4_Yi","ST4_FD")

names(a5) <- list_6

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA_1")
for (i in names(a5)) {
  write.csv(a5[[i]],  paste0(i, '.csv'), row.names = F, quote = F)
}


# model in for loop
setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/")
data_aug <- list.files(pattern = ".csv", full.names = T)
list_7 <- gsub(".csv", "", gsub("./", "ALL_", data_aug))

aug_1 <- list()
for (i in 1:length(data_aug)) {
  data <- read.csv(data_aug[i])
  aug_1[[length(aug_1)+1]] = data
}
names(aug_1) <- list_7

aug_1.1 <- aug_1[[1]] %>% dplyr::filter(env %in% list_1.1)
aug_1.2 <- aug_1[[1]] %>% dplyr::filter(env %in% list_1.2)
aug_1.3 <- aug_1[[1]] %>% dplyr::filter(env %in% list_1.3)

aug_2.1 <- aug_1[[2]] %>% dplyr::filter(env %in% list_2.1)
aug_2.2 <- aug_1[[2]] %>% dplyr::filter(env %in% list_2.2)
aug_2.3 <- aug_1[[2]] %>% dplyr::filter(env %in% list_2.3)

aug_3.2 <- aug_1[[3]] %>% dplyr::filter(env %in% list_3.2)
aug_3.3 <- aug_1[[3]] %>% dplyr::filter(env %in% list_3.3)

aug_4.1 <- aug_1[[4]] %>% dplyr::filter(env %in% list_4.1)
aug_4.2 <- aug_1[[4]] %>% dplyr::filter(env %in% list_4.2)
aug_4.3 <- aug_1[[4]] %>% dplyr::filter(env %in% list_4.3)

aug_2 <- c(list(aug_1.1,aug_1.2,aug_1.3,aug_2.1,aug_2.2,aug_2.3,aug_3.2,aug_3.3,aug_4.1,aug_4.2,aug_4.3), aug_1)
names(aug_2) <- list_8 

aug_3 <- list()
for (i in 1:(length(aug_2))) {
  data <- aug_2[[i]]
  data <- data[,c(1:3)]
  colnames(data) <- c("env", "id", "BLUE")
  aug_3[[length(aug_3)+1]] <- data
}
names(aug_3) <- list_8

effects <- data.frame(name=c("block","position"),
                      fixed=c(FALSE,TRUE),
                      factor=c(TRUE,FALSE))
effects
directory <- "~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA_1"


P5 <- file.path(directory, "assay_FD.csv")

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA_1")
data_cov <- list.files(pattern = ".csv", full.names = T)
list_9 <- gsub(".csv", "", gsub("./", "", data_cov))
list_8

vcov_1 <- list()
for (i in 1:length(data_cov)) {
  ans2a <- Stage1(filename = data_cov[i], traits="resp", effects = effects, solver="asreml")
  stage1.vcov <- ans2a$vcov
  vcov_1[[length(vcov_1)+1]] <- stage1.vcov
}
names(vcov_1) <- list_8
names(aug_3)

directory1 <- "~/Documents/Cesar/git/big_files"
G1 <- file.path(directory1, "Norberg_1.txt")
geno1 <- read_geno(filename=G1, ploidy=4, map=TRUE, min.minor.allele=5)

ST2 <- list()
for (i in 1:length(data_cov)) {
  ans2c <- Stage2(data=aug_3[[i]], vcov=vcov_1[[i]], geno=geno1, silent=FALSE)
  prep <- blup_prep(data=aug_3[[i]], vcov=vcov_1[[i]], geno=geno1, vars=ans2c$vars)
  pred.id <- blup(prep, geno = geno1, what="id")
  ST2[[length(ST2)+1]] <- pred.id
}

# IMPORTANT
save.image("~/Documents/Cesar/git/big_files/stagewise1_Norberg.RData")
load("~/Documents/Cesar/git/big_files/stagewise1_Norberg.RData")

names(ST2) <- list_8
ST2 <-rbindlist(ST2, use.names=TRUE, fill=TRUE, idcol="trial")
ST2 <- ST2[,c(1:3)]
colnames(ST2)[2] <- "gen"
colnames(pred.id)[1] <- "gen"

ST2 <- ST2 %>% spread(key = trial, value = BV)

f1 <- inner_join(pred.id, PCA, by = "gen") 
write.csv(f1, "~/Documents/Cesar/git/big_files/pheno_fa2.csv", quote = F, row.names = F)


prep <- blup_prep(data=ans1$blue,vcov=ans1$vcov,geno=geno1,vars=ans2$vars)
pred.marker <- blup(data=prep,geno=geno1,what="marker",gwas.ncore=2)

?blup_prep
?blup

####################
# MET FA
head(Yield_BLUE)
str(Yield_BLUE2)
Yield_BLUE2 <- as.data.frame(Yield_BLUE2)


head(MSC_BLUE2)
head(DM_BLUE2)
head(Height_BLUE2)
head(Yield_BLUE2)
head(FD_BLUE2)


T1 <- list(MSC_BLUE2, DM_BLUE2, Height_BLUE2)
# T1 <- list(MSC_BLUE2, DM_BLUE2, Height_BLUE2, Yield_BLUE2, FD_BLUE2)
names(T1) <- c("MS", "DM", "He", "Yi")
list_5 <- c("gen", "env", "loc", "year", "cut")


# model used for BLUP5.1

# data <- Yield_BLUE2
# data <- as.data.frame(data)
# data[list_5] <- lapply(data[list_5], factor)
# data <- data[order(data$gen, data$env), ]
# data1 <- na.omit(data)

# FA_1 <- asreml::asreml(fixed = BLUE ~ 1 + loc,
#                        random = ~ + env + facv(env, 1):id(gen),
#                        data = data1, na.action = list(x = "include", y = "include"),
#                        weights = weight, family = asreml::asr_gaussian(dispersion = 1))
# 
# BLUP5.1 <- predict.asreml(FA_1, classify='env:gen', vcov = F, sed = F)$pvals
# BLUP6.1 <- predict.asreml(FA_1, classify='loc:gen', sed = T)$pvals
# BLUP7.1 <- predict.asreml(FA_1, classify='gen', sed = T)$pvals

# data <- FD_BLUE2
# data <- as.data.frame(data)
# data[list_5] <- lapply(data[list_5], factor)
# data <- data[order(data$gen, data$env), ]
# data1 <- na.omit(data)
# str(data1)
# #fa FD
# #facv others
# 
# FA_3 <- asreml::asreml(fixed = BLUE ~ 1 + gen + loc,
#                        random = ~ + env + fa(env, 1):id(gen),
#                        data = data1, na.action = list(x = "include", y = "include"),
#                        weights = weight, family = asreml::asr_gaussian(dispersion = 1))
# 
# BLUP1 <- predict.asreml(FA_3, classify='env:gen', sed = T)$pvals
# BLUP3 <- predict.asreml(FA_3, classify='loc:gen', sed = T)$pvals
# BLUP4 <- predict.asreml(FA_3, classify='gen', sed = T)$pvals


BLUP7.1 <- BLUP7.1 %>% dplyr::select(1:2)
colnames(BLUP7.1)[2] <- "ST4_Yi"

BLUP4 <- BLUP4 %>% dplyr::select(1:2)
colnames(BLUP4)[2] <- "ST4_FD"

BLUP3 <- BLUP3 %>% dplyr::select(1:3) %>% spread(key = loc, value = predicted.value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
colnames(BLUP3)[2:3] <- c("ST3_FD_ID", "ST3_FD_WA")
BLUP6.1 <- BLUP6.1 %>% dplyr::select(1:3) %>% spread(key = loc, value = predicted.value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
colnames(BLUP6.1)[2:4] <- c("ST3_Yi_ID", "ST3_Yi_OR", "ST3_Yi_WA")


BLUP1 <- BLUP1 %>% dplyr::select(1:3) %>% spread(key = env, value = predicted.value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
colnames(BLUP1)[2:4] <- gsub("^", "ST1_FD_", colnames(BLUP1)[2:4])

BLUP5.1 <- BLUP5.1 %>% dplyr::select(1:3) %>% spread(key = env, value = predicted.value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
colnames(BLUP5.1)[2:32] <- gsub("^", "ST1_Yi_", colnames(BLUP5.1)[2:32])


T5 <- list()
T6 <- list()
T7 <- list()

for (i in 1:(length(T1))) {
  data <- T1[[i]]
  data <- as.data.frame(data)
  data[list_5] <- lapply(data[list_5], factor)
  data <- data[order(data$gen, data$env), ]
  data1 <- na.omit(data)
  
  FA_1 <- asreml::asreml(fixed = BLUE ~ 1 + gen + loc, 
                         random = ~ + env + facv(env, 1):id(gen),
                         data = data1, na.action = list(x = "include", y = "include"), 
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  BLUP5 <- predict.asreml(FA_1, classify='env:gen', sed = T)$pvals
  BLUP6 <- predict.asreml(FA_1, classify='loc:gen', sed = T)$pvals
  BLUP7 <- predict.asreml(FA_1, classify='gen', sed = T)$pvals
  T5[[length(T5)+1]] <- BLUP5
  T6[[length(T6)+1]] <- BLUP6
  T7[[length(T7)+1]] <- BLUP7
}
names(T7) <- c("ST4_MS", "ST4_DM", "ST4_He")
names(T6) <- c("ST3_MS", "ST3_DM", "ST3_He")
names(T5) <- c("ST1_MS", "ST1_DM", "ST1_He")


T7 <-rbindlist(T7, use.names=TRUE, fill=TRUE, idcol="env")
T7 <- T7 %>% dplyr::select(1:3) %>% spread(key = env, value = predicted.value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

head(T6[[1]])
T6.1 <- list()
for (i in 1:(length(T6))) {
  data <- T6[[i]]
  data <- data %>% dplyr::select(1:3) %>% spread(key = loc, value = predicted.value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
  T6.1[[length(T6.1)+1]] <- data
}
names(T6.1) <- c("ST3_MS", "ST3_DM", "ST3_He")

colnames(T6.1[[1]])[2:4] <- c("ST3_MS_ID", "ST3_MS_OR", "ST3_MS_WA")
colnames(T6.1[[2]])[2:4] <- c("ST3_DM_ID", "ST3_DM_OR", "ST3_DM_WA")
colnames(T6.1[[3]])[2:4] <- c("ST3_He_ID", "ST3_He_OR", "ST3_He_WA")

inner_join(T6.1[[1]], T6.1[[2]], by = "gen")

T6.1 <-rbindlist(T6.1, use.names=TRUE, fill=TRUE, idcol="env")

colnames() <- c("ST3_MS_ID", "ST3_MS_OR", "ST3_MS_WA")

T5.1 <- list()
for (i in 1:(length(T5))) {
  data <- T5[[i]]
  data <- data %>% dplyr::select(1:3) %>% spread(key = env, value = predicted.value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
  T5.1[[length(T5.1)+1]] <- data
}

colnames(T5.1[[1]])[2:14] <- gsub("^", "ST1_MS_", colnames(T5.1[[1]])[2:14])
colnames(T5.1[[2]])[2:9] <- gsub("^", "ST1_DM_", colnames(T5.1[[2]])[2:9])
colnames(T5.1[[3]])[2:23] <- gsub("^", "ST1_He_", colnames(T5.1[[3]])[2:23])

head(BLUP5.1)
head(BLUP1)
head(BLUP4)
head(BLUP3)
BLUP <- inner_join(T5.1[[1]], T5.1[[2]], by = "gen") %>% inner_join(., T5.1[[3]], by = "gen") %>% 
  inner_join(., BLUP5.1, by = "gen") %>% inner_join(., BLUP1, by = "gen") %>% 
  inner_join(., T6.1[[1]], by = "gen") %>% inner_join(., T6.1[[2]], by = "gen") %>% 
  inner_join(., T6.1[[3]], by = "gen") %>% inner_join(., BLUP6.1, by = "gen") %>% 
  inner_join(., BLUP3, by = "gen") %>% inner_join(., T7, by = "gen") %>% 
  inner_join(., BLUP7.1, by = "gen") %>% inner_join(., BLUP4, by = "gen") %>% 
  inner_join(., PCA, by = "gen") 
colnames(BLUP)

write.csv(BLUP, "~/Documents/Cesar/git/big_files/pheno_fa1.csv", row.names = F, quote = F)



BLUP2 <- preds2$pvals
BLUP3.1 <- preds3.1$pvals
BLUP3.2 <- preds3.2$pvals

sd(BLUP1$predicted.value)
sd(BLUP2$predicted.value)
sd(BLUP3.1$ST4_WA_Yi)

BLUP3.1 <- BLUP3.1 %>% dplyr::select(1:2)
colnames(BLUP3.1)[2] <- "ST4_WA_Yi"
BLUP3.2 <- BLUP1 %>% dplyr::select(1:2)
colnames(BLUP3.2)[2] <- "ST4_FA1_WA_Yi"

BLUP3.2 <- BLUP3.2 %>% dplyr::select(1:3) %>% spread(key = env, value = predicted.value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
BLUP <- inner_join(BLUP3.1, BLUP3.2, by = "id") %>% inner_join(., BLUP3.1, by = "id") %>% inner_join(., PCA, by = "id") 
BLUP <- inner_join(BLUP3.1, BLUP3.2, by = "id") %>% inner_join(., PCA, by = "id") 
write.csv(BLUP, "~/Documents/Cesar/git/big_files/pheno_fa1.csv", row.names = F, quote = F)

colnames(BLUP3.2)[2] <- "ST1_ID_2019_1"
BLUP3.2 <- BLUP3.2[,c(1,5)]
c(1,5)
colnames(Yield_BLUE1.1)[2] <- "ST0_ID_2019_1"
Yield_BLUE1.1 <- Yield_BLUE1[,c(1,5)]
c(1,5)

# preds1 <- predict.asreml(FA_1, classify='id', sed = T)
# BLUP1 <- preds1$pvals
preds2 <- predict.asreml(FA_2, classify='id', sed = T)

BLUP2 <- preds2$sed

BLUP1.1 <- BLUP1.1[,c(1,2)]
colnames(BLUP1.1) <- c("id", "FA_2")

BLUP1 <- BLUP1[,c(1,2)]
colnames(BLUP1) <- c("id", "FA1")
BLUP2 <- BLUP2[,c(1,2)]
colnames(BLUP2) <- c("id", "FA2")




Y1[2]
Y1 <- list(Yield_ID, Yield_OR, Yield_WA)
Y2 <- list()
for (i in 1:(length(Y1))) {
  FA_3 <- asreml::asreml(fixed = BLUE ~ 1 + id, 
                         random = ~ + fa(env, 1):id, 
                         data = Y1[i], na.action = list(x = "include", y = "include"), 
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  preds3 <- predict.asreml(FA_3, classify='id', sed = T)
  BLUP3 <- preds3$pvals
  Y2[[length(Y2)+1]] <- BLUP3
}
names(Y2) <- c("Yield_ID", "Yield_OR", "Yield_WA")
Y2 <-rbindlist(Y2, use.names=TRUE, fill=TRUE, idcol="env")
Y2 <- Y2 %>% dplyr::select(1:3) %>% spread(key = env, value = predicted.value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)


################

PCA <- read.csv("~/Documents/Cesar/git/big_files/pheno.csv")
colnames(PCA)
PCA <- PCA %>% dplyr::select(c(1,109:111))
str(PCA)

PCA$gen <- as.character(PCA$gen)
PCA$gen <- as.factor(PCA$gen)

colnames(PCA)[1] <- "id"
