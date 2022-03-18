# stagewise
rm(list = ls())
library(StageWise)
library(asreml)
library(data.table)
library(tidyverse)

P1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/ID_2019_1.csv")

P1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/ID_2018_1.csv")
P2 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/ID_2018_2.csv")
P3 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/ID_2018_3.csv")

P5 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/assay1.csv")




head(P1)
P4 <- rbind(P1, P2, P3)
head(P4)
colnames(P4)[1] <- "env"
colnames(P4)[6] <- "id"
P5 <- P4[,c(1,6:8,3,4,14)]
head(P5)
str(P5)
write.csv(P5, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/assay1.csv", quote = F, row.names = F)
directory <- "~/Documents/Cesar/git/Norberg_2020/BLUE_values"
P5 <- file.path(directory, "assay1.csv")

P5 <- list()
for (i in 1:length(data_ar5)) {
  data <- read.csv(data_ar5[i])
  data <- data[,c(6,3,4,7,8,15,20,25)]
  colnames(data) <- c("id", "block", "position", "row", "col", "resp", "cov1", "cov2")
  P5[[length(P5)+1]] = data
}

names(P5) <- list_5
P5 <-rbindlist(P5, use.names=TRUE, fill=TRUE, idcol="env")
write.csv(P5, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/assay_FD.csv", quote = F, row.names = F)

directory <- "~/Documents/Cesar/git/Norberg_2020/BLUE_values"
P5 <- file.path(directory, "assay_FD.csv")


effects <- data.frame(name=c("block","position"),
                      fixed=c(FALSE,TRUE),
                      factor=c(TRUE,FALSE))
effects

ans2a <- Stage1(filename = P5, 
                traits="resp",
                effects = effects, solver="asreml")
stage1.vcov <- ans2a$vcov
stage1.blue <- ans2a$blue

directory1 <- "~/Documents/Cesar/git/big_files"
G1 <- file.path(directory1, "Norberg_1.txt")
geno1 <- read_geno(filename=G1, ploidy=4, map=TRUE, min.minor.allele=5)

ans2c <- Stage2(data=stage1.blue, vcov=stage1.vcov, geno=geno1, silent=FALSE)

prep <- blup_prep(data=stage1.blue,
                  vcov=stage1.vcov,
                  geno=geno1,
                  vars=ans2c$vars)

pred.id <- blup(prep, geno = geno1, what="id")
knitr::kable(head(pred.id),digits=2)

pred.id <- pred.id[,c(1,2)]
colnames(pred.id) <- c("gen", "ST4_FD_Overall")
f1 <- inner_join(pred.id, PCA, by = "gen") 
write.csv(f1, "~/Documents/Cesar/git/big_files/pheno_fa1.csv", quote = F, row.names = F)

pred.marker <- blup(data=prep, geno=geno1, what="marker", gwas.ncore = 32)
head(pred.marker)




stage1.1.blue <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/4_Yield_1stage.csv")
stage1.1.blue$merged <- as.factor(stage1.1.blue$merged)
levels(stage1.1.blue$merged)
stage1.2.blue <- stage1.1.blue %>% dplyr::filter(merged %in% c("BLUE_ID_2018_1", "BLUE_ID_2018_2", "BLUE_ID_2018_3"))
colnames(stage1.2.blue) <- c("env", "id", "BLUE", "weight")
stage1.2.blue <- stage1.2.blue[,-4]
stage1.2.blue$env <- gsub("BLUE_", "", stage1.2.blue$env)



head(FD_BLUE)
ans2c <- Stage2(data=stage1.blue, vcov=stage1.vcov, geno=geno1, silent=FALSE)
ans2c <- Stage2(data=FD_BLUE, vcov=stage1.vcov, geno=geno1, silent=FALSE)


prep <- blup_prep(data=FD_BLUE,
                  vcov=stage1.vcov,
                  geno = geno1,
                  vars=ans2c$vars)
pred2.id <- blup(prep, geno = geno1, what="id")


pred.marker <- blup(data=prep, geno=geno1, what="marker", gwas.ncore= 32)
# manhattan_plot(pred.marker,thresh=5.1,rotate.label=TRUE)


gwas_threshold(geno1, alpha=0.05, n.core= 32)
?gwas_threshold

 class(geno1)

pheno <- read.csv("~/Documents/Cesar/git/big_files/pheno.csv", row.names = 1)
head(pheno)
pheno <- pheno %>% rownames_to_column(var = "id")
colnames(pheno)
pheno <- pheno[,c(1,85)]

pheno <- inner_join(pheno, pred2.id, by = "id")
trait1 <- colnames(pheno)[1:(length(colnames(pheno))-3)]
trait1
cor(pheno$ST2_Yi_ID_2018, pheno$BV, use = "complete.obs")
plot(x = pheno$ST2_Yi_ID_2018, y = pheno$BV)
var(pheno$ST2_Yi_ID_2018, use = "complete.obs")
var(pheno$BV)

##########################
dim(P1)
head(P1)
colnames(P1)
P1.1 <- P1[,c(3,6,7,8,13,18,23)]
head(P1.1)
colnames(P1.1) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
# P1.1 <- P1.1 %>% dplyr::filter(!gen %in% c(201, 202))
str(P1.1)



# BLUP
m1 <- asreml::asreml(fixed = resp ~ 1 + cov1 + cov2, 
                     random = ~+block + gen, residual = ~ar1(row):ar1(col), 
                     data = P1.1, na.action = list(x = "include", y = "include"))
# BLUE
m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                     random = ~+block, residual = ~ar1(row):ar1(col), 
                     data = P1.1, na.action = list(x = "include", y = "include"))


summary(m2)$bic

preds <- predict(m2, classify='gen', vcov=TRUE)
vcov1 <- as.matrix(preds$vcov)

# Adding Weights
pvals <- preds$pvals
vcov <- as.matrix(preds$vcov)
sel <- matrix(1, ncol=1, nrow=length(pvals$predicted.value))
sel[is.na(pvals$predicted.value),] <- 0
vcov <- vcov[sel==1,sel==1]
pvals$weight[sel==1] <- diag(solve(vcov)) 

colnames(pvals)
str(pvals)
fa1 <- pvals[,c(1,2)]
fa1$gen <- as.character(fa1$gen)
cor(fa1$BLUE_He_ID_2019_1, pvals$predicted.value, use = "complete.obs")



m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                     random = ~+block, residual = ~ar1(row):ar1(col), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))


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

list_1 <- gsub(".csv", "", gsub("./", "", data_ar1))
list_2 <- gsub(".csv", "", gsub("./", "", data_ar2))
list_3 <- gsub(".csv", "", gsub("./", "", data_ar3))
list_4 <- gsub(".csv", "", gsub("./", "", data_ar4))
list_5 <- gsub(".csv", "", gsub("./", "", data_ar5))

lev1 <- c("block", "gen", "row", "col")
lev2 <- c("resp", "cov1", "cov2")

data <- read.csv(data_ar1[13])
head(data)
str(data)
#################
# 1_MSC

MSC_BLUE <- list()
MSC_vcov <- list()
for (i in 1:length(data_ar1)) {
  data <- read.csv(data_ar1[i])
  data <- data[,c(3,6,7,8,11,16,21)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~+block, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  preds <- predict(m2, classify='gen', vcov=TRUE)
  vcov1 <- as.matrix(preds$vcov)
  
  # Adding Weights
  pvals <- preds$pvals
  vcov <- as.matrix(preds$vcov)
  sel <- matrix(1, ncol=1, nrow=length(pvals$predicted.value))
  sel[is.na(pvals$predicted.value),] <- 0
  vcov <- vcov[sel==1,sel==1]
  pvals$weight[sel==1] <- diag(solve(vcov)) 
  #
  MSC_BLUE[[length(MSC_BLUE)+1]] = pvals
  MSC_vcov[[length(MSC_vcov)+1]] = vcov1
}

names(MSC_BLUE) <- list_1
names(MSC_vcov) <- list_1

MSC_BLUE <-rbindlist(MSC_BLUE, use.names=TRUE, fill=TRUE, idcol="env")
MSC_BLUE <- MSC_BLUE %>% dplyr::filter(!gen %in% c(201, 202))
# write.csv(MSC_BLUE, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/1_MSC_1stage.csv", row.names = F, quote = F)

#################
# 2_DM

DM_BLUE <- list()
DM_vcov <- list()
for (i in 1:length(data_ar2)) {
  data <- read.csv(data_ar2[i])
  data <- data[,c(3,6,7,8,12,17,22)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~+block, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  preds <- predict(m2, classify='gen', vcov=TRUE)
  vcov1 <- as.matrix(preds$vcov)
  
  # Adding Weights
  pvals <- preds$pvals
  vcov <- as.matrix(preds$vcov)
  sel <- matrix(1, ncol=1, nrow=length(pvals$predicted.value))
  sel[is.na(pvals$predicted.value),] <- 0
  vcov <- vcov[sel==1,sel==1]
  pvals$weight[sel==1] <- diag(solve(vcov)) 
  DM_BLUE[[length(DM_BLUE)+1]] = pvals
  DM_vcov[[length(DM_vcov)+1]] = vcov1
}

names(DM_BLUE) <- list_2
names(DM_vcov) <- list_2

DM_BLUE <-rbindlist(DM_BLUE, use.names=TRUE, fill=TRUE, idcol="env")
DM_BLUE <- DM_BLUE %>% dplyr::filter(!gen %in% c(201, 202))
# write.csv(DM_BLUE, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/2_DM_1stage.csv", row.names = F, quote = F)

#################
# 3_Height

Height_BLUE <- list()
Height_vcov <- list()
for (i in 1:length(data_ar3)) {
  data <- read.csv(data_ar3[i])
  data <- data[,c(3,6,7,8,13,18,23)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~+block, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  preds <- predict(m2, classify='gen', vcov=TRUE)
  vcov1 <- as.matrix(preds$vcov)
  
  # Adding Weights
  pvals <- preds$pvals
  vcov <- as.matrix(preds$vcov)
  sel <- matrix(1, ncol=1, nrow=length(pvals$predicted.value))
  sel[is.na(pvals$predicted.value),] <- 0
  vcov <- vcov[sel==1,sel==1]
  pvals$weight[sel==1] <- diag(solve(vcov)) 
  Height_BLUE[[length(Height_BLUE)+1]] = pvals
  Height_vcov[[length(Height_vcov)+1]] = vcov1
}

names(Height_BLUE) <- list_3
names(Height_vcov) <- list_3

Height_BLUE <-rbindlist(Height_BLUE, use.names=TRUE, fill=TRUE, idcol="env")
Height_BLUE <- Height_BLUE %>% dplyr::filter(!gen %in% c(201, 202))
# write.csv(Height_BLUE, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/3_Height_1stage.csv", row.names = F, quote = F)

#################
# 4_Yield

Yield_BLUE <- list()
Yield_vcov <- list()
for (i in 1:length(data_ar4)) {
  data <- read.csv(data_ar4[i])
  data <- data[,c(3,6,7,8,14,19,24)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~+block, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  preds <- predict(m2, classify='gen', vcov=TRUE)
  vcov1 <- as.matrix(preds$vcov)
  
  # Adding Weights
  pvals <- preds$pvals
  vcov <- as.matrix(preds$vcov)
  sel <- matrix(1, ncol=1, nrow=length(pvals$predicted.value))
  sel[is.na(pvals$predicted.value),] <- 0
  vcov <- vcov[sel==1,sel==1]
  pvals$weight[sel==1] <- diag(solve(vcov)) 
  Yield_BLUE[[length(Yield_BLUE)+1]] = pvals
  Yield_vcov[[length(Yield_vcov)+1]] = vcov1
}

names(Yield_BLUE) <- list_4
names(Yield_vcov) <- list_4

Yield_BLUE <-rbindlist(Yield_BLUE, use.names=TRUE, fill=TRUE, idcol="env")
Yield_BLUE <- Yield_BLUE %>% dplyr::filter(!gen %in% c(201, 202))
# write.csv(Yield_BLUE, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/4_Yield_1stage.csv", row.names = F, quote = F)

#################
# 5_FD

FD_BLUE <- list()
FD_vcov <- list()
for (i in 1:length(data_ar5)) {
  data <- read.csv(data_ar5[i])
  data <- data[,c(3,6,7,8,15,20,25)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~+block, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  preds <- predict(m2, classify='gen', vcov=TRUE)
  vcov1 <- as.matrix(preds$vcov)
  blue <- preds$pvals
  blue <- blue[,c(1,2)]
  colnames(blue) <- c("id", "BLUE")
  # Adding Weights
  pvals <- preds$pvals
  vcov <- as.matrix(preds$vcov)
  sel <- matrix(1, ncol=1, nrow=length(pvals$predicted.value))
  sel[is.na(pvals$predicted.value),] <- 0
  vcov <- vcov[sel==1,sel==1]
  pvals$weight[sel==1] <- diag(solve(vcov)) 
  FD_BLUE[[length(FD_BLUE)+1]] = blue
  FD_vcov[[length(FD_vcov)+1]] = vcov1
}

names(FD_BLUE) <- list_5
names(FD_vcov) <- list_5

FD_BLUE <-rbindlist(FD_BLUE, use.names=TRUE, fill=TRUE, idcol="env")

FD_BLUE <- FD_BLUE %>% dplyr::filter(!gen %in% c(201, 202))
# write.csv(FD_BLUE, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/5_FD_1stage.csv", row.names = F, quote = F)
vcov1 <- preds$vcov

stage1.vcov <- ans2a$vcov
stage1.blue <- ans2a$blue
colnames(FD_BLUE) <- c("env", "id", "BLUE", "std.error", "error", "weight")

ans2b <- Stage2(data = FD_BLUE, vcov = FD_vcov)

#################
# end
# save.image("~/Documents/Cesar/git/big_files/data_6.RData")