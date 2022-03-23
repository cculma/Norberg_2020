# stagewise
rm(list = ls())
library(StageWise)
library(asreml)
library(data.table)
library(tidyverse)

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
  vcov1 <- preds$vcov
  blue <- preds$pvals
  blue <- blue[,c(1,2)]
  colnames(blue) <- c("id", "BLUE")
  dimnames(vcov1) <- list(blue$id,blue$id)
  
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
  vcov1 <- preds$vcov
  blue <- preds$pvals
  blue <- blue[,c(1,2)]
  colnames(blue) <- c("id", "BLUE")
  dimnames(vcov1) <- list(blue$id,blue$id)
  
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
  vcov1 <- preds$vcov
  blue <- preds$pvals
  blue <- blue[,c(1,2)]
  colnames(blue) <- c("id", "BLUE")
  dimnames(vcov1) <- list(blue$id,blue$id)
  
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
  
  preds <- predict.asreml(m2, classify='gen', vcov=TRUE)
  vcov1 <- preds$vcov
  blue <- preds$pvals
  colnames(blue) <- c("id", "BLUE", "std.error", "status")
  dimnames(vcov1) <- list(blue$id,blue$id)
  # Adding Weights
  blue$weight <- (1/blue$std.error)^2
  Yield_BLUE[[length(Yield_BLUE)+1]] = blue
  Yield_vcov[[length(Yield_vcov)+1]] = vcov1
}

names(Yield_BLUE) <- list_4
names(Yield_vcov) <- list_4

Yield_BLUE <-rbindlist(Yield_BLUE, use.names=TRUE, fill=TRUE, idcol="env")
Yield_BLUE1 <- Yield_BLUE %>% dplyr::filter(!id %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = env, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

Yield_BLUE1 <- inner_join(Yield_BLUE1, PCA, by = "id")
write.csv(Yield_BLUE1, "~/Documents/Cesar/git/big_files/pheno_fa1.csv", row.names = F, quote = F)

Yield_BLUE2 <- Yield_BLUE %>% dplyr::filter(!id %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")



ans2b <- Stage2(data = Yield_BLUE, vcov = Yield_vcov, geno=geno1, silent=FALSE)
summary(ans2b$vars)
prep <- blup_prep(data = Yield_BLUE, vcov = Yield_vcov, geno=geno1, vars=ans2b$vars)
pred.id <- blup(prep, geno = geno1, what="id")

#################
# 5_FD

FD_BLUE <- list()
FD_vcov <- list()
for (i in 1:length(data_ar5)) {
  data <- read.csv(data_ar5[i])
  data <- data[,c(3,6,7,8,15,20,25)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
#  data <- data %>% dplyr::filter(!gen %in% c("201","202"))
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~+block, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  preds <- predict.asreml(m2, classify='gen', vcov=TRUE)
  vcov1 <- preds$vcov
  blue <- preds$pvals
  colnames(blue) <- c("id", "BLUE", "std.error", "status")
  dimnames(vcov1) <- list(blue$id,blue$id)
  # Adding Weights
  blue$weight <- (1/blue$std.error)^2
  FD_BLUE[[length(FD_BLUE)+1]] = blue
  FD_vcov[[length(FD_vcov)+1]] = vcov1
}
names(FD_BLUE) <- list_5
names(FD_vcov) <- list_5

FD_BLUE <-rbindlist(FD_BLUE, use.names=TRUE, fill=TRUE, idcol="env")
FD_BLUE <- FD_BLUE %>% dplyr::filter(!id %in% c(201, 202)) %>% separate(1, c("loc", "other"), sep = "_", remove = F, convert = FALSE, extra = "merge") %>% dplyr::select(-3) 


# write.csv(FD_BLUE, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/5_FD_1stage.csv", row.names = F, quote = F)


ans2b <- Stage2(data = FD_BLUE, vcov = FD_vcov, geno=geno1, silent=FALSE)
summary(ans2b$vars)
prep <- blup_prep(data = FD_BLUE, vcov = FD_vcov, geno=geno1, vars=ans2b$vars)
pred.id <- blup(prep, geno = geno1, what="id")

plot(hclust(as.dist(1-summary(ans2b$vars)[[2]])),
     hang = -1,xlab="")


colnames(pred.id)[1] <- "gen"
pheno <- read.csv("~/Documents/Cesar/git/big_files/pheno_fa1.csv")
pheno$gen <- as.character(pheno$gen)
pred.id <- inner_join(pheno, pred.id, by = "gen")
cor(pred.id$ST4_FD, pred.id$BV)


#################
library(metan)
# remove ourliers
colnames(Yield_BLUE)
str(Yield_BLUE)
lev3 <- c("env", "id")
Yield_BLUE$env <- as.factor(Yield_BLUE$env)
Yield_BLUE$id <- as.factor(Yield_BLUE$id)
find_outliers(Yield_BLUE$BLUE, plots = F)
find_outliers(Yield_BLUE2$BLUE, plots = F)
find_outliers(Yield_BLUE3$BLUE, plots = F)
find_outliers(Yield_BLUE4$BLUE, plots = F)

row1 <- c(324,601,611,616,620,640,644,648,651,660,672,678,684,691,694,707,708,709,710,730,732,738,739,748,750,752,759,760,761,765,770,772,773,774,775,778,782,784,786,789,792,793,795,797,2042,2070,2131,2183,2186,2802,2809,2837,2889,2940,2945,2963,2969,2973,2975,2977,2990,2998,4265,4394,5307,5310,5353,5355,5359,5360,5365,5374,5376,5387,5389,5390,5393,5394,5396,6037)
row2 <- c(600,626,631,648,706,708,709,710,723,724,729,751,2781,2800,2831,2840,2885,2911,2927,5247,5274,5312)
row3 <- c(655,2089,2889,5273,5277)
Yield_BLUE2 <- removeRows(row1, Yield_BLUE)
Yield_BLUE3 <- removeRows(row2, Yield_BLUE2)
Yield_BLUE4 <- removeRows(row3, Yield_BLUE3)

boxplot(Yield_BLUE4$BLUE)
summary(Yield_BLUE4$BLUE)

# MSC_BLUE
colnames(MSC_BLUE)
MSC_BLUE$env <- as.factor(MSC_BLUE$env)
MSC_BLUE$id <- as.factor(MSC_BLUE$id)
find_outliers(MSC_BLUE$BLUE, plots = F)
find_outliers(MSC_BLUE2$BLUE, plots = F)
row1 <- c(45,63,2429)
MSC_BLUE2 <- removeRows(row1, MSC_BLUE)

# DM_BLUE
find_outliers(DM_BLUE$BLUE, plots = F)
find_outliers(DM_BLUE2$BLUE, plots = F)
find_outliers(DM_BLUE3$BLUE, plots = F)
row1 <- c(241,355,481,492,866,1005,1205,1208,1221,1279,1324,1392)
row2 <- c(1326)
DM_BLUE2 <- removeRows(row1, DM_BLUE)
DM_BLUE3 <- removeRows(row2, DM_BLUE2)

# Height_BLUE
find_outliers(Height_BLUE$BLUE, plots = F)
find_outliers(Height_BLUE2$BLUE, plots = F)
row1 <- c(606,656,672,706,731,762,3028,3594)
Height_BLUE2 <- removeRows(row1, Height_BLUE)

# FD_BLUE
find_outliers(FD_BLUE$BLUE, plots = F)
find_outliers(FD_BLUE2$BLUE, plots = F)
find_outliers(FD_BLUE3$BLUE, plots = F)
row1 <- c(166,225,234,253,261,304,307,310,346,356,358,366,368,377,389,510,566,568)
row2 <- c(167,570)
FD_BLUE2 <- removeRows(row1, FD_BLUE)
FD_BLUE3 <- removeRows(row2, FD_BLUE2)


##############
removeRows <- function(rowNum, data) {
  newData <- data[-rowNum, , drop = FALSE]
  rownames(newData) <- NULL
  newData
}
##############
Yield_tidy <- list()
for (i in 1:length(data_ar4)) {
  data <- read.csv(data_ar4[i])
  data <- data[,c(3,6,7,8,14,19,24)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
  Yield_tidy[[length(Yield_tidy)+1]] <- data
}

names(Yield_tidy) <- list_4

Yield_tidy <-rbindlist(Yield_tidy, use.names=TRUE, fill=TRUE, idcol="env")

find_outliers(Yield_tidy$resp, plots = F)
row1 <- c(675,676,680,682,685,686,689,690,691,696,701,703,704,705,706,713,714,716,735,742,744,770,784,789,796,800,816,822,824,838,839,840,848,850,855,856,864,866,871,2238,2244,2328,2334,3094,3104,3123,3124,3127,3135,3137,3167,3179,3185,3279,5726,5767,5773,5787,5801,5803,5815,5821,5865,5881,5897,5929)
row2 <- c(734,758,806,808,823,858,1060,2242,2246,2276,2411,3126,3175,3245,5761,5779,5837,5913)
row3 <- c(5843,5854)
Yield_tidy$resp[row1] <- NA
Yield_tidy$resp[row2] <- NA
Yield_tidy$resp[row3] <- NA

Yield_tidy

str(Yield_tidy)
Yield_tidy$env <- as.factor(Yield_tidy$env)
str(a4)
Yield_tidy1 <- split(Yield_tidy, Yield_tidy$env)

BLUE_Yi <- list()
VCOV_Yi <- list()
for (i in 1:length(Yield_tidy1)) {
  data <-Yield_tidy1[[i]]
  data <- data[order(data$row, data$col), ]
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~+block, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  preds <- predict.asreml(m2, classify='gen', vcov=TRUE)
  vcov1 <- preds$vcov
  blue <- preds$pvals
  colnames(blue) <- c("id", "BLUE", "std.error", "status")
  dimnames(vcov1) <- list(blue$id,blue$id)
  # Adding Weights
  blue$weight <- (1/blue$std.error)^2
  BLUE_Yi[[length(BLUE_Yi)+1]] = blue
  VCOV_Yi[[length(VCOV_Yi)+1]] = vcov1
}
names(BLUE_Yi) <- names(Yield_tidy1)
names(VCOV_Yi) <- names(Yield_tidy1)

BLUE_Yi <-rbindlist(BLUE_Yi, use.names=TRUE, fill=TRUE, idcol="env")
BLUE_Yi1 <- BLUE_Yi %>% dplyr::filter(!id %in% c(201, 202)) %>% separate(1, c("loc", "other"), sep = "_", remove = F, convert = FALSE, extra = "merge") %>% dplyr::select(-3) 

BLUE_Yi1 <- BLUE_Yi %>% dplyr::filter(!id %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = env, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

str(PCA)
str(BLUE_Yi1)
boxplot(BLUE_Yi$BLUE)
boxplot(Yield_tidy$resp)
colnames(PCA)[1] <- "id"
BLUE_Yi1 <- inner_join(BLUE_Yi1, PCA, by = "id")

write.csv(BLUE_Yi1, "~/Documents/Cesar/git/big_files/pheno_fa2.csv", quote = F, row.names = F)

##############
# end
# save.image("~/Documents/Cesar/git/big_files/data_6.RData")
# load("~/Documents/Cesar/git/big_files/data_6.RData")
