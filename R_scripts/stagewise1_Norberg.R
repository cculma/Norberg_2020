# stagewise
rm(list = ls())
library(StageWise)
library(asreml)
library(data.table)
library(tidyverse)
library(asremlPlus)

################

directory <- "~/Documents/git/Norberg_2020/BLUE_values"
P5 <- file.path(directory, "assay1.csv")

################
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
                       random = ~ + block, residual = ~sar(row):sar(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  preds <- predict(m2, classify='gen', vcov=TRUE)
  vcov1 <- preds$vcov
  blue <- preds$pvals
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  dimnames(vcov1) <- list(blue$id,blue$id)
  # Adding Weights
  blue$weight <- (1/blue$std.error)^2
  MSC_BLUE[[length(MSC_BLUE)+1]] = blue
  MSC_vcov[[length(MSC_vcov)+1]] = vcov1
}

names(MSC_BLUE) <- list_1
names(MSC_vcov) <- list_1
MSC_BLUE <-rbindlist(MSC_BLUE, use.names=TRUE, fill=TRUE, idcol="env")
MSC_BLUE1 <- MSC_BLUE %>% dplyr::filter(!gen %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = env, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
MSC_BLUE2 <- MSC_BLUE %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")


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
                       random = ~ + block, residual = ~sar(row):sar(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  preds <- predict(m2, classify='gen', vcov=TRUE)
  vcov1 <- preds$vcov
  blue <- preds$pvals
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  dimnames(vcov1) <- list(blue$id,blue$id)
  
  # Adding Weights
  blue$weight <- (1/blue$std.error)^2
  DM_BLUE[[length(DM_BLUE)+1]] = blue
  DM_vcov[[length(DM_vcov)+1]] = vcov1
}

names(DM_BLUE) <- list_2
names(DM_vcov) <- list_2

DM_BLUE <-rbindlist(DM_BLUE, use.names=TRUE, fill=TRUE, idcol="env")
DM_BLUE1 <- DM_BLUE %>% dplyr::filter(!gen %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = env, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
DM_BLUE2 <- DM_BLUE %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")


#################
# 3_Height
lev2 <- c("block", "gen", "position", "row", "col")
data <- read.csv(data_ar3[1])
data <- data[,c(3,6,4,7,8,13,18,23)]
colnames(data) <- c("block", "gen", "position", "row", "col", "resp", "cov1", "cov2")
data[,lev2] <- lapply(data[,lev2], factor)
data <- data[order(data$row, data$col), ]
head(data)

m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                     random = ~ + block, 
                     residual = ~ ar2(row):ar2(col),
                     data = data, 
                     na.action = list(x = "include", y = "include"))
infoCriteria.asreml(m2)

Height_BLUE <- list()
Height_vcov <- list()
for (i in 1:length(data_ar3)) {
  data <- read.csv(data_ar3[i])
  data <- data[,c(3,6,7,8,13,18,23)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~+block, residual = ~sar(row):sar(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  preds <- predict(m2, classify='gen', vcov=TRUE)
  vcov1 <- preds$vcov
  blue <- preds$pvals
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  dimnames(vcov1) <- list(blue$id,blue$id)
  
  # Adding Weights
  blue$weight <- (1/blue$std.error)^2
  Height_BLUE[[length(Height_BLUE)+1]] = blue
  Height_vcov[[length(Height_vcov)+1]] = vcov1
}

names(Height_BLUE) <- list_3
names(Height_vcov) <- list_3

Height_BLUE <-rbindlist(Height_BLUE, use.names=TRUE, fill=TRUE, idcol="env")
Height_BLUE1 <- Height_BLUE %>% dplyr::filter(!gen %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = env, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
Height_BLUE2 <- Height_BLUE %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")


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
                       random = ~ + block , residual = ~ sar(row):sar(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  preds <- predict.asreml(m2, classify='gen', vcov=TRUE)
  vcov1 <- preds$vcov
  blue <- preds$pvals
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  dimnames(vcov1) <- list(blue$id,blue$id)
  
  # Adding Weights
  blue$weight <- (1/blue$std.error)^2
  Yield_BLUE[[length(Yield_BLUE)+1]] = blue
  Yield_vcov[[length(Yield_vcov)+1]] = vcov1
}
names(Yield_BLUE) <- list_4
names(Yield_vcov) <- list_4

Yield_BLUE <-rbindlist(Yield_BLUE, use.names=TRUE, fill=TRUE, idcol="env")
Yield_BLUE1 <- Yield_BLUE %>% dplyr::filter(!gen %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = env, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
Yield_BLUE2 <- Yield_BLUE %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")

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
                       random = ~ + block, 
                       residual = ~sar(row):sar(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  preds <- predict.asreml(m2, classify='gen', vcov=TRUE)
  vcov1 <- preds$vcov
  blue <- preds$pvals
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  dimnames(vcov1) <- list(blue$id,blue$id)
  # Adding Weights
  blue$weight <- (1/blue$std.error)^2
  FD_BLUE[[length(FD_BLUE)+1]] = blue
  FD_vcov[[length(FD_vcov)+1]] = vcov1
}
names(FD_BLUE) <- list_5
names(FD_vcov) <- list_5


FD_BLUE <-rbindlist(FD_BLUE, use.names=TRUE, fill=TRUE, idcol="env")
FD_BLUE1 <- FD_BLUE %>% dplyr::filter(!gen %in% c(201, 202)) %>% dplyr::select(1:3) %>% spread(key = env, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
FD_BLUE2 <- FD_BLUE %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")



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
