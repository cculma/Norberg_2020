# stagewise
rm(list = ls())
library(StageWise)
library(asreml)
library(data.table)
library(tidyverse)
library(asremlPlus)
directory <- "~/Documents/Cesar/git/Norberg_2020/BLUE_values"
P5 <- file.path(directory, "assay1.csv")


##########################
dim(P1)
head(P1)
colnames(P1)
P1.1 <- P1[,c(3,6,7,8,13,18,23)]
head(P1.1)
colnames(P1.1) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")

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

lev1 <- c("block", "id", "row", "col")

#################
# 1_MSC

ST1_1MSC <- list()
for (i in 1:length(data_ar1)) {
  data <- read.csv(data_ar1[i])
  data <- data[,c(3,6,4,7,8,11,16,21)]
  colnames(data) <- c("block", "id", "position", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data <- data[order(data$row, data$col), ]
  ST1_1MSC[[length(ST1_1MSC)+1]] = data
}

names(ST1_1MSC) <- list_1
ST1_1MSC <-rbindlist(ST1_1MSC, use.names=TRUE, fill=TRUE, idcol="env")

ST1_1MSC <- ST1_1MSC %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")
ST1_1MSC <- na.omit(ST1_1MSC)
head(ST1_1MSC)
str(ST1_1MSC)
ST1_1MSC$loc <- as.factor(ST1_1MSC$loc)
levels(ST1_1MSC$loc)
write.csv(ST1_1MSC, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/STAGEWISE/ST1_1MSC.csv", quote = F, row.names = F)

?blup_prep
?blup

effects <- data.frame(name=c("block","position", "cov1", "cov2"),
                      fixed=c(FALSE,TRUE,TRUE,TRUE),
                      factor=c(TRUE,FALSE,FALSE,FALSE))

effects

a1 <- file.path("~/Documents/Cesar/git/Norberg_2020/BLUE_values/STAGEWISE/ST1_1MSC.csv")

ans1a <- Stage1(filename=a1, 
                traits="resp",
                effects=effects, 
                solver="asreml")

rm(stage1.blue)
stage1.blue <- ans1a$blue
stage1.vcov <- ans1a$vcov

table(stage1.blue$loc)

g1 <- file.path("~/Documents/Cesar/git/big_files/Norberg_1.txt")
# geno <- read.csv(g1, check.names=F)
# geno[1:5,1:5]
# dim(geno) #  97316   195
geno <- read_geno(filename=g1, ploidy=4, map=TRUE, min.minor.allele=5)
class(geno)


ST1 <- stage1.blue %>% spread(key = env, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
colnames(ST1)[2:14] <- gsub("^", "ST1_MS_", colnames(ST1)[2:14])

str(ST1)
ST2 <- inner_join(ST1, pred.id, by = "id") %>% inner_join(., PCA, by = "id")

write.csv(ST2, "~/Documents/Cesar/git/big_files/pheno_fa2.csv", quote = F, row.names = F)

locs <- c("ID","OR","WA")
blues <- ans1a$blue[ans1a$blue$loc %in% locs,]
tmp <- sapply(strsplit(names(ans1a$vcov),split="_"),"[[",1)
vcov <- ans1a$vcov[tmp %in% locs]

ans2b <- Stage2(data = blues, 
                vcov = vcov, 
                geno = geno,  
                silent=FALSE)
summary(ans2b$vars)
ans2b$aic

ans2c <- Stage2(data=stage1.blue, 
                vcov=stage1.vcov, 
                geno=NULL, 
                silent=FALSE)
ans2c$aic

prep1 <- blup_prep(data=blues, vcov=vcov, geno=geno, vars=ans2b$vars, method = NULL)
prep2 <- blup_prep(data=blues, vcov=vcov, geno=NULL, vars=ans2c$vars, method = NULL)
prep3 <- blup_prep(data=blues, vcov=vcov, geno=geno, vars=ans2b$vars, method = "Vinv")

index.ID <- c(ID=1, OR=0, WA=0)  
index.OR <- c(ID=0, OR=1, WA=0)  
index.WA <- c(ID=0, OR=0, WA=1) 

pred.ID <- blup(data=prep1, geno=geno, index.weights=index.ID,what="id")
pred.OR <- blup(data=prep1, geno=geno, index.weights=index.OR,what="id")
pred.WA <- blup(data=prep1, geno=geno, index.weights=index.WA,what="id")


pred.01 <- blup(data=prep1, geno=geno, what = "id")
pred.02 <- blup(data=prep2, geno=NULL, what = "id")
pred.03 <- blup(data=prep3, geno=geno, what = "id")
hist(pred.01$BV.r2)

pred.ID <- pred.ID[1:2]
colnames(pred.ID)[2] <- "ST3_ID"
pred.OR <- pred.OR[1:2]
colnames(pred.OR)[2] <- "ST3_OR"
pred.WA <- pred.WA[1:2]
colnames(pred.WA)[2] <- "ST3_WA"
pred.al <- pred.al[1:2]
colnames(pred.al)[2] <- "ST4_overall"

ST3 <- inner_join(pred.ID, pred.OR, by = "id") %>% inner_join(., pred.WA, by = "id") %>% inner_join(., pred.al, by = "id") %>% inner_join(., PCA, by = "id") 

write.csv(ST3, "~/Documents/Cesar/git/big_files/pheno_MCS.csv", quote = F, row.names = F)

ans2c <- Stage2(data=stage1.blue, 
                vcov=stage1.vcov, 
                geno=NULL, 
                silent=FALSE)

mm7 = asreml(fixed = height ~ 1 + site , 
             random = ~ diag(site):block + fa(site,1):female,  
             rcov = ~ at(site):units, 
             data = ds, workspace=80e6 )

mm8 <- asreml(fixed = BLUE ~ 1, 
              random = ~ + fa(env, 1):id, 
              data = stage1.blue, 
              na.action = list(x = "include", y = "include"), 
              weights = stage1.vcov, 
              family = asreml::asr_gaussian(dispersion = 1))

head(stage1.blue)

mm7 <- update.asreml(mm7)
summary(mm7)$varcomp[2:5] 
info.crit.asreml(mm7)



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
sessionInfo()
