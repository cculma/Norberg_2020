
library(tidyverse)
library(data.table)
library(goeveg)
library(ggcorrplot)
library(metan)
library(asreml)
library(asremlPlus)

# model in for loop
setwd("~/Documents/git/Norberg_2020/BLUE_values/split_data/")
data_ar <- list.files(pattern = ".csv", full.names = T)
data_ar1 <- data_ar[c(1,4,8,11,15,19,22,23,27:31)] # 1_MSC
data_ar2 <- data_ar[c(1,4,8,11,15,19,22,27)] # 2_DM
# data_ar3 <- data_ar[c(4,8,10:14,16:19,21:31)] # 3_Height
data_ar3 <- data_ar[c(4,8,11,12,14,16,17,19,21:31)] # 3_Height removing OR FD
data_ar4 <- data_ar # 4_Yield
data_ar5 <- data_ar[c(7,21,26)] # 5_FD

list_1 <- gsub(".csv", "", gsub("./", "", data_ar1))
list_2 <- gsub(".csv", "", gsub("./", "", data_ar2))
list_3 <- gsub(".csv", "", gsub("./", "", data_ar3))
list_4 <- gsub(".csv", "", gsub("./", "", data_ar4))
list_5 <- gsub(".csv", "", gsub("./", "", data_ar5))

# Yi = 4_Yield
R_Yi <- list()
for (i in 1:length(data_ar4)) {
  data <- read.csv(data_ar4[i])
  data <- data[,c(3,5,6,7,8,14,19,24)]
  colnames(data) <- c("block", "ID", "gen", "row", "col", "resp", "cov1", "cov2")
  R_Yi[[length(R_Yi)+1]] = data
}
names(R_Yi) <- list_4
head(R_Yi[[1]])

R_Yi <-rbindlist(R_Yi, use.names=TRUE, fill=TRUE, idcol="env")
head(R_Yi)

a4 <- R_Yi %>% dplyr::filter(!gen %in% c(201, 202)) %>% select(c(1,4,7))
a5 <- R_Yi %>% dplyr::filter(gen %in% c(201, 202)) %>% unite("gen", c(gen, block), sep = "_", remove = T) %>% select(c(1,2,6))

a6 <- rbind(a4, a5) %>% spread(key = env, value = resp) %>% remove_rownames() %>% column_to_rownames("gen")

P00 <- cor(a6, use = "complete.obs")

ggcorrplot(P00[,ncol(P00):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + theme_classic(base_family = "Arial", base_size = 12) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank()) + labs(title = "raw data")


#################
# MetaN to find outliers

names(R_Yi)

find_outliers(R_Yi[[1]]$resp, plots = F)
R_Yi[[1]][c(27,39,107,131,148,152,156,193,209), 6] <- NA
R_Yi[[1]]$resp <- as.numeric(R_Yi[[1]]$resp)

find_outliers(R_Yi[[8]]$resp, plots = F)
R_Yi[[8]][c(50,54,82,139,218), 6] <- NA
R_Yi[[8]]$resp <- as.numeric(R_Yi[[8]]$resp)

find_outliers(R_Yi[[9]]$resp, plots = F)
R_Yi[[9]][c(55, 124, 166 ), 6] <- NA
R_Yi[[9]]$resp <- as.numeric(R_Yi[[9]]$resp)

find_outliers(R_Yi[[15]]$resp, plots = F)
R_Yi[[15]][c(23, 44, 58, 159, 106, 132, 141), 6] <- NA
R_Yi[[15]]$resp <- as.numeric(R_Yi[[15]]$resp)

find_outliers(R_Yi[[11]]$resp, plots = F)
R_Yi[[11]][c(26, 37, 43, 127, 132, 133, 41, 45, 212), 6] <- NA
R_Yi[[11]]$resp <- as.numeric(R_Yi[[11]]$resp)

###############


for (i in 1:length(R_Yi)) {
  out1 <- as.data.frame(find_outliers(R_Yi[[3]]$resp, plots = F))
}

R_Yi[[1]]$resp <- as.numeric(R_Yi[[1]]$resp)

removeRows <- function(rowNum, data) {
  newData <- data[-rowNum, , drop = FALSE]
  rownames(newData) <- NULL
  newData
}

row1 <- c(27,39,107,131,148,152,156,193,209)
Yi_ID_2018_1 <- removeRows(row1, R_Yi[[1]])

find_outliers(R_Yi$resp, plots = T)
R_Yi[c(673,674,677,683,687,688,690,691,692,693,701,702,704,707,708,713,715,716,734,741,743,771,781,792,793,797,813,821,823,837,838,839,845,851,853,854,861,867,870,2237,2243,2327,2333,3093,3103,3123,3124,3128,3136,3138,3168,3180,3186,3280,5727,5766,5776,5786,5802,5804,5814,5824,5868,5884,5900,5932,735,759,805,807,822,859,1057,2241,2245,2275,2412,3125,3176,3246,5764,5778,5840,5916,5842,5855), 7] <- NA
R_Yi$resp <- as.numeric(R_Yi$resp)

R_Yi <- split(R_Yi[,-1], R_Yi$env)

