
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

lev1 <- c("block", "gen", "row", "col","check")

Yi_H2 <- list()
for (i in 1:length(data_ar4)) {
  data <- read.csv(data_ar4[i])
  data <- data[,c(3,6,7,8,14,19,24)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data <- data[order(data$row, data$col), ]
  data$check <- dplyr::recode_factor(data$gen, "201" = "control", "202" = "control", .default = "test")
  data[,lev1] <- lapply(data[,lev1], factor)
  m3 <- asreml::asreml(fixed = resp ~ 1 + at(check, "control"):gen, 
                       random = ~ block + at(check, "test"):gen, 
                       residual = ~ ar1(row):id(col),
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- update.asreml(m3)
  vg <- vpredict(m3, VG ~ V2)
  h2 <- vpredict(m3, h2 ~ V2 / (V1 + V2 + V3 + V4))
  H2 <- rbind(vg, h2)
  H2 <- H2 %>% rownames_to_column("component")
  
  Yi_H2[[length(Yi_H2)+1]] = H2
}  
names(Yi_H2) <- list_4

Yi_H2 <-rbindlist(Yi_H2, use.names=TRUE, fill=TRUE, idcol="env")

# MS = 1_MSC
MS_H2 <- list()
for (i in 1:length(data_ar1)) {
  data <- read.csv(data_ar1[i])
  data <- data[,c(3,6,7,8,11,16,21)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data <- data[order(data$row, data$col), ]
  data$check <- dplyr::recode_factor(data$gen, "201" = "control", "202" = "control", .default = "test")
  data[,lev1] <- lapply(data[,lev1], factor)
  m3 <- asreml::asreml(fixed = resp ~ 1 + at(check, "control"):gen, 
                       random = ~ block + at(check, "test"):gen, 
                       residual = ~ ar1(row):id(col),
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- update.asreml(m3)
  vg <- vpredict(m3, VG ~ V2)
  h2 <- vpredict(m3, h2 ~ V2 / (V1 + V2 + V3 + V4))
  H2 <- rbind(vg, h2)
  H2 <- H2 %>% rownames_to_column("component")

  MS_H2[[length(MS_H2)+1]] = H2
}
names(MS_H2) <- list_1
MS_H2 <-rbindlist(MS_H2, use.names=TRUE, fill=TRUE, idcol="env")

# DM = 2_DM
DM_H2 <- list()
for (i in 1:length(data_ar2)) {
  data <- read.csv(data_ar2[6])
  data <- data[,c(3,6,7,8,12,17,22)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data <- data[order(data$row, data$col), ]
  data$check <- dplyr::recode_factor(data$gen, "201" = "control", "202" = "control", .default = "test")
  data[,lev1] <- lapply(data[,lev1], factor)
  m3 <- asreml::asreml(fixed = resp ~ 1 + at(check, "control"):gen, 
                       random = ~ block + at(check, "test"):gen, 
                       residual = ~ ar1(row):id(col),
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- update.asreml(m3)
  vg <- vpredict(m3, VG ~ V2)
  h2 <- vpredict(m3, h2 ~ V2 / (V1 + V2 + V3 + V4))
  H2 <- rbind(vg, h2)
  H2 <- H2 %>% rownames_to_column("component")
  
  DM_H2[[length(DM_H2)+1]] = H2
}
names(DM_H2) <- list_2
DM_H2 <-rbindlist(DM_H2, use.names=TRUE, fill=TRUE, idcol="env")

# He = 3_Height
PH_H2 <- list()
for (i in 1:length(data_ar3)) {
  data <- read.csv(data_ar3[i])
  data <- data[,c(3,6,7,8,13,18,23)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data <- data[order(data$row, data$col), ]
  data$check <- dplyr::recode_factor(data$gen, "201" = "control", "202" = "control", .default = "test")
  data[,lev1] <- lapply(data[,lev1], factor)
  m3 <- asreml::asreml(fixed = resp ~ 1 + at(check, "control"):gen, 
                       random = ~ block + at(check, "test"):gen, 
                       residual = ~ ar1(row):id(col),
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- update.asreml(m3)
  vg <- vpredict(m3, VG ~ V2)
  h2 <- vpredict(m3, h2 ~ V2 / (V1 + V2 + V3 + V4))
  H2 <- rbind(vg, h2)
  H2 <- H2 %>% rownames_to_column("component")

  PH_H2[[length(PH_H2)+1]] = H2
}
names(PH_H2) <- list_3
PH_H2 <-rbindlist(PH_H2, use.names=TRUE, fill=TRUE, idcol="env")


# FD = 5_FD
FD_H2 <- list()
for (i in 1:length(data_ar5)) {
  data <- read.csv(data_ar5[i])
  data <- data[,c(3,6,7,8,15,20,25)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data <- data[order(data$row, data$col), ]
  data$check <- dplyr::recode_factor(data$gen, "201" = "control", "202" = "control", .default = "test")
  data[,lev1] <- lapply(data[,lev1], factor)
  m3 <- asreml::asreml(fixed = resp ~ 1 + at(check, "control"):gen, 
                       random = ~ block + at(check, "test"):gen, 
                       residual = ~ ar1(row):id(col),
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- update.asreml(m3)
  vg <- vpredict(m3, VG ~ V2)
  h2 <- vpredict(m3, h2 ~ V2 / (V1 + V2 + V3 + V4))
  H2 <- rbind(vg, h2)
  H2 <- H2 %>% rownames_to_column("component")
  
  FD_H2[[length(FD_H2)+1]] = H2
}

names(FD_H2) <- list_5
FD_H2 <-rbindlist(FD_H2, use.names=TRUE, fill=TRUE, idcol="env")


write.csv(Yi_H2, "~/Documents/git/Norberg_2020/BLUE_values/H2_Yi.csv")
write.csv(MS_H2, "~/Documents/git/Norberg_2020/BLUE_values/H2_MS.csv")
write.csv(DM_H2, "~/Documents/git/Norberg_2020/BLUE_values/H2_DM.csv")
write.csv(PH_H2, "~/Documents/git/Norberg_2020/BLUE_values/H2_PH.csv")
write.csv(FD_H2, "~/Documents/git/Norberg_2020/BLUE_values/H2_FD.csv")


# total_Yi ----------------------------------------------------------------

# go to R script sum_yield.R

lev1 <- c("block", "gen", "row", "col","check")
names(Y1)
str(data)
head(Y1[[1]])

YiT_H2 <- list()
for (i in 1:length(Y1)) {
  data <- Y1[[i]]
  data <- data[order(data$row, data$col), ]
  data$check <- dplyr::recode_factor(data$gen, "201" = "control", "202" = "control", .default = "test")
  data[,lev1] <- lapply(data[,lev1], factor)
  m3 <- asreml::asreml(fixed = resp ~ 1 + at(check, "control"):gen, 
                       random = ~ block + at(check, "test"):gen, 
                       residual = ~ ar1(row):id(col),
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- update.asreml(m3)
  vg <- vpredict(m3, VG ~ V2)
  h2 <- vpredict(m3, h2 ~ V2 / (V1 + V2 + V3 + V4))
  H2 <- rbind(vg, h2)
  H2 <- H2 %>% rownames_to_column("component")
  
  YiT_H2[[length(YiT_H2)+1]] = H2
  
}

names(YiT_H2) <- names(Y1)
YiT_H2 <-rbindlist(YiT_H2, use.names=TRUE, fill=TRUE, idcol="env")

write.csv(YiT_H2, "~/Documents/git/Norberg_2020/BLUE_values/H2_YiT.csv", row.names = F)


# H2_table ----------------------------------------------------------------

a2 <- read.csv("~/Documents/git/Norberg_2020/BLUE_values/H2_all.csv")
a3 <- a2 %>% dplyr::select(-3) %>% spread(key = Trait, value = H2)
a4 <- a2 %>% dplyr::select(-4) %>% spread(key = Trait, value = VG)

write.csv(a3, "~/Documents/git/Norberg_2020/BLUE_values/H2_all1.csv")
write.csv(a4, "~/Documents/git/Norberg_2020/BLUE_values/VG_all1.csv")

# low_variance ------------------------------------------------------------
# DM WA_2018_1 problems
data_ar2

data <- read.csv(data_ar2[6])
data <- data[,c(3,6,7,8,12,17,22)]
colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
data <- data[order(data$row, data$col), ]
data$check <- dplyr::recode_factor(data$gen, "201" = "201", "202" = "202", .default = "test")
data1[,lev1] <- lapply(data1[,lev1], factor)


head(data)

ggplot(data, aes(x=check, y=resp)) + geom_boxplot()

data1 <- read.csv(data_ar2[8])
data1 <- data1[,c(3,6,7,8,12,17,22)]
colnames(data1) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
data1 <- data1[order(data1$row, data1$col), ]
data1$check <- dplyr::recode_factor(data1$gen, "201" = "201", "202" = "202", .default = "test")
data1[,lev1] <- lapply(data1[,lev1], factor)


ggplot(data1, aes(x=check, y=resp)) + geom_boxplot()

summary(data$resp)
summary(data1$resp)


library(metan)
find_outliers(data$resp, plots = F)
find_outliers(data1$resp, plots = F)
R_Yi[[1]][c(27,39,107,131,148,152,156,193,209), 6] <- NA
R_Yi[[1]]$resp <- as.numeric(R_Yi[[1]]$resp)
