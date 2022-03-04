# this script wants to import all csv files from 1st stage analysis in Mr.Bean 
# using ASReml with augmented design and generate a single csv file to be imported
# in MET

rm(list = ls())
library(tidyverse)
library(data.table)

#################
# MSC

setwd("~/Documents/Cesar/git/Norberg_2020/mr_bean_blue1/BLUE1/1_MSC/")
data_MSC <- list.files(pattern = ".csv", full.names = T)
list_1 <- gsub(".csv", "", gsub("./", "", data_MSC))

MSC_1 <- list()
for (i in 1:length(data_MSC)) {
  data <- read.csv(data_MSC[i])
  data <- data[,c(1,2,4)]
  MSC_1[[length(MSC_1)+1]] = data
}
names(MSC_1) <- list_1
MSC_2 <-rbindlist(MSC_1, use.names=TRUE, fill=TRUE, idcol="merged")
write.csv(MSC_2, "~/Documents/Cesar/git/Norberg_2020/mr_bean_blue1/BLUE2/MSC_1stage.csv", row.names = F, quote = F)

#################
# DM

setwd("~/Documents/Cesar/git/Norberg_2020/mr_bean_blue1/BLUE1/2_DM/")
data_DM <- list.files(pattern = ".csv", full.names = T)
list_2 <- gsub(".csv", "", gsub("./", "", data_DM))

DM_1 <- list()
for (i in 1:length(data_DM)) {
  data <- read.csv(data_DM[i])
  data <- data[,c(1,2,4)]
  DM_1[[length(DM_1)+1]] = data
}
names(DM_1) <- list_2
DM_2 <-rbindlist(DM_1, use.names=TRUE, fill=TRUE, idcol="merged")
write.csv(DM_2, "~/Documents/Cesar/git/Norberg_2020/mr_bean_blue1/BLUE2/DM_1stage.csv", row.names = F, quote = F)

#################
# Height

setwd("~/Documents/Cesar/git/Norberg_2020/mr_bean_blue1/BLUE1/3_Height/")
data_Height <- list.files(pattern = ".csv", full.names = T)
list_3 <- gsub(".csv", "", gsub("./", "", data_Height))

Height_1 <- list()
for (i in 1:length(data_Height)) {
  data <- read.csv(data_Height[i])
  data <- data[,c(1,2,4)]
  Height_1[[length(Height_1)+1]] = data
}
names(Height_1) <- list_3
Height_2 <-rbindlist(Height_1, use.names=TRUE, fill=TRUE, idcol="merged")
write.csv(Height_1, "~/Documents/Cesar/git/Norberg_2020/mr_bean_blue1/BLUE2/Height_1stage.csv", row.names = F, quote = F)

#################
# Yield

setwd("~/Documents/Cesar/git/Norberg_2020/mr_bean_blue1/BLUE1/4_Yield/")
data_Yield <- list.files(pattern = ".csv", full.names = T)
list_4 <- gsub(".csv", "", gsub("./", "", data_Yield))

Yield_1 <- list()
for (i in 1:length(data_Yield)) {
  data <- read.csv(data_Yield[i])
  data <- data[,c(1,2,4)]
  Yield_1[[length(Yield_1)+1]] = data
}
names(Yield_1) <- list_4
Yield_2 <-rbindlist(Yield_1, use.names=TRUE, fill=TRUE, idcol="merged")
write.csv(Yield_2, "~/Documents/Cesar/git/Norberg_2020/mr_bean_blue1/BLUE2/Yield_1stage.csv", row.names = F, quote = F)

#################
# FD

setwd("~/Documents/Cesar/git/Norberg_2020/mr_bean_blue1/BLUE1/5_FD/")
data_FD <- list.files(pattern = ".csv", full.names = T)
list_2 <- gsub(".csv", "", gsub("./", "", data_FD))

FD_1 <- list()
for (i in 1:length(data_FD)) {
  data <- read.csv(data_FD[i])
  data <- data[,c(1,2,4)]
  FD_1[[length(FD_1)+1]] = data
}
names(FD_1) <- list_2
FD_2 <-rbindlist(FD_1, use.names=TRUE, fill=TRUE, idcol="merged")
write.csv(FD_2, "~/Documents/Cesar/git/Norberg_2020/mr_bean_blue1/BLUE2/FD_1stage.csv", row.names = F, quote = F)

