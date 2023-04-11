library(caret)
library(asreml)
library(data.table)
library(tidyverse)
library(asremlPlus)

# import_data -------------------------------------------------------------

# 1 stage results
# MS = 1_MSC
# DM = 2_DM
# He = 3_Height
# Yi = 4_Yield
# FD = 5_FD

# model in for loop
setwd("~/Documents/git/Norberg_2020/BLUE_values/split_data/")
data_ar <- list.files(pattern = ".csv", full.names = T)
data_ar4 <- data_ar # 4_Yield
list_4 <- gsub(".csv", "", gsub("./", "", data_ar4))

Y2 <- list()
for (i in 1:length(data_ar4)) {
  data <- read.csv(data_ar4[i])
  data <- data[,c(3,6,7,8,14,19,24)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,colnames(data)[1:4]] <- lapply(data[,colnames(data)[1:4]], factor)
  data <- data[order(data$row, data$col), ]
  Y2[[length(Y2)+1]] = data
  
}
names(Y2) <- list_4

names(Y2)
Y3 <- Y2[-c(26,31)] # remove "WA_2019_5" and "WA_2020_5"
Y3 <-rbindlist(Y3, use.names=TRUE, fill=TRUE, idcol="Site")

Y3 <- Y3 %>% separate(1, c("loc", "year_cut"), remove = T,  extra = "merge")
Y3[,c("loc", "year_cut")] <- lapply(Y3[,c("loc", "year_cut")], factor)
Y3$cut_month <- recode_factor(Y3$year_cut,
                             "2018_1" = 1,"2018_2" = 2,
                             "2018_3" = 3,"2019_1" = 12,
                             "2019_2" = 13,"2019_3" = 14,
                             "2019_4" = 15,"2020_1" = 24,
                             "2020_2" = 25,"2020_3" = 26,
                             "2020_4" = 27)
levels(Y3$year_cut)
Y3 <- Y3 %>% separate(2, c("year", "cut"), remove = F,  extra = "merge")
str(Y3)
Y3$cut_month <- as.numeric(as.character(Y3$cut_month))

# Legendre
tmin=min(Y3$cut_month)
tmax=max(Y3$cut_month)
Y3$qi= (2*(Y3$cut_month-tmin)/(tmax-tmin))-1

# Center 
Y3$ddayc <- scale(Y3$cut_month, scale=FALSE)
Y3$ddayc_sq <- (Y3$ddayc)^2
lev1 <- c("year", "cut", "year_cut", "cut_month")
class(Y3)
Y3 <- as.data.frame(Y3)
Y3[,lev1] <- lapply(Y3[,lev1], factor)

data <- Y3
# data <- data %>% dplyr::filter(loc == "WA")


data <- data[order(data$loc, data$year_cut, data$col, data$row),]
str(data)


head(data)

m2 <- asreml::asreml(fixed = resp ~ 1 + ddayc + ddayc_sq, 
                     random = ~ gen + block + gen:ddayc, 
                     residual = ~ dsum(~ar1(col):ar1(row) | year_cut), 
                     data = data,
                     na.action = list(x = "include", y = "include"))
plot(m2)
summary(m2)$varcomp
wald.asreml(m2, denDF = 'algebraic')$wald

nlevels(data$loc)

m3 <- asreml::asreml(fixed = resp ~ 1 + ddayc + ddayc_sq, 
                     random = ~gen + block + gen:ddayc + ~diag(year_cut):id(gen), 
                     residual = ~ dsum(~ar1(col):ar1(row) | year_cut),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m4 <- asreml::asreml(fixed = resp ~ loc + loc:ddayc,
                     random = ~ block + fa(loc, 1):id(gen) + loc:ddayc:gen,
                     residual = ~ dsum(~ante(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m5 <- asreml::asreml(fixed = resp ~ gen,
                     random = ~ block + fa(loc, 1):id(gen) + loc:ddayc:gen + diag(loc):spl(ddayc),
                     residual = ~ dsum(~ante(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

plot(m4)
summary(m4)$varcomp
summary(m4)$aic
summary(m5)$aic


m5 <- asreml::asreml(fixed = resp ~ 1 + loc,
                     random = ~fa(loc, 1):gen + fa(year_cut, 1):gen + gen:loc:block,
                     residual = ~ dsum(~corgh(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m6 <- asreml::asreml(fixed = resp ~ 1 + loc + loc:block,
                     random = ~fa(loc, 2):id(gen),
                     residual = ~ dsum(~corgh(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m7 <- asreml::asreml(fixed = resp ~ 1 + loc + loc:block,
                     random = ~us(loc):id(gen),
                     residual = ~ dsum(~ante(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

infoCriteria.asreml(m3)

infoCriteria.asreml(m4)
infoCriteria.asreml(m5)

infoCriteria.asreml(m6)
infoCriteria.asreml(m7)

REMLRT(m4, m5)
lrt(m4, m5)

preds <- predict(m5, classify = "gen:year_cut", vcov = F)
preds$pvals
