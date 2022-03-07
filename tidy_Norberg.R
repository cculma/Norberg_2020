# tidy data to add rows and cols in yield (Y), plant height (PH) and fall dormancy (FD)
rm(list = ls())
library(tidyverse)

a1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/spatial_distribution/cols_rows1.csv", check.names = F)
a2 <- read.csv("Y_PH_FD.csv", check.names = F)

colnames(a1)
colnames(a2)

lev3 <- c("Location", "Block", "Position", "ID", "Treatment")
a1[,lev3] <- lapply(a1[,lev3], factor)

a3 <- inner_join(a1, a2, by = c("Location", "Block", "Position", "ID", "Treatment"))
str(a3)
lev1 <- append(colnames(a1), c("Year", "Cut"))
a3[,lev1] <- lapply(a3[,lev1], factor)
summary(a3)
colnames(a3)

b1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/original_data/Guojie_2020_MSC.csv")
b2 <- read.csv("~/Documents/Cesar/git/Norberg_2020/original_data/Guojie_2020_Yield.csv")
colnames(b1)
colnames(b2)
b3 <- right_join(b1, b2, by = c("Location", "Year", "Cut", "Block", "Position", "ID", "Treatment"))
lev2 <- c("Location", "Year", "Cut", "Block", "Position", "ID", "Treatment")
b3[,lev2] <- lapply(b3[,lev2], factor)
str(b3)

c1 <- inner_join(a1, b3, by = c("Location", "Block", "Position", "ID", "Treatment"))
str(c1)

write.csv(a3, "Y_PH_FD1.csv", row.names = F, quote = F)


str(a3)
N_list <- as.factor(c(a3$Location, a3$Year, a3$Cut))
length(N_list)
a4 <- a3 %>% unite("merged", c(Location, Year, Cut), sep = "_", remove = F)
a4$merged <- as.factor(a4$merged)
str(a4)
a5 <- split(a4, a4$merged)
names(a5)

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/")
for (i in names(a5)) {
  write.csv(a5[[i]],  paste0(i, '.csv'), row.names = F, quote = F)
}

c1 <- c1 %>% unite("merged", c(Location, Year, Cut), sep = "_", remove = F)
c1$merged <- as.factor(c1$merged)
str(c1)
c2 <- split(c1, c1$merged)
names(c2)
setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/")
for (i in names(c2)) {
  write.csv(c2[[i]],  paste0(i, '.csv'), row.names = F, quote = F)
}

