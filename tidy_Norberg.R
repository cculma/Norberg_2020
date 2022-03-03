# tidy data to add rows and cols in yield (Y), plant height (PH) and fall dormancy (FD)
rm(list = ls())
library(tidyverse)

a1 <- read.csv("cols_rows1.csv", check.names = F)
a2 <- read.csv("Y_PH_FD.csv", check.names = F)

colnames(a1)
colnames(a2)

a3 <- inner_join(a1, a2, by = c("Location", "Block", "Position", "ID", "Treatment"))
str(a3)
lev1 <- append(colnames(a1), c("Year", "Cut"))
a3[,lev1] <- lapply(a3[,lev1], factor)
summary(a3)

write.csv(a3, "Y_PH_FD1.csv", row.names = F, quote = F)


str(a3)
N_list <- as.factor(c(a3$Location, a3$Year, a3$Cut))
length(N_list)
a4 <- a3 %>% unite("merged", c(Location, Year, Cut), sep = "_", remove = F)
a4$merged <- as.factor(a4$merged)
str(a4)
a5 <- split(a4, a4$merged)
names(a5)

setwd("~/Documents/Cesar/git/Norberg_2020/mr_blue1/")
for (i in names(a5)) {
  write.csv(a5[[i]],  paste0(i, '.csv'), row.names = F, quote = F)
}

