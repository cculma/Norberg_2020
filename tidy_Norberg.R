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
