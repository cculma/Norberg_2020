# tidy data to add rows and cols in yield (Y), plant height (PH) and fall dormancy (FD)
rm(list = ls())
library(tidyverse)

a1 <- read.csv("cols_rows1.csv")
a2 <- read.csv("Y_PH_FD.csv")

colnames(a1)
colnames(a2)
