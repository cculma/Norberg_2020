# tidy data to add rows and cols in yield (Y), plant height (PH) and fall dormancy (FD)
rm(list = ls())
library(tidyverse)

a1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/spatial_distribution/cols_rows1.csv", check.names = F)
a1 <- read.csv("~/Documents/git/Norberg_2020/spatial_distribution/cols_rows1.csv", check.names = F)

a2 <- read.csv("~/Documents/Cesar/git/Norberg_2020/Y_PH_FD.csv", check.names = F)

head(a1)
head(a2)
colnames(a1)
colnames(a2)

lev3 <- c("Location", "Block", "Position", "ID", "Treatment")
a1[,lev3] <- lapply(a1[,lev3], factor)
a2[,lev3] <- lapply(a2[,lev3], factor)
str(a1)

a3 <- inner_join(a1, a2, by = c("Location", "Block", "Position", "ID", "Treatment"))
str(a3)
lev1 <- append(colnames(a1), c("Year", "Cut"))
a3[,lev1] <- lapply(a3[,lev1], factor)
summary(a3)
colnames(a3)
str(a3)
b1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/original_data/Guojie_2020.csv")
b1 <- read.csv("~/Documents/git/Norberg_2020/original_data/Guojie_2Years_Yield_Height_FD.csv", check.names = F)

colnames(b1)
head(b1)
b1 <- b1[,c(1:12)]
lev2 <- c("Location", "Year", "Cut", "Block", "Position", "ID", "Treatment")
b1[,lev2] <- lapply(b1[,lev2], factor)
str(b1)
head(b1)

b1.1 <- b1 %>% dplyr::filter(Treatment %in% c(201))
head(b1.1)
colnames(b1.1) <- c("Location", "Year", "Cut", "Block", "Position", "ID", "Treatment", "MSC_201", "DM_201", "Height_201", "Yield_201", "FD_201")
b1.1 <- b1.1[,-c(5,6,7)]

b1.2 <- b1 %>% dplyr::filter(Treatment %in% c(202))
head(b1.2)
colnames(b1.2) <- c("Location", "Year", "Cut", "Block", "Position", "ID", "Treatment", "MSC_202", "DM_202", "Height_202", "Yield_202", "FD_202")
b1.2 <- b1.2[,-c(5,6,7)]

b1.3 <- left_join(b1, b1.1, by= c("Location", "Year", "Cut", "Block")) %>% left_join(., b1.2, by= c("Location", "Year", "Cut", "Block"))
summary(b1.3)
a3 <- inner_join(a1, b1.3, by = c("Location", "Block", "Position", "ID", "Treatment"))
str(a3)
colnames(a3)[2] <- "block"
colnames(a3)[5] <- "id"
head(a3)
# c1 <- inner_join(a1, b1, by = c("Location", "Block", "Position", "ID", "Treatment"))
# str(c1)

# write.csv(a3, "Y_PH_FD1.csv", row.names = F, quote = F)



a4 <- a3 %>% unite("merged", c(Location, Year, Cut), sep = "_", remove = F)
a4$merged <- as.factor(a4$merged)
str(a4)
a5 <- split(a4, a4$merged)
names(a5)

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/")
setwd("~/Documents/git/Norberg_2020/BLUE_values/split_data/")

for (i in names(a5)) {
  write.csv(a5[[i]],  paste0(i, '.csv'), row.names = F, quote = F)
}

# c1 <- c1 %>% unite("merged", c(Location, Year, Cut), sep = "_", remove = F)
# c1$merged <- as.factor(c1$merged)
# str(c1)
# c2 <- split(c1, c1$merged)
# names(c2)
# setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/")
# for (i in names(c2)) {
#   write.csv(c2[[i]],  paste0(i, '.csv'), row.names = F, quote = F)
# }

