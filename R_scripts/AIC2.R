library(tidyverse)

setwd("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/")
AIC1 <- list.files(pattern = "*AIC.csv", full.names = T)

list_1 <- gsub("_AIC.csv", "_", gsub("./", "", AIC1))
list_2 <- gsub("_AIC.csv", "", gsub("./", "", AIC1))

data <- read.csv(AIC1[1])
head(data)

AIC2 <- list()
for (i in 1:length(AIC1)) {
  data <- read.csv(AIC1[i])
  data <- data  %>% slice(1:(n() - 6))  %>% dplyr::select(1,5,8) %>% spread(key = model, value = AIC, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
  
  colnames(data)[2:length(data)] <- gsub("^", list_1[i], colnames(data)[2:length(data)])
  AIC2[[length(AIC2)+1]] = data
}
names(AIC2) <- list_2

AIC3 <- full_join(AIC2[[3]], AIC2[[1]], by = "trait") %>% full_join(., AIC2[[5]], by = "trait") %>% full_join(., AIC2[[4]], by = "trait") %>% full_join(., AIC2[[2]], by = "trait") 


AIC4 <- list()
for (i in 1:length(AIC1)) {
  data <- read.csv(AIC1[i])
  data <- data  %>% slice(tail(row_number(), 6))  %>% dplyr::select(3,5,8)
  colnames(data)[1:2] <- gsub("^", list_1[i], colnames(data)[1:2])
  AIC4[[length(AIC4)+1]] = data
}
names(AIC4) <- list_2
rm 
AIC5 <- AIC4[[3]] %>% dplyr::select(3,1,2) %>% full_join(., AIC4[[1]], by = "model") %>% full_join(., AIC4[[5]], by = "model") %>% full_join(., AIC4[[4]], by = "model") %>% full_join(., AIC4[[2]], by = "model") 


write.table(AIC3, "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/T1.tsv", row.names = F, quote = F, sep = "\t")

write.table(AIC5, "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/T2.tsv", row.names = F, quote = F, sep = "\t")
