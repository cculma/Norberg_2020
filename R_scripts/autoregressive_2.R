library(ggplot2)
library(tidyverse)
library(asreml)

a5 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/5_FD_1stage.csv")
head(a5)
hist(a5$predicted.value)
colnames(a5)[1] <- "trial"

b4 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/4_Yield/1_stage/predictions_2stage_ASReml_mrbean.csv")
b4 <- b4 %>% dplyr::select(1:3) %>% separate(1, into = c("BLUE", "Loc", "year", "cut"), sep = "_", remove = F, convert = T, extra = "warn")
colnames(b4)
str(b4)
trait1 <- colnames(b4)[1:(length(colnames(b4))-1)]
b4[,trait1] <- lapply(b4[,trait1], factor)


b5 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/5_FD/1_stage/predictions_2stage_ASReml_mrbean.csv")
b5 <- b5 %>% dplyr::select(1:3) %>% separate(1, into = c("BLUE", "Loc", "year", "cut"), sep = "_", remove = F, convert = T, extra = "warn")
colnames(b5)
str(b5)
trait1 <- colnames(b5)[1:(length(colnames(b5))-1)]
b5[,trait1] <- lapply(b5[,trait1], factor)

# %>% spread(trial, predicted.value) %>% remove_rownames() %>% column_to_rownames(var = "gen")
# list_5 <- gsub("BLUE_", "ST1_FD_", colnames(a5))
# colnames(a5) <- list_5
# a5 <- a5 %>% rownames_to_column(var = "gen")

cor(a5$predicted.value, b5$predicted.value, use = "complete.obs")

# antedependence model
model_ante <- asreml(predicted.value ~ year + cut + year:cut,
                     residual = ~gen:ante(cut), data = b5)


model_ar1 <- asreml(fixed = predicted.value ~ year + cut + year:cut,
                    random = ~ Loc,
                    residual = ~ gen:ar1(Loc), data = b4)

summary(model_ante)$bic

wald(model_ante)

# predict tratment by time means
predict(model_ante, classify = "Year:cut")

head(grassUV)
str(grassUV)
dim(grassUV)

