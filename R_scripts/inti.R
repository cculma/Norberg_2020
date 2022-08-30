library(inti)
library(purrr)
library(dplyr)

library(tidyverse)
library(asreml)
devtools::install_github("AparicioJohan/agriutilities") 
library(agriutilities)


fb <- inti::met
str(fb)
envs <- levels(fb$env)
model <- 1:length(envs) %>% map(function(x) {
  
  model <- fb %>% filter(env %in% envs[x]) %>% 
    
    H2cal(trait = "yield"
          , gen.name = "cultivar"
          , rep.n = 4
          , fixed.model = "0 + (1|rep) + (1|rep:alpha) + cultivar"
          , random.model = "1 + (1|rep) + (1|rep:alpha) + (1|cultivar)"
          # , plot_diag = T
          , emmeans = F
    )
  
  blues <- model$blues %>% mutate(trial = levels(fb$env)[x])
  
})

blues.h2cal <- bind_rows(model) %>% 
  separate(trial, c("zone", "location", "year")) %>% 
  mutate(across(c(yield, smith.w), as.numeric)) %>% 
  mutate(across(!c(yield, smith.w), as.factor))


met <- blues.h2cal %>%
  mutate(across(!yield, as.factor)) %>%
  H2cal(trait = "yield"
        , gen.name = "cultivar"
        , rep.n = 4
        , env.n = 18
        , env.name = "location"
        , fixed.model = "0 + zone + (1|zone:location) + (1|zone:cultivar) + cultivar"
        , random.model = "1 + zone + (1|zone:location) + (1|zone:cultivar) + (1|cultivar)"
        # , plot_diag = T
        , emmeans = T
        # , weights = blues.h2cal$smith.w
  )

blups.h2cal <- met$blups

str(blues.h2cal)
str(blups.h2cal)


library(caret)
library(tidyverse)
G1 <- read.csv("~/Documents/Cesar/git/big_files/Norberg_1.txt", check.names = F)
G1[1:4,1:6]
G1 <- G1 %>% unite(col = "Marker1", 1:2, sep = "_", remove = T) %>% column_to_rownames(var = "Marker1") 
G1 <- t(G1)
dim(G1)
nzv <- nearZeroVar(G1)
G2 <- G1[, -nzv]
dim(G2)
G2 <- t(G2)
G2[1:4,1:6]
class(G2)
G2 <- as.data.frame(G2)
G2 <- G2 %>% rownames_to_column(var = "Marker1") %>% separate(1, c("Chrom", "Position"), sep = "_", remove = T, convert = FALSE, extra = "merge")
write.csv(G2, "~/Documents/Cesar/git/big_files/Norberg_2.txt", quote = F, row.names = F)

i1$model <- "Diag"
i2$model <- "US"
i3$model <- "FA_1"
i4$model <- "FA_2"
i5$model <- "FA_3"
i6$model <- "CORGH"

BLUP_D <- predictPlus(classify = "gen", asreml.obj = Diag, 
                     wald.tab = NULL)$predictions

BLUP_U <- predictPlus(classify = "gen", asreml.obj = US, 
                      wald.tab = NULL, 
                      present = c("env", "loc", "gen"))$predictions

BLUP_1 <- predictPlus(classify = "gen", asreml.obj = FA_1, 
                      wald.tab = NULL, 
                      present = c("env", "loc", "gen"))$predictions

BLUP_2 <- predictPlus(classify = "gen", asreml.obj = FA_2, 
                      wald.tab = NULL, 
                      present = c("env", "loc", "gen"))$predictions

BLUP_3 <- predictPlus(classify = "gen", asreml.obj = FA_3, 
                      wald.tab = NULL, 
                      present = c("env", "loc", "gen"))$predictions

BLUP_C <- predictPlus(classify = "gen", asreml.obj = CORGH, 
                      wald.tab = NULL, 
                      present = c("env", "loc", "gen"))$predictions

M2 <- c("Dia", "UNS", "FA1", "FA2", "FA3", "COR")
M1 <- list(BLUP_D,BLUP_U,BLUP_1,BLUP_2,BLUP_3,BLUP_C)
M3 <- list()
for (i in 1:length(M1)) {
  data <- M1[[i]][,c(1,2)]
  colnames(data)[2] <- M2[i]
  M3[[length(M3)+1]] <- data
}
M4 <- inner_join(M3[[1]], M3[[2]]) %>% inner_join(., M3[[3]]) %>% inner_join(., M3[[4]]) %>% inner_join(., M3[[5]]) %>% inner_join(., M3[[6]])
var(M4$COR)

fa.asreml(FA_2, uniplot = T)


m0 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2,  
                     random = ~ + block, residual = ~ar1(row):ar1(col), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))
infoCriteria.asreml(m0)

m1 <- asreml::asreml(fixed = resp ~ 1 + gen,  
                     random = ~ + block + block:cov1 + block:cov2, residual = ~ar1(row):ar1(col), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m1 <- asreml::asreml(fixed = resp ~ 1 + cov1 + cov2,  
                     random = ~ + gen + block, residual = ~ar1(row):ar1(col), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))


m1 <- asreml::asreml(	fixed = resp ~ 1 + gen + cov1 + cov2, 
                      random = ~+block + row:col, 
                      residual = ~ar1(row):ar1(col), 
                      data = data, 
                      na.action = list(x = "include", y = "include"))

m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                     random = ~ + block + row:col, 
                     residual = ~ar1(row):id(col), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m3 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                     random = ~ + block + row:col, 
                     residual = ~ar2(row):ar2(col), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))

infoCriteria.asreml(m1)
infoCriteria.asreml(m2)
infoCriteria.asreml(m3)


blue1 <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals
hist(blue1$predicted.value)
hist(blue0$predicted.value)

blue0 <- predict.asreml(m0, classify='gen', vcov=TRUE)$pvals

blue0 <- blue0[,c(1,2)]
blue1 <- blue1[,c(1,2)]
colnames(blue0)[2] <- "ST0_Yi_M0"
colnames(blue1)[2] <- "ST0_Yi_M1"

blue2 <- inner_join(blue0, blue1) %>% inner_join(., PCA) 
write.csv(blue2, "~/Documents/Cesar/git/big_files/yield.csv", quote = F, row.names = F)
