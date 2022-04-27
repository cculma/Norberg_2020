rm(list=ls())
library(tidyverse)
library(AGHmatrix)
library(sommer)
library(e1071)
library(caret)

G <- read.csv("~/Documents/Cesar/git/big_files/AllSamples_Ms_filter_q30_imputed_GWASPoly_contigRemoved.txt", header = TRUE, row.names = 1, check.names = F) 
dim(G)# [1] 97316   194
G1 <- G %>% unite(Chrom1, 1:2, remove = T)
G1 <- as.matrix(G1 %>% remove_rownames() %>% column_to_rownames(var = "Chrom1"))
G2 <- t(G1)
G2[1:5,1:5]

lev2 <- (QTL_06$Marker1)

G2.1 <- G2[,lev2]
str(G2.1)

have.both = intersect(rownames(G2), lev2)
G2
G2.1 <- G2[have.both,]
dim(G2.1)
dim(G2) # [1]   192 97316
class(G2)
G3 <- as.data.frame(G2)
numo <- atcg1234(data=G3, ploidy=4, maf=0.05); 
G4 <- numo$M
G4.2 <- G4[,lev2]
lev3 <- colnames(G4.2)
G4.2 <- as.data.frame(G4.2)
G4.2[lev3] <- lapply(G4.2[lev3], as.numeric)  
G4.2[lev3] <- lapply(G4.2[lev3], factor) 
have.both = intersect(rownames(G4.2), rownames(pheno))
G4.2 <- G4.2[have.both,]
str(G4.2)
dim(G4.2)
G4.2[1:5,1:5]
class(G4.2)
G4.2 <- G4.2 %>% rownames_to_column(var = "gen")
G4.2$gen <- as.factor(G4.2$gen)

P7 <- inner_join(G4.2, P6)
write.table(P7, "~/Documents/Cesar/git/big_files/markers2.3.tsv", row.names = F, quote = F, sep = "\t")


summary(G4.2)
G4.3 <- G4.2 %>% rownames_to_column(var = "gen") %>% gather (key = "marker", value = "SNP", 2:81) %>% group_by(marker) %>% count(SNP) %>% spread (SNP, n) %>% column_to_rownames(var = "marker")
str(G4.3)
G4.3$sum <- rowSums(G4.3, na.rm = T)
G4.3 <- G4.3 %>% rownames_to_column(var = "Marker1")

G4.1 <- numo$ref.alleles
class(G4.1)
G4.1 <- G4.1[,lev2]
G4.2 <- t(G4.1)
G4.2 <- as.data.frame(G4.2)
G4.2 <- G4.2 %>% rownames_to_column(var = "Marker1") %>% unite(col = "SNP", 3:2, sep = "/", remove = T)

QTL_06 <- QTL_01 %>% dplyr::select(Marker, Chrom, Position) %>% distinct(Marker, .keep_all = TRUE) %>% unite(col = "Marker1", 2:3, sep = "_", remove = T) %>% inner_join(., QTL_03, by = "Marker") %>% inner_join(., G4.2, by = "Marker1") %>% inner_join(., G4.3, by = "Marker1") %>% inner_join(., QTL_03, by = "Marker")


QTL_06 <- QTL_06 %>% inner_join(., G4.2, by = "Marker1") %>% inner_join(., G4.3, by = "Marker1")


dim(G4) # [1]   192 96996
G4[1:5,1:5]

nzv <- nearZeroVar(G4)
G4 <- G4[, -nzv]
combo_info <- findLinearCombos(G4)
G4 <- G4[, combo_info$remove]
rm(combo_info)
dim(G4) # [1]   192 92657

G4[1:5,1:5]
G4 <- t(G4)
G4[1:5,1:5]
class(G4)
G4 <- as.data.frame(G4)
G4 <- G4 %>% rownames_to_column(var = "Marker1") %>% separate(col = 1, into = c("Chrom", "Position"), remove = F, sep = "_")
Marker <- seq(1:nrow(G4))
G6 <- cbind(Marker, G4)
dim(G6) # [1] 92465   196
G6 <- G6[,-2]
G6[1:5,1:5]
write.csv(G6, "~/Documents/Cesar/git/big_files/Norberg_2.txt", row.names = F, quote = F)



dim(G5) # [1]   192 92657
class(G5)


# Generate G matrix for GBLUP
G4 <- as.matrix(G4)
G4.1 <- Gmatrix(G4, method="Slater", ploidy=4)
?Gmatrix
G5 <- G4 %*% t(G4)/ncol(G4)

dim(G4.1)
dim(G5.1)
row.names(G4.1)
lev2 <- levels(data1$gen)

# Model 8: Factor Analytically model (fa1) 
met8 <- asreml(fixed = BLUE ~ 1 + env,
               random = ~ + fa(env, 1):id(gen),
               data = data1, na.action = list(x = "include", y = "include"), 
               weights = weight, family = asreml::asr_gaussian(dispersion = 1))
met8 <- update.asreml(met8)
infoCriteria.asreml(met8)

data4 <- data1 %>%  dplyr::filter(gen %in% have.both)
dim(data1)
dim(data4)

met9 <- asreml(fixed = BLUE ~ 1 + env + loc,
               random= ~+ fa(env, 1):id(gen) +  vm(gen, G4.1) ,
               data = data1, na.action = list(x = "include", y = "include"), 
               weights = weight, family = asreml::asr_gaussian(dispersion = 1))
infoCriteria.asreml(met9)

met9.1 <- summary(met9)$varcom

met7 <- asreml::asreml(fixed = BLUE ~ 1 + gen + loc, 
                       random = ~ + fa(env, 1):id(gen) + vm(gen, G4.1),
                       data = data1, na.action = list(x = "include", y = "include"), 
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))
FA_1 <- update.asreml(FA_1)



GBLUP2 <- predictPlus(classify = "gen", asreml.obj = met9, 
                     wald.tab = NULL, 
                     present = c("env", "loc", "gen"))$predictions

GBLUP3 <- predictPlus(classify = "loc:gen", asreml.obj = met9, 
                     wald.tab = NULL, 
                     present = c("env", "loc", "gen"))$predictions

GBLUP3 <- GBLUP3[,c(1:3)]
GBLUP2 <- GBLUP2[,c(1:2)]


GBLUP3$loc <- gsub("^", "ST3_GFD_", GBLUP3$loc)

GBLUP3 <- GBLUP3 %>% spread(key = loc, value = predicted.value, fill = NA, 
                          convert = FALSE, drop = TRUE, sep = NULL)
colnames(GBLUP2)[2] <- "ST4_GFD"
GBLUP4 <- inner_join(GBLUP3, GBLUP2, by = "gen") %>% left_join(., PCA, by = "gen") 
write.csv(GBLUP4, "~/Documents/Cesar/git/big_files/GFD_cal.csv", quote = F, row.names = F)



library(dplyr)
set.seed(1)
dat <- data.frame(ID = sample(letters,100,rep=TRUE))
dat %>% 
  group_by(ID) %>%
  summarise(no_rows = length(ID))

