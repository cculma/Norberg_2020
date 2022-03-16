# stagewise
rm(list = ls())
library(StageWise)
library(asreml)


P1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/ID_2019_1.csv")
P1 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/ID_2018_1.csv")
P2 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/ID_2018_2.csv")
P3 <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/ID_2018_3.csv")
P5 <- system.file("~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/4_Yield_1stage.csv")
P6 <- read.csv(P5)
directory <- "~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data"
P5 <- file.path(directory, "assay1.csv")

head(P1)
P4 <- rbind(P1, P2, P3)
head(P4)
colnames(P4)[1] <- "env"
colnames(P4)[6] <- "id"
P5 <- P4[,c(1,6:8,3,4,14)]
head(P4)
head(P5)
str(P5)
write.csv(P5, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/assay1.csv", quote = F, row.names = F)

effects <- data.frame(name=c("Block","Position"),
                      fixed=c(FALSE,TRUE),
                      factor=c(TRUE,FALSE))
effects
str(P4)
P1$Treatment <- as.character(P1$Treatment)
?Stage1
ans2a <- Stage1(filename = P5, 
                traits="Yield",
                effects = effects, solver="asreml")

ans2a <- Stage1(filename = P5, 
                traits="Yield",
                effects = effects, solver="asreml")

model2 <- Stage1(filename=P5, 
                 traits="Yield",
                 effects=effects, solver="spats", spline=c("row","col"))

model2$H2

stage1.vcov <- model2$vcov
stage1.blue <- model2$blue
colnames(stage1.blue)
stage1.blue$env <- as.factor(stage1.blue$env)
stage1.blue$id <- as.factor(stage1.blue$id)
str(stage1.blue)

stage1.1.blue <- read.csv("~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/4_Yield_1stage.csv")
stage1.1.blue$merged <- as.factor(stage1.1.blue$merged)
levels(stage1.1.blue$merged)
stage1.2.blue <- stage1.1.blue %>% dplyr::filter(merged %in% c("BLUE_ID_2018_1", "BLUE_ID_2018_2", "BLUE_ID_2018_3"))
colnames(stage1.2.blue) <- c("env", "id", "BLUE", "weight")
stage1.2.blue <- stage1.2.blue[,-4]
stage1.2.blue$env <- gsub("BLUE_", "", stage1.2.blue$env)


directory1 <- "~/Documents/Cesar/git/big_files"
G1 <- file.path(directory1, "Norberg_1.txt")
geno1 <- read_geno(filename=G1, ploidy=4, map=TRUE, min.minor.allele=5)
?Stage2
ans2c <- Stage2(data=stage1.blue, vcov=stage1.vcov, geno=geno1, silent=FALSE)
summary(ans2c$vars)

ans2d <- Stage2(data=stage1.2.blue, vcov=stage1.vcov, geno=geno1, silent=FALSE)
summary(ans2d$vars)


prep <- blup_prep(data=stage1.blue,
                  vcov=stage1.vcov,
                  geno = geno1,
                  vars=ans2d$vars)
pred2.id <- blup(prep, geno = geno1, what="id")

pred.marker <- blup(data=prep, geno=geno1, what="marker", gwas.ncore= 32)
# manhattan_plot(pred.marker,thresh=5.1,rotate.label=TRUE)


gwas_threshold(geno1, alpha=0.05, n.core= 32)
?gwas_threshold

 class(geno1)

pheno <- read.csv("~/Documents/Cesar/git/big_files/pheno.csv", row.names = 1)
head(pheno)
pheno <- pheno %>% rownames_to_column(var = "id")
colnames(pheno)
pheno <- pheno[,c(1,85)]

pheno <- inner_join(pheno, pred2.id, by = "id")
trait1 <- colnames(pheno)[1:(length(colnames(pheno))-3)]
trait1
cor(pheno$ST2_Yi_ID_2018, pheno$BV, use = "complete.obs")
plot(x = pheno$ST2_Yi_ID_2018, y = pheno$BV)
var(pheno$ST2_Yi_ID_2018, use = "complete.obs")
var(pheno$BV)

##########################

head(P1)
dim(P1)
colnames(P1)
P1.1 <- P1[,c(3,6,7,8,13,16,17)]
head(P1.1)
colnames(P1.1) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
str(P1.1)
P1.1$block <- as.factor(P1.1$block)
P1.1$col <- as.factor(P1.1$col)
P1.1$row <- as.factor(P1.1$row)
P1.1$gen <- as.factor(P1.1$gen)
P1.1$resp <- as.numeric(P1.1$resp)
P1.1$cov1 <- as.numeric(P1.1$cov1)
P1.1$cov2 <- as.numeric(P1.1$cov2)
P1.1 <- P1.1[order(P1.1$row, P1.1$col), ] 

# BLUP
m1 <- asreml::asreml(fixed = resp ~ 1 + cov1 + cov2, 
                     random = ~+block + gen, residual = ~ar1(row):ar1(col), 
                     data = P1.1, na.action = list(x = "include", y = "include"))
# BLUE
m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                     random = ~+block, residual = ~ar1(row):ar1(col), 
                     data = P1.1, na.action = list(x = "include", y = "include"))


summary(m2)$bic

BLUP <- summary(m2, coef = T)$linear.predictors
BLUE <- summary(m2, coef = T)$coef.fixed
class(BLUE)
BLUE <- BLUE[-c(1,2,203),]
hist(BLUE[,1])

m2$linear.predictors

preds <- predict(m2, classify='gen', vcov=TRUE)
vcov1 <- as.matrix(preds$vcov)

# Adding Weights
pvals <- preds$pvals
vcov <- as.matrix(preds$vcov)
sel <- matrix(1, ncol=1, nrow=length(pvals$predicted.value))
sel[is.na(pvals$predicted.value),] <- 0
vcov <- vcov[sel==1,sel==1]
pvals$weight[sel==1] <- diag(solve(vcov)) 
colnames(pvals)
str(pvals)
fa1 <- pvals[,c(1,2)]
fa1$gen <- as.character(fa1$gen)
cor(fa1$BLUE_He_ID_2019_1, pvals$predicted.value, use = "complete.obs")

###################

# model in for loop
setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/split_data/")
data_ar1 <- list.files(pattern = ".csv", full.names = T)
data_ar2 <- data_ar1[c(4,8,10:14,16,19,21:31)]
list_5 <- c("FA_MS", "FA_DM", "FA_He", "FA_Yi", "FA_FD")

