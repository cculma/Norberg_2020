# GWASpoly using FA scores
# GWASpoly using corrected values by biplot

rm(list = ls()) # clean Global Environment
# setwd("~/Documents/Cesar/blup_data/Roza2019/Analysis_2021/GWAS/")
# setwd("~/OneDrive - Washington State University (email.wsu.edu)/Roza_2019/git/Roza2019/")
library(GWASpoly)
library(tidyverse)
library(vcfR)
library(GenomicRanges)
library(genomation)
library(plyranges)
library(Repitools)
library(data.table)
library(plotly)

# workstation
setwd("~/Documents/Cesar/git/big_files/")

# pheno <- read.csv("~/Documents/Cesar/git/big_files/pheno_fa.csv", row.names = 1)
pheno <- read.csv("~/Documents/Cesar/git/big_files/pheno_fa1.csv", row.names = 1)
head(pheno)
trait1 <- colnames(pheno)[1:(length(colnames(pheno))-3)]
trait1
params <- set.params(fixed=c("PC1","PC2","PC3"),
                     fixed.type=rep("numeric",3), n.PC = 3)
models_1 <- c("general", "additive", "1-dom", "2-dom",  "diplo-additive", "diplo-general")
# models_1 <- c("general", "diplo-general")

data_1 <- read.GWASpoly(ploidy=4, 
                        pheno.file="pheno_fa1.csv", 
                        geno.file="AllSamples_Ms_filter_q30_imputed_GWASPoly_contigRemoved.txt", 
                        format="ACGT", n.traits=length(trait1), delim=",")
data_2 <- set.K(data = data_1, LOCO = F, n.core = 32)
data_3.2 <- GWASpoly(data = data_2, models = models_1, traits = trait1, params = params, n.core = 32)

<<<<<<< HEAD
data_5.2 <- set.threshold(data_3.2, method= "Bonferroni", level=0.05)
data_6.2 <- get.QTL(data_5.2) 
data_6.3 <- get.QTL(data_5.2) 
data_6.5 <- data_6.3 %>% distinct(Marker, .keep_all = T) 


save(data_3.2, file = "~/Documents/Cesar/git/big_files/data_3.2.RData")
load("~/Documents/Cesar/git/big_files/data_3.2.RData")
=======

data_5.3 <- set.threshold(data_4, method= "Bonferroni", level=0.05)
data_6.4 <- get.QTL(data_5.3) 

data_6.5 <- data_6.4 %>% distinct(Marker, .keep_all = T) 
data_6.4$Marker
>>>>>>> 25f6f31d39018bf3f19383a22b8ac1ce0f010980


data_2 <- set.K(data = data_1, LOCO = T, n.core = 32)
data_4 <- GWASpoly(data = data_2, models = models_1, traits = c("ST4_FD"), params = params, n.core = 32)

load("~/Documents/Cesar/git/big_files/data_4.1.RData") # data all
data_5.1 <- set.threshold(data_4.1, method= "Bonferroni", level=0.05)
data_6.1 <- get.QTL(data_5.1) 

trait2 <- c("ST3_DM_ID", "ST3_DM_OR", "ST3_DM_WA", "ST3_He_OR", "ST3_He_WA", "ST3_MS_ID", "ST3_MS_OR", "ST3_MS_WA", "ST3_Yi_ID", "ST3_Yi_OR", "ST3_Yi_WA", "ST4_DM_Overall", "ST4_FD_Overall", "ST4_He_Overall", "ST4_MS_Overall", "ST4_Yi_Overall")

data_6.1 <- data_6.1 %>% dplyr::filter(!Trait %in% trait2)

data_6.4 <- rbind(data_6.1, data_6.2)
data_6.5 <- data_6.4 %>% distinct(Marker, .keep_all = T) 
