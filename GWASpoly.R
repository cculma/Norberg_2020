rm(list = ls()) # clean Global Environment
# setwd("~/Documents/Cesar/blup_data/Roza2019/Analysis_2021/GWAS/")
# setwd("~/OneDrive - Washington State University (email.wsu.edu)/Roza_2019/git/Roza2019/")
library(GWASpoly)

library(tidyverse)
library(vcfR)
library(parallel)
library(doParallel)
library(iterators)
library(foreach)
library(tidyr)
library(devtools)
library(sommer)
library(ggplot2)
library(ggpubr)
library(data.table)
library(ggthemes)
library(hrbrthemes)
library(VennDiagram)
library(plotly)

setwd("~/Documents/Cesar/git/big_files/")
pheno <- read.csv("~/Documents/Cesar/git/big_files/yield.csv", row.names = 1)
head(pheno)
trait1 <- colnames(pheno)[1:(length(colnames(pheno))-3)]
trait1
params <- set.params(fixed=c("PC1","PC2","PC3"),
                     fixed.type=rep("numeric",3), n.PC = 3)
models_1 <- c("general", "additive", "1-dom", "2-dom",  "diplo-additive", "diplo-general")

data_1 <- read.GWASpoly(ploidy=4, 
                        pheno.file="yield.csv", 
                        geno.file="AllSamples_Ms_filter_q30_imputed_GWASPoly_contigRemoved.txt", 
                        format="ACGT", n.traits=length(trait1), delim=",")

data_2 <- set.K(data = data_1, LOCO = F, n.core = 32)
# data_3 <- GWASpoly(data = data_2, models = models_1, traits = trait1, params = params, n.core = 30)
data_3 <- GWASpoly(data = data_2, models = models_1, traits = "ST4_Overall", params = params, n.core = 32)

data_5 <- set.threshold(data_3, method= "Bonferroni", level=0.05)


QTL_01 <- get.QTL(data_5)
QTL_02 <- QTL_01 %>% distinct(Marker, .keep_all = T) 

save(data_3, file = "~/Documents/Cesar/git/big_files/data_3.RData")
# load("~/Documents/Cesar/git/big_files/data_3.RData")

