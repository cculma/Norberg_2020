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

pheno <- read.csv("~/Documents/Cesar/git/big_files/pheno_MCS.csv", row.names = 1)
head(pheno)
trait1 <- colnames(pheno)[1:(length(colnames(pheno))-3)]
trait1
params <- set.params(fixed=c("PC1","PC2","PC3"),
                     fixed.type=rep("numeric",3), n.PC = 3)
models_1 <- c("general", "additive", "1-dom", "2-dom",  "diplo-additive", "diplo-general")
data_1 <- read.GWASpoly(ploidy=4, 
                        pheno.file="pheno_MCS.csv", 
                        geno.file="AllSamples_Ms_filter_q30_imputed_GWASPoly_contigRemoved.txt", 
                        format="ACGT", n.traits=length(trait1), delim=",")
data_2 <- set.K(data = data_1, LOCO = F, n.core = 32)
data_3.2 <- GWASpoly(data = data_2, models = models_1, traits = trait1, params = params, n.core = 32)
# ST1_1MSC_1 <- data_3.2
data_5.2 <- set.threshold(data_3.2, method= "Bonferroni", level=0.05)

data_6.3 <- get.QTL(data_5.2) 
QTL_01 <- get.QTL(data_5.2) 
data_6.5 <- data_6.3 %>% distinct(Marker, .keep_all = T) 

QTL_03 <- QTL_01 %>% group_by(Marker) %>% top_n(1, abs(Score)) %>% dplyr::select(Marker, Score) %>% distinct(Marker, .keep_all = TRUE)
QTL_04 <- QTL_01 %>% group_by(Marker) %>% summarise(Trait = paste(Trait, collapse = ";")) 
QTL_05 <- QTL_01 %>% group_by(Marker) %>% summarise(Model = paste(Model, collapse = ";")) 
QTL_06 <- QTL_01 %>% dplyr::select(Marker, Chrom, Position, Ref, Alt) %>% distinct(Marker, .keep_all = TRUE) %>% unite(col = "SNP", 5:4, sep = "/", remove = T) %>% unite(col = "Marker1", 2:3, sep = "_", remove = T) %>% inner_join(., QTL_03, by = "Marker")

S1 <- QTL_01 %>% dplyr::select(2,4) %>% distinct(Marker, Model, .keep_all = T) 
S1 <- dcast(S1, formula = Marker ~ Model, fun.aggregate = length)
S2 <- QTL_01 %>% dplyr::select(1,4) %>% distinct(Marker, Trait, .keep_all = T) 
S2 <- dcast(S2, formula = Marker ~ Trait, fun.aggregate = length)
colnames(S2)
S3 <- inner_join(QTL_06, S1, by = "Marker") %>% inner_join(., S2, by = "Marker")
write.table(S3, "~/Documents/Cesar/git/big_files/markers1.tsv", row.names = F, quote = F, sep = "\t")

load("~/Documents/Cesar/git/big_files/data_3.2.RData")
# file generated using GWAS_kamiak.R: pheno_fa.csv and AllSamples_Ms_filter_q30_imputed_GWASPoly_contigRemoved.txt