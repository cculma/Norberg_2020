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
library(devtools)
library(sommer)
library(ggplot2)
library(ggpubr)

library(ggthemes)
library(hrbrthemes)
library(VennDiagram)

#################
params <- set.params(fixed=c("PC1","PC2","PC3"),
                     fixed.type=rep("numeric",3), n.PC = 3)
models_1 <- c("general", "additive", "1-dom", "2-dom",  "diplo-additive", "diplo-general")
#################

# workstation
setwd("~/Documents/Cesar/git/big_files/")
# mac
setwd("~/Documents/git/Norberg_2020/GWAS_results/")
setwd("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/")
# FA1

pheno <- read.csv("pheno_fa1.csv", row.names = 1)
trait1 <- colnames(pheno)[1:(length(colnames(pheno))-3)]
trait1


data_1.1 <- read.GWASpoly(ploidy=4,
                        pheno.file="pheno_fa1.csv",
                        geno.file="Norberg_2.txt",
                        format="numeric", n.traits=length(trait1), delim=",")
data_2.1 <- set.K(data = data_1.1, LOCO = T, n.core = 10)
data_3.3 <- GWASpoly(data = data_2.1, models = models_1, traits = trait1, params = params, n.core = 10)

pheno <- read.csv("pheno_fa2.csv", row.names = 1)
trait1 <- colnames(pheno)[1:(length(colnames(pheno))-3)]
trait1

data_1.2 <- read.GWASpoly(ploidy=4,
                          pheno.file="pheno_fa2.csv",
                          geno.file="Norberg_2.txt",
                          format="numeric", n.traits=length(trait1), delim=",")
data_2.2 <- set.K(data = data_1.2, LOCO = T, n.core = 10)
data_3.4 <- GWASpoly(data = data_2.2, models = models_1, traits = trait1, params = params, n.core = 10)


pheno <- read.csv("pheno_fa.csv", row.names = 1)
trait1 <- colnames(pheno)[1:(length(colnames(pheno))-3)]
trait1



data_1.1 <- read.GWASpoly(ploidy=4,
                        pheno.file="pheno_fa.csv",
                        geno.file="Norberg_2.txt",
                        format="numeric", n.traits=length(trait1), delim=",")
data_2.1 <- set.K(data = data_1.1, LOCO = T, n.core = 30)
data_3.3 <- GWASpoly(data = data_2.1, models = models_1, traits = trait1, params = params, n.core = 30)
ST0_data_3.3 <- data_3.3
# data_3.3 <- GWASpoly(data = data_2.1, models = models_1, traits =c("BLUP_ST3_FD_WA"), params = params, n.core = 30)

# FD_data_3.3 <- data_3.3
# MS_data_3.3 <- data_3.3
save(ST0_data_3.3, file = "~/Documents/Cesar/git/big_files/ST0_data_3.3.RData")

# data_4 <- set.K(data = data_1, LOCO = F, n.core = 30)
# data_4.3 <- GWASpoly(data = data_4, models = models_1, traits = trait1, params = params, n.core = 30)
# FD_data_4.3 <- data_4.3

hist(pheno$ST1_MS_WA_2020_5)
boxplot(pheno$ST1_MS_WA_2020_5)

data_5.0 <- set.threshold(data_3.3, method= "Bonferroni", level=0.05)
data_5.1 <- set.threshold(data_3.4, method= "Bonferroni", level=0.05)
data_5.0 <- set.threshold(data_3.3, method= "FDR", level=0.05)
data_5.0 <- set.threshold(data_3.3, method= "M.eff", level=0.05)
QTL_01 <- get.QTL(data_5.0)
QTL_02 <- get.QTL(data_5.1)
QTL_01 <- QTL_01 %>% distinct(Marker, .keep_all = T) 
QTL_02 <- QTL_02 %>% distinct(Marker, .keep_all = T) 



data_5.1 <- set.threshold(FD_data_4.3, method= "Bonferroni", level=0.05)

data_5.0 <- set.threshold(data_3.3, method= "Bonferroni", level=0.05)
data_5.1 <- set.threshold(data_3.3, method= "M.eff", level=0.05)


QTL_01 <- get.QTL(data_5.0)

save(FD_data_3.3, file = "~/Documents/Cesar/git/big_files/FD_data_3.3.RData")
save(FD_data_4.3, file = "~/Documents/Cesar/git/big_files/FD_data_4.3.RData")


load("~/Documents/Cesar/git/big_files/data_3.3.RData")
# load("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/data_3.3.RData")

data_5 <- set.threshold(MS_data_3.3, method= "Bonferroni", level=0.05)


QTL_01 <- get.QTL(data_5)
QTL_01 <- QTL_01 %>% dplyr::filter(Score > 5) 
QTL_01 <- QTL_01 %>% dplyr::filter(!Trait %in% c("ST1_He_OR_2019_2", "ST1_MS_WA_2020_2"))

QTL_01 <- QTL_01 %>% distinct(Marker, .keep_all = T) 
cc1 <- count(QTL_01, Trait)
cc1$Trait

QTL_02.1 <- QTL_02 %>% distinct(Marker, .keep_all = T) 
cc2 <- count(QTL_02, Trait)

################

QTL_03 <- QTL_01 %>% group_by(Marker) %>% top_n(1, abs(Score)) %>% dplyr::select(Marker, Score) %>% distinct(Marker, .keep_all = TRUE)
QTL_04 <- QTL_01 %>% group_by(Marker) %>% summarise(Trait = paste(Trait, collapse = ";")) 
QTL_05 <- QTL_01 %>% group_by(Marker) %>% summarise(Model = paste(Model, collapse = ";")) 
QTL_06 <- QTL_01 %>% dplyr::select(Marker, Chrom, Position, Ref, Alt) %>% distinct(Marker, .keep_all = TRUE) %>% unite(col = "SNP", 5:4, sep = "/", remove = T) %>% unite(col = "Marker1", 2:3, sep = "_", remove = T) %>% inner_join(., QTL_03, by = "Marker")

################
# Generate a binary matrix for reduntant markers

S1 <- QTL_01 %>% dplyr::select(2,4) %>% distinct(Marker, Model, .keep_all = T) 
S1 <- dcast(S1, formula = Marker ~ Model, fun.aggregate = length)

S2 <- QTL_01 %>% dplyr::select(1,4) %>% distinct(Marker, Trait, .keep_all = T) 
S2 <- dcast(S2, formula = Marker ~ Trait, fun.aggregate = length)
colnames(S2)

colSums(S2[2:length(S2)])
QTL_07 <- QTL_06
QTL_07$totalM <- rowSums(S3[13:length(S3)])

colnames(S3)
QTL_08 <- QTL_01 %>% separate(1, c("stage", "trait", "loc", "year", "cut"), sep = "_", remove = T, convert = FALSE, extra = "merge")  %>% dplyr::select(3,4,5,8)  %>% distinct(Marker, .keep_all = T) 

QTL_08 <- QTL_01 %>% dplyr::select(3,4,5,8)  %>% distinct(Marker, .keep_all = T) write.csv(S4, "GWAS_results/markers_FDcor.csv")


QTL_07 <- right_join(QTL_07, QTL_08, by = "Marker") 

S3 <- inner_join(QTL_06, S1, by = "Marker") %>% inner_join(., S2, by = "Marker")

write.table(S3, "~/Documents/Cesar/git/big_files/markers1.tsv", row.names = F, quote = F, sep = "\t")

S2 <- dcast(S2, formula = Marker ~ Trait, fun.aggregate = length)
col_headings_3 <- c("stage", "trait", "loc", "year", "cut")
S4 <- as.data.frame(colnames(S2)) %>% separate(1, col_headings_3, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% dplyr::filter(!row_number() %in% c(1))

write.table(S3, "~/Documents/git/Norberg_2020/GWAS_results/FD_marker.csv", row.names = F, quote = F, sep = "\t")

write.table(S4, "~/Documents/Cesar/git/Norberg_2020/spatial_distribution/header.tsv", row.names = F, quote = F, sep = "\t")

##############
# To annotate markers
# load gene annotation Medicago sativa Zhongmu No1
load("~/Documents/Cesar/RNA/globus/lordec_reports/lordec_trim/bed_Shen/ORF_NMD/i_5.2.8.RData")
i_5.2.8 <- i_5.2.8 %>% dplyr::select(1,3)
head(i_5.2.8)

# GRanges
file <- ("~/Documents/Cesar/RNA/globus/lordec_reports/lordec_trim/bed_Shen/ORF_NMD/blast_corrected_shen.bed")
txdb <- readBed(file, track.line = FALSE, remove.unusual = FALSE,
                zero.based = TRUE)

col_headings_1 <- c('gene_id',	'uniprot', 'gene_name',	'trans_length_flag',	'blastp_match_flag',	'nmd_flag',	'frame')
col_headings_2 <- c('gene_id',	'isoform')

QTL_02 <- QTL_01 %>% distinct(Marker, .keep_all = T) 
gr5 <- GRanges(seqnames = QTL_02$Chrom,
               ranges = IRanges(QTL_02$Position, width = 1))

overlaps <- join_overlap_left(gr5, txdb)

df2 <- annoGR2DF(overlaps)
df2 <- unite(data = df2, col = "Marker1", 1:2, sep = "_", remove = F) %>% distinct(Marker1, .keep_all = TRUE) %>% dplyr::select(1:4,7) 
head(df2)
df3 <- df2 %>% separate(5, col_headings_1, sep = ";", remove = TRUE, convert = FALSE, extra = "warn") %>% separate(5, col_headings_2, sep = "\\.", remove = TRUE, convert = FALSE, extra = "warn") %>% dplyr::select(1,5,7) %>% inner_join(., i_5.2.8, by = "gene_id")

QTL_08 <- inner_join(QTL_06, QTL_04, by = "Marker") %>% inner_join(., QTL_05, by = "Marker") %>% left_join(., df3, by = "Marker1") 

nrow(QTL_08 %>% distinct(gene_id, .keep_all = TRUE))
sum(!is.na(QTL_08$gene_id)) # 72 annotated in Uniprot
sum(is.na(QTL_08$gene_id)) # 42 annotated in Uniprot
colnames(QTL_08)
write.table(QTL_08, "~/Documents/Cesar/git/big_files/markers3.tsv", row.names = F, quote = F, sep = "\t")

QTL_09 <- QTL_08 %>% group_by(gene_id) %>% summarise(Marker1 = paste(Marker1, collapse = ";")) 
QTL_10 <- QTL_08 %>% distinct(gene_id, .keep_all = TRUE) %>% dplyr::select(7:9)

QTL_10 <- inner_join(QTL_09, QTL_10, by = "gene_id")
QTL_10 <- na.omit(QTL_10)
head(QTL_10)
write.table(QTL_10, "~/Documents/Cesar/git/big_files/markers2.tsv", row.names = F, quote = F, sep = "\t")

# save image 
save.image(file = "~/Documents/Cesar/git/big_files/data_4.RData")
# data_4.RData is big (1.6 GB) I will reduce the size removing data_1, data_2, data_3, data_3.1
rm(list = c("data_1", "data_2", "data_3", "data_3.1", "data_5"))
save.image(file = "~/Documents/Cesar/git/big_files/data_5.RData")

####################
library(GWASpoly)
pheno <- read.csv("~/Documents/Cesar/git/big_files/5_FD_1stage.csv")
head(pheno)
g1 <- file.path("~/Documents/Cesar/git/big_files/Norberg_1.txt")

geno <- read_geno(filename=g1, ploidy=4, map=TRUE, min.minor.allele=5)
class(geno)


D1 <- read.GWASpoly(ploidy=4, pheno.file="5_FD_1stage.csv", 
                      geno.file="Norberg_1.txt",
                      format="numeric", n.traits=1, delim=",")

data.loco <- set.K(data = D1, LOCO=TRUE, n.core=32)

N <- 189 #Population size
params <- set.params(geno.freq = 1 - 5/N, fixed = "env", fixed.type = "factor")

data.loco.scan <- GWASpoly(data=data.loco,
                           models=c("additive","1-dom"),
                           traits=c("predicted.value"), 
                           params=params,
                           n.core=30)

class(data.loco.scan)
class(data_3.2)
data2 <- set.threshold(data.loco.scan,method="M.eff",level=0.05)


###########
# end

