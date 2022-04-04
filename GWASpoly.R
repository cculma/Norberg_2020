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

library(tidyr)
library(devtools)
library(sommer)
library(ggplot2)
library(ggpubr)

library(ggthemes)
library(hrbrthemes)
library(VennDiagram)

#################

models_1 <- c("general", "additive", "1-dom", "2-dom",  "diplo-additive", "diplo-general")
#################


# workstation
setwd("~/Documents/Cesar/git/kamiak_norberg/")
# mac
setwd("~/Documents/git/Norberg_2020/GWAS_results/")

# FA1
pheno <- read.csv("pheno_fa1.csv", row.names = 1)
trait1 <- colnames(pheno)[1:(length(colnames(pheno))-3)]
trait1

data_1 <- read.GWASpoly(ploidy=4,
                        pheno.file="pheno_fa1.csv",
                        geno.file="AllSamples_Ms_filter_q30_imputed_GWASPoly_contigRemoved.txt",
                        format="ACGT", n.traits=length(trait1), delim=",")

data_2 <- set.K(data = data_1, LOCO = T, n.core = 60)
data_3.3 <- GWASpoly(data = data_2, models = models_1, traits = trait1, params = params, n.core = 100)

# save(data_3.3, file = "/scratch/user/cesar.medinaculma/20220324_080110/data_3.3.RData")
load("~/Documents/Cesar/git/big_files/data_3.3.RData")
# load("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/data_3.3.RData")
?set.threshold
data_5 <- set.threshold(data_3.3, method= "Bonferroni", level=0.05)

QTL_01 <- get.QTL(data_5)
QTL_02 <- QTL_01 %>% distinct(Marker, .keep_all = T) 
cc <- count(QTL_01, Trait)

################
N = 190
params <- set.params(geno.freq = 1 - 5/N,  fixed=c("PC1","PC2","PC3"),
                     fixed.type=rep("numeric",3), n.PC = 3)


data_2 <- set.K(data = data_1, LOCO = F, n.core = 30)
data_3.4 <- GWASpoly(data = data_2, models = models_1, traits = c("ST1_He_OR_2019_2", "ST1_MS_WA_2020_2"), params = params, n.core = 30)
data_5 <- set.threshold(data_3.4, method= "M.eff", level=0.05)
QTL_01 <- get.QTL(data_5)
QTL_02 <- QTL_01 %>% distinct(Marker, .keep_all = T) 
save(data_3.4, file = "~/Documents/Cesar/git/kamiak_norberg/data_3.4.RData")
################
P6 <- manhattan.plot(data = data_5, traits= c("ST1_He_OR_2019_2")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())

P7 <- manhattan.plot(data = data_5, traits= c("ST1_MS_WA_2020_2")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())

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
S3 <- inner_join(QTL_06, S1, by = "Marker") %>% inner_join(., S2, by = "Marker")

write.table(S3, "~/Documents/Cesar/git/big_files/markers1.tsv", row.names = F, quote = F, sep = "\t")

S2 <- dcast(S2, formula = Marker ~ Trait, fun.aggregate = length)
col_headings_3 <- c("stage", "trait", "loc", "year", "cut")
S4 <- as.data.frame(colnames(S2)) %>% separate(1, col_headings_3, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% dplyr::filter(!row_number() %in% c(1))
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

