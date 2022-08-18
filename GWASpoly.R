rm(list = ls()) # clean Global Environment
# setwd("~/Documents/Cesar/blup_data/Roza2019/Analysis_2021/GWAS/")
# setwd("~/OneDrive - Washington State University (email.wsu.edu)/Roza_2019/git/Roza2019/")
session_info()

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

library(parallel)
#################
params <- set.params(fixed=c("PC1","PC2","PC3"),
                     fixed.type=rep("numeric",3), n.PC = 3)

params <- set.params(fixed="env",
                     fixed.type="factor")

models_1 <- c("general", "additive", "1-dom", "2-dom",  "diplo-additive", "diplo-general")
#################

# workstation
setwd("~/Documents/Cesar/git/big_files/")
# mac
setwd("~/Documents/git/Norberg_2020/GWAS_results/")
setwd("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/")
# FA1

pheno <- read.csv("Sum_yield.csv")
pheno <- read.csv("Yi_WA.csv")
head(pheno)
str(pheno)
pheno$env <- as.factor(pheno$env)
levels(pheno$env)

trait1 <- colnames(pheno)[2:(length(colnames(pheno))-3)]
trait1
length(trait1)

data_1.1 <- read.GWASpoly(ploidy=4,
                          pheno.file="PH_WA.csv",
                          geno.file="Norberg_2.txt",
                          format="numeric", n.traits=1, delim=",")

data_2.1 <- set.K(data = data_1.1, LOCO = F, n.core = 10)
data_3.3 <- GWASpoly(data = data_2.1, models = models_1, traits = "PH_WA", params = params, n.core = 10)

PH_WA_HC_data_3.3 <- data_3.3
FD1_HC_data_3.3 <- data_3.3
FD_OR_HC_data_3.3 <- FD1_HC_data_3.3
PH1_data_3.3 <- data_3.3
Yi_HC_data_3.3 <- data_3.3
Yi_HC_OR_data_3.3 <- data_3.3
PH_FD_HC_data_3.3 <- data_3.3
save(FD_OR_HC_data_3.3, file = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/FD_OR_HC_data_3.3.RData")

save(PH_WA_HC_data_3.3, file = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/PH_WA_HC_data_3.3.RData")

pheno <- read.csv("pheno_fa2.csv", row.names = 1)
trait1 <- colnames(pheno)[1:(length(colnames(pheno))-3)]
trait1

data_1.2 <- read.GWASpoly(ploidy=4,
                          pheno.file="Sum_yield.csv",
                          geno.file="Norberg_2.txt",
                          format="numeric", n.traits=length(trait1), delim=",")
data_2.2 <- set.K(data = data_1.2, LOCO = T, n.core = 10)
data_3.4 <- GWASpoly(data = data_2.2, models = models_1, traits = trait1, params = params, n.core = 10)

SumYi_data_3.3 <- GWASpoly(data = data_2.2, models = models_1, traits = trait1, params = params, n.core = 10)
save(SumYi_data_3.3, file = "/Users/cesarmedina/Library/CloudStorage/OneDrive-WashingtonStateUniversity(email.wsu.edu)/Sen_2020/yield_FD/RData/SumYi_data_3.3.RData")



pheno <- read.csv("pheno_fa.csv", row.names = 1)
trait1 <- colnames(pheno)[1:(length(colnames(pheno))-3)]
trait1
data_1.1 <- read.GWASpoly(ploidy=4,
                        pheno.file="Y",
                        geno.file="Norberg_2.txt",
                        format="numeric", n.traits=length(trait1), delim=",")
data_2.1 <- set.K(data = data_1.1, LOCO = T, n.core = 30)
data_3.3 <- GWASpoly(data = data_2.1, models = models_1, traits = trait1, params = params, n.core = 30)

FD_data_3.3 <- data_3.3
ST0_data_3.3 <- data_3.3
# data_3.3 <- GWASpoly(data = data_2.1, models = models_1, traits =c("BLUP_ST3_FD_WA"), params = params, n.core = 30)


# data_4 <- set.K(data = data_1, LOCO = F, n.core = 30)
# data_4.3 <- GWASpoly(data = data_4, models = models_1, traits = trait1, params = params, n.core = 30)

hist(pheno$ST1_MS_WA_2020_5)
boxplot(pheno$ST1_MS_WA_2020_5)
data_5.1 <- set.threshold.2(PH_WA_HC_data_3.3,  method= "Bonferroni", level=0.05)

data_5.2 <- set.threshold.1(PH_WA_HC_data_3.3, method = "M.eff", level=0.05, n.core = 6)

data_5.1 <- set.threshold(data_3.3, method= "Bonferroni", level=0.05)
?set.threshold
QTL_02 <- get.QTL(data_5.1)
QTL_03 <- get.QTL(data_5.2)

QTL_02 <- QTL_02 %>% distinct(Marker, .keep_all = T) 

QTL_02 <- QTL_02 %>% dplyr::filter(!Model %in% c("diplo-additive", "diplo-general"))
cc <- count(QTL_02, Model)
cc$Model
cc <- count(QTL_03, Trait)


data_5.2 <- set.threshold(Yi_HC_OR_data_3.3, method= "Bonferroni", level=0.05)
QTL_01 <- get.QTL(data_5.2)
QTL_01 <- QTL_01 %>% distinct(Marker, .keep_all = T) 

cc <- count(QTL_01, Trait)
R <- str_subset(cc$Trait, '_R_')
S <- str_subset(cc$Trait, '_S_')



load("/home/hawkins/Documents/Cesar/git/big_files/Yi_data_3.3.RData")
load("/home/hawkins/Documents/Cesar/git/big_files/DM_data_3.3.RData")
load("/home/hawkins/Documents/Cesar/git/big_files/MS_data_3.3.RData")
load("/home/hawkins/Documents/Cesar/git/big_files/FD_data_3.3.RData")
load("/home/hawkins/Documents/Cesar/git/big_files/PH_data_3.3.RData")

load("/Users/cesarmedina/Library/CloudStorage/OneDrive-WashingtonStateUniversity(email.wsu.edu)/Sen_2020/yield_FD/RData/Yi_data_3.3.RData")
load("/Users/cesarmedina/Library/CloudStorage/OneDrive-WashingtonStateUniversity(email.wsu.edu)/Sen_2020/yield_FD/RData/DM_data_3.3.RData")
load("/Users/cesarmedina/Library/CloudStorage/OneDrive-WashingtonStateUniversity(email.wsu.edu)/Sen_2020/yield_FD/RData/MS_data_3.3.RData")
load("/Users/cesarmedina/Library/CloudStorage/OneDrive-WashingtonStateUniversity(email.wsu.edu)/Sen_2020/yield_FD/RData/FD_data_3.3.RData")
load("/Users/cesarmedina/Library/CloudStorage/OneDrive-WashingtonStateUniversity(email.wsu.edu)/Sen_2020/yield_FD/RData/PH_data_3.3.RData")




data_5.1 <- set.threshold(MS_data_3.3, method= "Bonferroni", level=0.05, n.permute = 1)
data_5.2 <- set.threshold(DM_data_3.3, method= "Bonferroni", level=0.05, n.permute = 1)
data_5.3 <- set.threshold(PH_data_3.3, method= "Bonferroni", level=0.05, n.permute = 1)
data_5.3 <- set.threshold(PH1_data_3.3, method= "Bonferroni", level=0.05, n.permute = 1)
data_5.4 <- set.threshold(Yi_data_3.3, method= "Bonferroni", level=0.05, n.permute = 1)
data_5.5 <- set.threshold(FD_data_3.3, method= "Bonferroni", level=0.05, n.permute = 1)


# data_5.1 <- set.threshold.1(MS_data_3.3, method = "M.eff", level=0.05, n.core = 8, n.permute = 1)
# data_5.2 <- set.threshold.1(DM_data_3.3, method = "M.eff", level=0.05, n.core = 8, n.permute = 1)
# data_5.3 <- set.threshold.1(PH_data_3.3, method = "M.eff", level=0.05, n.core = 8, n.permute = 1)
# data_5.4 <- set.threshold.1(Yi_data_3.3, method = "M.eff", level=0.05, n.core = 8, n.permute = 1)
# data_5.5 <- set.threshold.1(FD_data_3.3, method = "M.eff", level=0.05, n.core = 8, n.permute = 1)


QTL_1 <- get.QTL(data_5.1)
QTL_2 <- get.QTL(data_5.2)
QTL_3 <- get.QTL(data_5.3)
QTL_4 <- get.QTL(data_5.4)
QTL_5 <- get.QTL(data_5.5)


QTL_01 <- rbind(QTL_1, QTL_2, QTL_3, QTL_4, QTL_5)

QTL_02 <- QTL_01 %>% dplyr::filter(Trait %in% "^ST0_")
QTL_02 <- QTL_01 %>% dplyr::filter(str_detect(Trait, "^ST0_"))

QTL_1 <- QTL_1 %>% distinct(Marker, .keep_all = T) 
QTL_2 <- QTL_2 %>% distinct(Marker, .keep_all = T) 
QTL_3 <- QTL_3 %>% distinct(Marker, .keep_all = T) 
QTL_4 <- QTL_4 %>% distinct(Marker, .keep_all = T) 
QTL_5 <- QTL_5 %>% distinct(Marker, .keep_all = T) 

QTL_01 <- QTL_5
QTL_01 <- QTL_01 %>% distinct(Marker, .keep_all = T) 
cc2 <- count(QTL_01, Trait)
cc2 <- count(QTL_5, Trait) %>% separate(1, c("st", "trans", "trait"), sep = "_", remove = T, convert = FALSE, extra = "merge") %>% unite(col = "trait", 1,3, sep = "_", remove = T) %>% spread(key = trans, value = n, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
colnames(cc2) <- c("trait", "raw", "scaled")
write.table(cc2, "~/Documents/Cesar/git/big_files/markers1.3.tsv", row.names = F, quote = F, sep = "\t")


################
cc <- count(QTL_3,Trait)
lev4 <- cc$Trait
lev4
cc1 <- count(QTL_3, Model)
cc1$Model

QTL_3 <- QTL_3 %>% dplyr::filter(!Model %in% c("diplo-general", "diplo-additive"))

"ST1_PH_OR_2020_2"
"ST1_PH_OR_2019_2"


fit_05 <- fit.QTL(data=data_5.3, trait = "ST1_PH_OR_2020_2",
                  qtl=QTL_3[,c("Marker","Model")])



fit_06 <- list()
for (i in 1:length(lev4)) {
  fit_05 <- fit.QTL(data=data_5.4, trait = lev4[i],
                    qtl=QTL_4[,c("Marker","Model")])
  fit_05 <- fit_05 %>% group_by(Marker) %>% top_n(1, abs(R2))
  fit_06[[length(fit_06) + 1]] <- fit_05
}
names(fit_06) <- lev4
fit_06 <-rbindlist(fit_06, use.names=TRUE, fill=TRUE, idcol="trait")
fit_06 <- fit_06 %>% group_by(Marker) %>% top_n(1, abs(R2))
colnames(fit_06)
fit_06 <- fit_06[,c(2,6)]

fit_06$trait <- "Yi"

PH_fit_06 <- fit_06
MS_fit_06 <- fit_06
DM_fit_06 <- fit_06
Yi_fit_06 <- fit_06
FD_fit_06 <- fit_06

PH_fit_06 <- PH_fit_06 %>% distinct(Marker, .keep_all = T)

fit_07 <- rbind(MS_fit_06,DM_fit_06,Yi_fit_06,FD_fit_06,PH_fit_06)
################

QTL_03 <- QTL_01 %>% group_by(Marker) %>% top_n(1, abs(Score)) %>% dplyr::select(Marker, Score) %>% distinct(Marker, .keep_all = TRUE)
QTL_04 <- QTL_01 %>% group_by(Marker) %>% summarise(Trait = paste(Trait, collapse = ";")) 
QTL_05 <- QTL_01 %>% group_by(Marker) %>% summarise(Model = paste(Model, collapse = ";")) 
QTL_06 <- QTL_01 %>% dplyr::select(Marker, Chrom, Position, Ref, Alt) %>% distinct(Marker, .keep_all = TRUE) %>% unite(col = "SNP", 5:4, sep = "/", remove = T) %>% unite(col = "Marker1", 2:3, sep = "_", remove = T) %>% inner_join(., QTL_03, by = "Marker")

QTL_07 <-  QTL_01 %>% dplyr::select(Marker, Score, Trait) %>% slice(-c(1,7)) %>% spread(key = Trait, value = Score, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

colnames(QTL_07) 
QTL_07 <- QTL_07[,c(1,2,4,3,5,6,8,7,9,10,12,11,13)]
write.table(QTL_07, "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/markers1.5.tsv", row.names = F, quote = F, sep = "\t")


################
# Generate a binary matrix for reduntant markers

S1 <- QTL_01 %>% dplyr::select(2,4) %>% distinct(Marker, Model, .keep_all = T) 
S1 <- dcast(S1, formula = Marker ~ Model, fun.aggregate = length)
colnames(S1)

S2 <- QTL_01 %>% dplyr::select(1,4) %>% distinct(Marker, Trait, .keep_all = T) 
S2 <- dcast(S2, formula = Marker ~ Trait, fun.aggregate = length)
colnames(S2)
S2 <- S2[,c(1,2,4,6,8,10,12,3,5,7,9,11,13)]

QTL_07 <- QTL_06

QTL_07$totalM <- rowSums(S3[13:length(S3)])

QTL_08 <- QTL_01 %>% separate(1, c("stage", "trait", "loc", "year", "cut"), sep = "_", remove = T, convert = FALSE, extra = "merge")  %>% dplyr::select(3,4,5,8)  %>% distinct(Marker, .keep_all = T) 

QTL_08 <- QTL_01 %>% dplyr::select(3,4,5,8)  %>% distinct(Marker, .keep_all = T) 

QTL_07 <- right_join(QTL_07, QTL_08, by = "Marker") 

S3 <- inner_join(QTL_06, S1, by = "Marker") %>% inner_join(., S2, by = "Marker") 
S3 <- inner_join(QTL_06, S1, by = "Marker") %>% left_join(., fit_07, by = "Marker") 
colnames(S3)
S3 <- S3[,c(1:4,11:18,5:9,19,20)]

write.table(S3, "~/Documents/Cesar/git/big_files/markers1.4.tsv", row.names = F, quote = F, sep = "\t")


col_headings_3 <- c("stage", "trait", "loc", "year", "cut")
S4 <- as.data.frame(colnames(S2)) %>% separate(1, col_headings_3, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% dplyr::filter(!row_number() %in% c(1))
write.csv(S4, "GWAS_results/markers_FDcor.csv")

write.table(S4, "~/Documents/Cesar/git/big_files/markers1.2.tsv", row.names = F, quote = F, sep = "\t")

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

QTL_08 <- inner_join(QTL_06, QTL_04, by = "Marker") %>% inner_join(., QTL_05, by = "Marker") %>% left_join(., df3, by = "Marker1") %>% left_join(., fit_07, by = "Marker")
colnames(QTL_08)
QTL_08.1 <- QTL_08[,c(5,7)]
QTL_08.1 <- na.omit(QTL_08.1)
QTL_08.1 <- QTL_08.1 %>% distinct(gene_id, .keep_all = T) %>% separate(1, c("st", "trait", "loc"), sep = "_", remove = F, convert = FALSE, extra = "merge") %>% dplyr::select(3,5)

nrow(QTL_08 %>% distinct(gene_id, .keep_all = TRUE))
sum(!is.na(QTL_08$gene_id)) # 72 annotated in Uniprot
sum(is.na(QTL_08$gene_id)) # 42 no annotated in Uniprot

colnames(QTL_08)
write.table(QTL_08, "~/Documents/Cesar/git/big_files/markers2.4.tsv", row.names = F, quote = F, sep = "\t")
write.table(QTL_08.1, "~/Documents/Cesar/git/big_files/markers2.2.tsv", row.names = F, quote = F, sep = "\t")

QTL_09 <- QTL_08 %>% group_by(gene_id) %>% summarise(Marker1 = paste(Marker1, collapse = ";")) 
QTL_10 <- QTL_08 %>% distinct(gene_id, .keep_all = TRUE) %>% dplyr::select(7:9)

QTL_10 <- inner_join(QTL_09, QTL_10, by = "gene_id")
QTL_10 <- na.omit(QTL_10)
head(QTL_10)
write.table(QTL_10, "~/Documents/Cesar/git/big_files/markers2.1.tsv", row.names = F, quote = F, sep = "\t")

# save image 
save.image(file = "~/Documents/Cesar/git/big_files/data_4.RData")
# data_4.RData is big (1.6 GB) I will reduce the size removing data_1, data_2, data_3, data_3.1
rm(list = c("data_1", "data_2", "data_3", "data_3.1", "data_5"))
save.image(file = "~/Documents/Cesar/git/big_files/data_5.RData")

####################

P1 <- read.csv("~/Documents/Cesar/git/big_files/pheno_MSC.csv")
P2 <- read.csv("~/Documents/Cesar/git/big_files/pheno_DM.csv")
P3 <- read.csv("~/Documents/Cesar/git/big_files/PH.csv")
P4 <- read.csv("~/Documents/Cesar/git/big_files/yield.csv")
P5 <- read.csv("~/Documents/Cesar/git/big_files/FD.csv")
colnames(P5)
R <- str_subset(colnames(P5), "_R_")
R1 <- c("gen", R)
P5 <- P5[,R1]
colnames(P1)
P1 <- P1[1:(length(P1)-3)]
P2 <- P2[1:(length(P2)-3)]
P3 <- P3[1:(length(P3)-3)]
P4 <- P4[1:(length(P4)-3)]

P6 <- P1 %>% inner_join(., P2) %>% inner_join(., P3) %>% inner_join(., P4) %>% inner_join(., P5)
P6$gen <- as.factor(P6$gen)

###########
# end

