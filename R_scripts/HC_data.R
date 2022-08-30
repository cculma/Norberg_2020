# High correlation traits
library(data.table)
library(tidyverse)
library(GWASpoly)
library(ggpubr)

# PH_WA
lev6 <- c("ST1_PH_WA_2019_5","ST1_PH_WA_2019_4","ST1_PH_WA_2019_3",
          "ST1_PH_WA_2020_3","ST1_PH_WA_2020_2","ST1_PH_WA_2019_1",
          "ST1_FD_WA_2019_5","ST1_PH_WA_2020_5")
# Yi_WA
lev7 <- c("ST1_Yi_WA_2018_3","ST1_Yi_WA_2019_2","ST1_Yi_WA_2019_3",
          "ST1_Yi_WA_2019_4","ST1_Yi_WA_2019_5","ST1_Yi_WA_2020_2",
          "ST1_Yi_WA_2020_3","ST1_Yi_WA_2020_4")

# Yi_OR
lev5 <- c("ST1_Yi_OR_2019_2","ST1_Yi_OR_2020_4","ST1_Yi_OR_2020_2")
# FD_OR
lev4 <- c("ST1_FD_OR_2019_4","ST1_PH_OR_2019_3","ST1_FD_OR_2020_4",
          "ST1_FD_OR_2018_3")

setwd("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/")
load("Yi_OR_HC_data_3.3.RData")
load("Yi_WA_HC_data_3.3.RData")
load("FD_OR_HC_data_3.3.RData")
load("PH_WA_HC_data_3.3.RData")

data_5.1 <- set.threshold(Yi_HC_OR_data_3.3, method= "Bonferroni", level=0.05)
data_5.2 <- set.threshold(Yi_HC_data_3.3, method= "Bonferroni", level=0.05)
data_5.3 <- set.threshold(FD_OR_HC_data_3.3, method= "Bonferroni", level=0.05)
data_5.4 <- set.threshold(PH_WA_HC_data_3.3, method= "Bonferroni", level=0.05)


data_5.1@map$Chrom <- gsub("Chr", "", data_5.1@map$Chrom)
data_5.1@map$Chrom <- as.factor(data_5.1@map$Chrom)

data_5.2@map$Chrom <- gsub("Chr", "", data_5.2@map$Chrom)
data_5.2@map$Chrom <- as.factor(data_5.2@map$Chrom)

data_5.3@map$Chrom <- gsub("Chr", "", data_5.3@map$Chrom)
data_5.3@map$Chrom <- as.factor(data_5.3@map$Chrom)

data_5.4@map$Chrom <- gsub("Chr", "", data_5.4@map$Chrom)
data_5.4@map$Chrom <- as.factor(data_5.4@map$Chrom)




QTL_01 <- get.QTL(data_5.1)
QTL_02 <- get.QTL(data_5.2)
QTL_03 <- get.QTL(data_5.3)
QTL_04 <- get.QTL(data_5.4)

QTL_01$Trait <- gsub("yield", "Yi_OR", QTL_01$Trait)
QTL_02$Trait <- gsub("yield", "Yi_WA", QTL_02$Trait)
QTL_03$Trait <- gsub("FD", "FD_OR", QTL_03$Trait)

QTL_01 <- rbind(QTL_01, QTL_02, QTL_03, QTL_04)
rm(QTL_02)
rm(QTL_03)
rm(QTL_04)

#############

QTL_03 <- QTL_01 %>% group_by(Marker) %>% top_n(1, abs(Score)) %>% dplyr::select(Marker, Score) %>% distinct(Marker, .keep_all = TRUE)
QTL_04 <- QTL_01 %>% group_by(Marker) %>% summarise(Trait = paste(Trait, collapse = ";")) 
QTL_05 <- QTL_01 %>% group_by(Marker) %>% summarise(Model = paste(Model, collapse = ";")) 
QTL_06 <- QTL_01 %>% dplyr::select(Marker, Chrom, Position, Ref, Alt) %>% distinct(Marker, .keep_all = TRUE) %>% unite(col = "SNP", 5:4, sep = "/", remove = T) %>% unite(col = "Marker1", 2:3, sep = "_", remove = T) %>% inner_join(., QTL_03, by = "Marker")


S1 <- QTL_01 %>% dplyr::select(2,4) %>% distinct(Marker, Model, .keep_all = T) 
S1 <- dcast(S1, formula = Marker ~ Model, fun.aggregate = length)
colnames(S1)

S2 <- QTL_01 %>% dplyr::select(1,4) %>% distinct(Marker, Trait, .keep_all = T) 
S2 <- dcast(S2, formula = Marker ~ Trait, fun.aggregate = length)
colnames(S2)

# QTL_08 <- QTL_01 %>% separate(1, c("stage", "trait", "loc", "year", "cut"), sep = "_", remove = T, convert = FALSE, extra = "merge")  %>% dplyr::select(3,4,5,8)  %>% distinct(Marker, .keep_all = T) 

# QTL_08 <- QTL_01 %>% dplyr::select(3,4,5,8)  %>% distinct(Marker, .keep_all = T) 

# QTL_07 <- right_join(QTL_07, QTL_08, by = "Marker") 

S3 <- inner_join(QTL_06, S1, by = "Marker") %>% inner_join(., S2, by = "Marker") 

S3 <- inner_join(QTL_06, S1, by = "Marker") %>% left_join(., fit_07, by = "Marker") 
colnames(S3)
S3 <- S3[,c(1:4,11:18,5:9,19,20)]

QTL_07$totalM <- rowSums(S3[13:length(S3)])
write.table(S3, "~/Documents/Cesar/git/big_files/markers1.4.tsv", row.names = F, quote = F, sep = "\t")


col_headings_3 <- c("stage", "trait", "loc", "year", "cut")
S4 <- as.data.frame(colnames(S2)) %>% separate(1, col_headings_3, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% dplyr::filter(!row_number() %in% c(1))
write.csv(S4, "GWAS_results/markers_FDcor.csv")

write.table(S4, "~/Documents/Cesar/git/big_files/markers1.2.tsv", row.names = F, quote = F, sep = "\t")


#############

M1 <- manhattan.plot(data = data_5.1) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank(), strip.text.x = element_blank())


M2 <- manhattan.plot(data = data_5.2) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank(), strip.text.x = element_blank())

M3 <- manhattan.plot(data = data_5.3) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank(), strip.text.x = element_blank())

M4 <- manhattan.plot(data = data_5.4) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank(), strip.text.x = element_blank())

M5 <- manhattan.plot(data = data_5.3, chrom = "Chr8") + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank(), strip.text.x = element_blank())

M6 <- manhattan.plot(data = data_5.2, chrom = "Chr7") + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank(), strip.text.x = element_blank())

manhattan.plot(data = data_5.2, chrom = "Chr6") + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank(), strip.text.x = element_blank())

myplot1 <- ggarrange(M1, M3, M2, M4, M5, M6, 
                     labels = c("a", "b", "c", "d", "e", "f"), 
                     ncol = 3, nrow = 2, 
                     font.label = list(size = 14, face = "plain"))
# a Yi_OR
# b FD_OR
# c Yi_WA
# d PH_WA
# e FD_OR Chr8
# f Yi_WA Chr7

ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/figures/HC/M1.png", plot = myplot1, dpi = 300, width = 6, height = 4)

data_5.1 <- set.threshold(Yi_HC_OR_data_3.3, method= "Bonferroni", level=0.1)
data_5.2 <- set.threshold(Yi_HC_data_3.3, method= "Bonferroni", level=0.1)
data_5.3 <- set.threshold(FD_OR_HC_data_3.3, method= "Bonferroni", level=0.1)
data_5.4 <- set.threshold(PH_WA_HC_data_3.3, method= "Bonferroni", level=0.1)

#############
# R2
QTL_01 <- get.QTL(data_5.1)
QTL_02 <- get.QTL(data_5.2)
QTL_03 <- get.QTL(data_5.3)
QTL_04 <- get.QTL(data_5.4)


cc <- count(QTL_04,Trait)
lev8 <- cc$Trait
lev8


QTL_04 <- QTL_04 %>% dplyr::filter(!Model %in% c("diplo-general", "diplo-additive"))

fit_05 <- fit.QTL(data=data_5.4, trait = lev8,
                  qtl=QTL_04[,c("Marker","Model")])

fit_05$trait <- "PH_WA"
# fit_06 <- fit_05
fit_06 <- rbind(fit_06, fit_05)
fit_07 <- fit_06[,c(1,5:7)] %>% group_by(Marker) %>% top_n(1, abs(R2)) 


############
# To annotate markers
# load gene annotation Medicago sativa Zhongmu No1
load("~/Documents/Cesar/RNA/globus/lordec_reports/lordec_trim/bed_Shen/ORF_NMD/i_5.2.8.RData")
i_5.2.8 <- i_5.2.8 %>% dplyr::select(1,3)
head(i_5.2.8)

# GRanges
# file <- ("~/Documents/Cesar/RNA/globus/lordec_reports/lordec_trim/bed_Shen/ORF_NMD/blast_corrected_shen.bed")
file <- ("~/OneDrive - Washington State University (email.wsu.edu)/RNA/Ms_Shen/blast_corrected_shen.bed")

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

QTL_08 <- full_join(S3, df3, by = "Marker1") 

write.table(QTL_08, "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/markers_HC.tsv", row.names = F, quote = F, sep = "\t")

##############



