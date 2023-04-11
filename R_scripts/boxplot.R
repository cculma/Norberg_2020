# boxplot
rm(list = ls())

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(RColorBrewer)
library(brew)
library(ggstatsplot)
library(ggcorrplot)

a1 <- read.table("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/markers2.3.tsv", sep = "\t", header = T)
a3 <- read.csv("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/FD.csv")

a5 <- read.csv("~/Documents/git/big_files/FD_cal.csv")
colnames(a5)
a5 <- a5 %>% dplyr::select(gen, ST1_FD_WA_2018_3)
a5 <- a5 %>% dplyr::select(-c("PC1","PC2","PC3"))


colnames(a3)
a3 <- a3[,c(1,30,32)]
a3 <- a3[,c(1,30:33)]
colnames(a3)[2:5] <- gsub("_S_", "_", colnames(a3)[2:5])
hist(a3$ST0_S_FD_ID_2019_4)

summary(a3$ST0_S_FD_ID_2019_4)
summary(a3$ST0_S_FD_WA_2019_5)
cor(a3$ST1_S_FD_ID_2019_4, a3$ST1_S_FD_WA_2019_5, use = "complete.obs")



str(a3)
a3$gen <- as.character(a3$gen)

colnames(a1)
a2 <- a1 %>% dplyr::select(Chr5_16888172, ST1_R_FD_WA_2018_3)

a2 <- a1 %>% dplyr::select(Chr5_16888158, ST1_R_FD_WA_2018_3)
a2 <- a1 %>% dplyr::select(Chr3_5642102, ST0_Yi_OR_2020_1) # FHY3

a2 <- a1 %>% dplyr::select(Chr6_79943661, ST0_Yi_ID_2019_1)
a2 <- a1 %>% dplyr::select(Chr6_37188399, ST0_Yi_WA_2018_3)
a2 <- a1 %>% dplyr::select(Chr5_81433854, ST0_Yi_ID_2019_1)

a2 <- a1 %>% dplyr::select(Chr5_16888158,ST3_R_FD_WA)


colnames(a1)

str(a2)
levels(a2$loc)
a4 <- a2 %>% dplyr::filter(marker %in% c(158)) %>% dplyr::filter(loc %in% c("WA"))

a2$Chr5_16888158 <- as.factor(a2$Chr5_16888158)
aggregate(a4$FD ~ a4$dosage, FUN=mean)
aggregate(a4$FD ~ a4$dosage, FUN=sd)



hist(a1$ST0_Yi_ID_2019_1)

a2[,1] <- as.factor(a2[,1])
levels(a2[,1])
a2 <- a2 %>% rownames_to_column("gen")
levels(a2$Chr3_5642102)
a2$Chr3_5642102 <- gsub("0", "1", a2$Chr3_5642102)
str(a2)
a2$Chr3_5642102 <- as.factor(a2$Chr3_5642102)
colnames(a2)

ggbetweenstats(data = a2, x = Chr3_5642102, y = ST0_Yi_OR_2020_1,
               plot.type = "box",
               type = "",
               pairwise.comparisons = TRUE,
               p.adjust.method = "fdr",
               package = "ggsci",
               palette = "nrc_npg", 
               point.args = list(alpha = 0),
               ggtheme = ggplot2::theme_classic(),
               outlier.tagging = TRUE,  outlier.label = gen)

ggplot(a2, aes(x=a2[,1], y=a2[,2])) + geom_boxplot()

str(a2)
a2$marker <- as.factor(a2$marker)
a2$dosage <- as.factor(a2$dosage)
levels(a2$marker)
levels(a2$dosage)
cc <- count(a2, dosage, marker)


grouped_ggbetweenstats(data = a2, x = dosage, y = ST3_FD_OR,
                       grouping.var = marker,
                       plot.type = "box",
                       type = "p",
                       pairwise.display = "s",
                       p.adjust.method = "fdr",
                       ggtheme = ggplot2::theme_classic(),
                       package = "ggsci",
                       palette = "default_jco",
                       outlier.tagging = TRUE,
                       outlier.label = gen,
                       plotgrid.args = list(nrow = 8))



head(a2)
colnames(a2) <- c("dosage" ,"response")
my_comparisons <- list( c("0", "1"), c("1", "2"), c("0", "2") )
my_comparisons <- list( c("1", "3"), c("1", "2"), c("2", "3") )

ggboxplot(a2, x = "dosage", y = "response", color = "dosage", palette = "jco") + stat_compare_means(comparisons = my_comparisons) + stat_compare_means(label.y = 15)

ggboxplot(a2, x = "dosage", y = "response", color = "dosage", palette = "jco") +  stat_compare_means(method = "t.test")

ggpaired(a2, x = "dosage", y = "response", color = "dosage", line.color = "gray", line.size = 0.4, palette = "jco")+ stat_compare_means(paired = TRUE)


Chr5_16888172
Chr5_16888195

a2 <- a1 %>% dplyr::select(gen, Chr5_16888158,
Chr5_16888160,
Chr5_16888161,
Chr5_16888163,
Chr5_16888169,
Chr5_16888170,
Chr5_16888188,
Chr5_16888190)
a2 <- a2 %>% gather(key = "marker", value = "dosage", 1:8)

a2 <- a2 %>% rownames_to_column("gen") %>% full_join(.,a3, by = "gen") %>% dplyr::filter(gen %in% c(25,166,168,46,75,136,28,42))
head(a2)


write.csv(a2, "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/markers2.5.csv", row.names = F, quote = F)

a2 <- a1 %>% dplyr::select(Chr5_16888158,
Chr5_16888160,
Chr5_16888161,
Chr5_16888169,
Chr5_16888170,
Chr5_16888172,
Chr5_16888188,
ST1_R_FD_WA_2018_3)

colnames(a2)
head(a2)
a2 <- a2 %>% gather(key = "marker", value = "dosage", 2:11) %>% gather(key = "loc", value = "FD", 2:3)


a2 <- a1 %>% dplyr::select(gen, Chr5_16888158,
                           Chr5_16888160,
                           Chr5_16888161,
                           Chr5_16888163,
                           Chr5_16888169,
                           Chr5_16888170,
                           Chr5_16888188,
                           Chr5_16888190)

a4 <- inner_join(a2, a5, by = "gen")
write.csv(a4, "Documents/git/Norberg_2020/BLUE_values/Markers_FD.csv", row.names = F, quote = F)

setwd("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/")
G <- read.csv("AllSamples_Ms_filter_q30_imputed_GWASPoly_contigRemoved.txt", header = TRUE, row.names = 1, check.names = F) 
G[1:5,1:5]
G1 <- G %>% unite(Chrom1, 1:2, remove = T)
G1 <- as.matrix(G1 %>% remove_rownames() %>% column_to_rownames(var = "Chrom1"))
G2 <- t(G1)
G2[1:5,1:5]
G2 <- as.data.frame(G2)
G2 <- G2 %>% rownames_to_column("gen")
G2 <- G2 %>% dplyr::select(gen, Chr5_16888158,
                           Chr5_16888160,
                           Chr5_16888161,
                           Chr5_16888163,
                           Chr5_16888169,
                           Chr5_16888170,
                           Chr5_16888188,
                           Chr5_16888190,
                           Chr5_16888195)
G2$gen <- as.integer(G2$gen)
a4 <- inner_join(G2, a5, by = "gen")
write.csv(a4, "~/Documents/git/Norberg_2020/BLUE_values/Markers_FD.csv", row.names = F, quote = F)


ggplot(a2, aes(x = a2[,1], y = a2[,3], fill = a2[,2])) + geom_boxplot(alpha = 0.6, outlier.shape = NA)  + scale_fill_manual(values = mycolors) + theme_ipsum(base_family = "Arial", base_size = 12) + theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 0, hjust = 0.95, vjust = 0.2)) + facet_grid( ~ a2[,2], scales = "free", space = "free") + labs(y = "FD_raw WA", x = "Allele dosage") + theme(panel.spacing = unit(0.3, "lines"))


colnames(a2)
head(a2)
str(a2)
lev1 <- c("gen", "marker", "dosage", "loc")
a2[,lev1] <- lapply(a2[,lev1], factor)
lev2 <- levels(a2$marker)
lev2 <- gsub("Chr5_16888", "", lev2)
levels(a2$marker) <- gsub("Chr5_16888", "", levels(a2$marker))
levels(a2$loc) <- gsub("ST3_S_FD_", "", levels(a2$loc))


P5 <- ggplot(a2, aes(x = dosage, y = FD, fill = marker)) + geom_boxplot(alpha = 0.6, outlier.shape = NA)  + scale_fill_manual(values = mycolors) + theme_ipsum(base_family = "Arial", base_size = 12) + theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 0, hjust = 0.95, vjust = 0.2)) + facet_grid(loc ~ marker, scales = "free", space = "free") + labs(y = "FD", x = "Allele dosage") + theme(panel.spacing = unit(0.4, "lines"))

a2[,2] <- as.factor(a2[,2])
a2[,3] <- as.factor(a2[,3])

# a2[,2] <- gsub("Chr5_1688", "", a2[,2])
# Chr5_16888158
# P5 <- ggplot(a2, aes(x = a2[,3], y = a2[,1], fill = a2[,2])) + geom_boxplot(alpha = 0.6, outlier.shape = NA)  + scale_fill_manual(values = mycolors) + theme_ipsum(base_family = "Arial", base_size = 12) + theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 0, hjust = 0.95, vjust = 0.2)) + facet_grid( ~ a2[,2], scales = "free", space = "free") + labs(y = "FD_raw WA", x = "Allele dosage") + theme(panel.spacing = unit(0.3, "lines"))

ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/figures/GWAS/FD_G27495.pdf", plot = P5, width = 8, height = 4, device = cairo_pdf)

a2 <- a1 %>% dplyr::select(Chr1_11444898, ST0_PH_WA_2020_1)
a2[,1] <- as.factor(a2[,1])
ggplot(a2, aes(x=a2[,1], y=a2[,2])) + geom_boxplot()

colnames(a2) <- c("dosage", "PH")
# Box plot facetted by "dose"
ggboxplot(a2, x = "dosage", y = "PH", short.panel.labs = FALSE) + stat_compare_means(method = "anova", label.y = 40) + stat_compare_means(label = "p.signif", method = "t.test", ref.group = "2")  

colnames(a1)
lev1 <- subset(colnames(a1),  grepl("^ST1_DM_", colnames(a1)))
lev0 <- gsub("DM", "Yi", lev1)

lev2 <- c(lev0, lev1)

qual_BLUP9 <- a1[,lev2]

qual_BLUP9$gen <- as.factor(qual_BLUP9$gen)
str(qual_BLUP9)
colnames(qual_BLUP9)
summary(qual_BLUP9)
ggscatterstats(
  data = qual_BLUP9, ## dataframe from which variables are taken
  x = ST1_Yi_ID_2019_1, ## predictor/independent variable
  y = ST1_DM_ID_2019_1, ## dependent variable
  xfill = "#CC79A7", ## fill for marginals on the x-axis
  yfill = "#009E73", ## fill for marginals on the y-axis
)

P00 <- cor(qual_BLUP9, use = "complete.obs")
ggcorrplot(P00[,ncol(P00):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + scale_fill_gradient(low = "white", high = "orangered") + theme_ipsum(base_family = "Arial", base_size = 10) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())


qual_BLUP9 <- a1[,lev1]
qual_BLUP9 <- qual_BLUP9 %>% rownames_to_column(var = "gen") %>% gather(key = "trait", value = "BLUP", 2:9)
head(qual_BLUP9)
qual_BLUP9$trait <- gsub("ST1_DM_", "", qual_BLUP9$trait)
qual_BLUP9 <- qual_BLUP9 %>% separate(2, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")
str(qual_BLUP9)
lev3 <- c("gen", "trait", "loc", "cut", "year")
qual_BLUP9[lev3] <- lapply(qual_BLUP9[lev3], factor)
levels(qual_BLUP9$trait)
qual_BLUP9 <- qual_BLUP9 %>% dplyr::filter(!loc %in% "ID")


ggplot(qual_BLUP9, aes(x = trait, y = BLUP, fill = trait)) + geom_boxplot(alpha = 0.6, outlier.shape = NA)  + scale_fill_manual(values = mycolors) + theme_ipsum(base_family = "Arial", base_size = 12) + theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + theme(panel.spacing = unit(0.4, "lines"))

ggbetweenstats(data = qual_BLUP9, x = trait, y = BLUP,
               plot.type = "box",
               type = "p",
               conf.level = 0.99,
               package = "ggsci",
               palette = "nrc_npg", 
               ggtheme = ggplot2::theme_classic(),
               outlier.tagging = TRUE,  outlier.label = gen)

grouped_ggbetweenstats(data = qual_BLUP9, x = year, y = BLUP,
                       grouping.var = loc,
                       xlab = "Env",
                       ylab = "BLUP",
                       plot.type = "box",
                       type = "p",
                       pairwise.display = "significant",
                       p.adjust.method = "fdr",
                       ggtheme = ggplot2::theme_classic(),
                       package = "ggsci",
                       palette = "default_jco",
                       outlier.tagging = TRUE,
                       outlier.label = gen,
                       plotgrid.args = list(nrow = 2)
)
?grouped_ggbetweenstats



# DM ----------------------------------------------------------------------

a4 <- read.csv("~/Documents/git/big_files/pheno_fa2.csv")
colnames(a4)
a5 <- a4 %>% dplyr::select("gen","ST1_DM_ID_2019_1")
colnames(a1)

a2 <- a1 %>% dplyr::select("gen","Chr4_62801591")
a5 <- inner_join(a2, a5, by = "gen")

colnames(a5) <- c("gen","dosage", "DM")

a5$dosage <- as.factor(a5$dosage)

a5 %>% dplyr::count(dosage)


compare_means(DM ~ dosage, data = a6,
              method = "t.test")


hist(a5$DM)

a6 <- a5 %>% dplyr::filter(DM < 20.8 & DM > 12)
a6 <- a6 %>% dplyr::filter(!gen == "60")

ggplot(a6, aes(x = a6[,2], y = a6[,3], fill = a6[,2])) + geom_boxplot(alpha = 0.6, outlier.shape = 1) 

ggboxplot(a6, x = "dosage", y = "DM", short.panel.labs = FALSE) + stat_compare_means(method = "anova", label.y = 40) + stat_compare_means(label = "p.signif", method = "t.test", ref.group = "2")  


# compare_traits ----------------------------------------------------------

a7 <- read.csv("~/Documents/git/big_files/pheno_fa.csv")

colnames(a7)

a7 <- a7 %>% dplyr::select(gen, ST0_Yi_ID_2019_1, ST0_Yi_ID_2019_4, ST0_PH_ID_2019_1, ST0_R_FD_ID_2019_4)

a7 <- a7 %>% dplyr::select(gen, ST1_Yi_ID_2019_1, ST1_Yi_ID_2019_4, ST1_PH_ID_2019_1, ST1_R_FD_ID_2019_4)

a7 <- a7 %>% dplyr::select(gen, ST1_Yi_ID_2019_1, ST1_Yi_ID_2019_4, ST1_PH_ID_2019_1, ST1_R_FD_ID_2019_4)

a7 <- a7 %>% column_to_rownames("gen")
cor(a7, use = "complete.obs")


a8 <- read.csv("~/Documents/git/Norberg_2020/Raw_data/ID_2019_1.csv")
a8 <- a8 %>% unite("gen_block", c(Treatment, Block), sep = "_", remove = T) %>% select(c(4,9,10))
a8 <- a8 %>% column_to_rownames("gen_block")
cor(a8, use = "complete.obs")


lev0 <- subset(colnames(a7), grepl("^ST1_PH", colnames(a7)))
lev1 <- subset(colnames(a7), grepl("^ST1_R_FD", colnames(a7)))
lev2 <- subset(colnames(a7), grepl("^ST1_Yi", colnames(a7)))
lev3 <- subset(colnames(a7), grepl("^ST1_DM", colnames(a7)))
lev4 <- subset(colnames(a7), grepl("^ST1_MS", colnames(a7)))


lev0 <- c("gen", lev0)
lev0 <- lev0[-6]

lev1 <- c("gen", lev1)
lev2 <- c("gen", lev2)
lev3 <- c("gen", lev3)
lev4 <- c("gen", lev4)

a8 <- a7[,lev0]
a8 <- a7[,lev1]
a8 <- a7[,lev2]
a8 <- a7[,lev3]
a8 <- a7[,lev4]

a8 <- a8 %>% column_to_rownames("gen")
a8 <- cor(a8, use = "complete.obs")
a8 <- round(a8, 2)
a8[upper.tri(a8)] <- NA

write.csv(a8, "~/Documents/git/Norberg_2020/BLUE_values/Cor_MS.csv", row.names = T, quote = F)


library(RColorBrewer)
# Define the number of colors you want
nb.cols <- length(unique(a2[,4]))
nb.cols <- 7
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)
# scale_fill_manual(values = mycolors)
