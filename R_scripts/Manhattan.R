# figure manhattan Norberg
# generate manhattan plot by trait

rm(list = ls(all = T))
library(plotly)
library(dplyr)
library(ggpubr)
library(GWASpoly)
library(ggplot2)

load("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/data_3.2.RData")

load("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/data_3.3.RData")

load("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/data_3.4.RData")



############### 
data_5.0 <- set.threshold(data_3.3, method= "Bonferroni", level=0.05)
data_5.1 <- set.threshold(data_3.4, method= "Bonferroni", level=0.05)

setwd("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/")

load("Yi_data_3.3.RData")
load("DM_data_3.3.RData")
load("MS_data_3.3.RData")
load("FD_data_3.3.RData")
load("PH_data_3.3.RData")


data_5.1 <- set.threshold(MS_data_3.3, method= "Bonferroni", level=0.05)
data_5.2 <- set.threshold(DM_data_3.3, method= "Bonferroni", level=0.05)
data_5.3 <- set.threshold(PH_data_3.3, method= "Bonferroni", level=0.05)
data_5.4 <- set.threshold(Yi_data_3.3, method= "Bonferroni", level=0.05)
data_5.5 <- set.threshold(FD_data_3.3, method= "Bonferroni", level=0.05)

data_5.1@map$Chrom <- gsub("Chr", "", data_5.1@map$Chrom)
data_5.1@map$Chrom <- as.factor(data_5.1@map$Chrom)
data_5.2@map$Chrom <- gsub("Chr", "", data_5.2@map$Chrom)
data_5.2@map$Chrom <- as.factor(data_5.2@map$Chrom)
data_5.3@map$Chrom <- gsub("Chr", "", data_5.3@map$Chrom)
data_5.3@map$Chrom <- as.factor(data_5.3@map$Chrom)
data_5.4@map$Chrom <- gsub("Chr", "", data_5.4@map$Chrom)
data_5.4@map$Chrom <- as.factor(data_5.4@map$Chrom)
data_5.5@map$Chrom <- gsub("Chr", "", data_5.5@map$Chrom)
data_5.5@map$Chrom <- as.factor(data_5.5@map$Chrom)



QTL_1 <- get.QTL(data_5.1)
QTL_2 <- get.QTL(data_5.2)
QTL_3 <- get.QTL(data_5.3)
QTL_4 <- get.QTL(data_5.4)
QTL_5 <- get.QTL(data_5.5)


cc1 <- count(QTL_1,Trait)
length(cc1$Trait)
cc2 <- count(QTL_2,Trait)
length(cc2$Trait)
cc3 <- count(QTL_3,Trait)
length(cc3$Trait)
cc4 <- count(QTL_4,Trait)
length(cc4$Trait)
cc5 <- count(QTL_5,Trait)
lev5 <- cc5$Trait
lev5 <- subset(lev5,  grepl("_R_", lev5))


P1 <- manhattan.plot(data = data_5.1, traits = cc1$Trait) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank())

P2 <- manhattan.plot(data = data_5.2, traits = cc2$Trait) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank())

P3 <- manhattan.plot(data = data_5.3, traits = cc3$Trait) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank())

P4 <- manhattan.plot(data = data_5.4, traits = cc4$Trait) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank())

P5 <- manhattan.plot(data = data_5.5, traits = lev5) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank())


ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/figures/GWAS/MS_manhattan.png", plot = P1, dpi = 300, width = 8, height = 8)

ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/figures/GWAS/DM_manhattan.png", plot = P2, dpi = 300, width = 6, height = 4)

ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/figures/GWAS/PH_manhattan.png", plot = P3, dpi = 300, width = 8, height = 8)

ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/figures/GWAS/Yi_manhattan.png", plot = P4, dpi = 300, width = 8, height = 8)

ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/figures/GWAS/FD_manhattan.png", plot = P5, dpi = 300, width = 6, height = 4)


P6 <- manhattan.plot(data = data_5.5, traits = "ST3_R_FD_WA", chrom = "5") + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank(), strip.text.x = element_blank())

ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/figures/GWAS/FD_5.pdf", plot = P6, dpi = 300, width = 5, height = 3)


manhattan.plot(data = data_5.5, traits = lev5) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank())



# assay
load("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/data_4.RData")
data_5.0 <- set.threshold(data_3.2, method= "Bonferroni", level=0.1)
data_5.1 <- set.threshold(data_4, method= "Bonferroni", level=0.1)

T1 <- cc$Trait
T2 <- T1[1:12]
T3 <- T1[13:24]
T4 <- T1[25:30]


manhattan.plot(data = data_5.4, traits = c("ST0_Yi_OR_2020_1", "ST1_Yi_OR_2020_1"), chrom = "3") + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank(), strip.text.x = element_blank())

cc2 <- count(QTL_4, Trait)
cc3 <- count(QTL_5, Trait)
cc3$Trait

cc3 <- c("ST1_R_FD_WA_2018_3", "ST1_R_FD_WA_2019_5", "ST3_R_FD_ID", "ST3_R_FD_WA") 

P9 <- manhattan.plot(data = data_5.5, traits = cc3 ) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.text.x = element_blank(), strip.background = element_rect(fill = "white", color = "white")) + geom_label(data = labels, aes(label=label1), x = -Inf, y = Inf, hjust=0.1, vjust=0.9, inherit.aes = FALSE, label.size = NA)


ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/figures/poster/FD_manhattan.png", plot = P9, dpi = 300, width = 6, height = 4)


P8 <- manhattan.plot(data = FD_data_3.3, traits= c("ST3_R_FD_WA"), chrom = "Chr5") + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank()) + geom_point(shape=1, alpha = 0.2, size=3)

geom_point(alpha = 0.5)
ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/figures/GWAS/Chr5_G27495.pdf", plot = P8, width = 4, height = 3, device = cairo_pdf)


P8 <- manhattan.plot(data = data_5.1, traits= c("ST1_MS_WA_2020_2", "ST1_He_OR_2019_2")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank()) 




qq.plot(data = data_5.1, trait="ST1_MS_WA_2020_2") 
qq.plot(data = data_5.1, trait="ST1_He_OR_2019_2") 

#~~~~~~~~~

load("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/ST1_1MSC.RData")
data_6.2 <- set.threshold(ST1_1MSC_1, method= "Bonferroni", level=0.2)




manhattan.plot(data = data_5.0) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())

#~~~~~~~~~


P9 <- manhattan.plot(data = data_5.0, traits= T2) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())

P10 <- manhattan.plot(data = data_5.0, traits= T3) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())

P11 <- manhattan.plot(data = data_5.0, traits= T4) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())

setwd("~/Documents/git/Norberg_2020/GWAS_results/Figures_3.3/")
ggsave(filename = "myplot1.jpg", plot = P9, width = 12, height = 9)
ggsave(filename = "myplot2.jpg", plot = P10, width = 12, height = 9)
ggsave(filename = "myplot3.jpg", plot = P11, width = 9, height = 6)


P12 <- manhattan.plot(data = data_5, traits=c ("MET_jun")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
P13 <- manhattan.plot(data = data_5, traits=c ("MET_jul")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
P14 <- manhattan.plot(data = data_5, traits=c ("MET_aug")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
P15 <- manhattan.plot(data = data_5, traits=c ("MET_sep")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())


Q1 <- manhattan.plot(data = data_5, traits=c ("MET_20")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
Q2 <- manhattan.plot(data = data_5, traits=c ("MET_21")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
Q3 <- manhattan.plot(data = data_5, traits=c ("FA1_all")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
Q4 <- manhattan.plot(data = data_5, traits=c ("PH_19_1stage")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
Q5 <- manhattan.plot(data = data_5, traits=c ("PH_20_1stage")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
Q6 <- manhattan.plot(data = data_5, traits=c ("FA1_PH")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
Q7 <- manhattan.plot(data = data_5, traits=c ("CT_20_1stage")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())


myplot1 <- ggarrange(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15,
                     labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o"), ncol = 5, nrow = 3)
myplot2 <- ggarrange(Q1, Q2, Q3, Q4, Q5, Q6, Q7, 
                     labels = c("a", "b", "c", "d", "e", "f", "g"), ncol = 3, nrow = 3)

ggsave(filename = "myplot1.jpg", plot = myplot1, width = 15, height = 9)
ggsave(filename = "myplot2.jpg", plot = myplot2, width = 9, height = 9)


myplot1 <- LD.plot(data_5) + theme_classic(base_family = "Arial", base_size = 12) + ggtitle("LD plot")

ggsave(filename = "myplot1.jpg", plot = myplot1, width = 6, height = 6)
