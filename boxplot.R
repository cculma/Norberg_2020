# boxplot
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(brew)
a1 <- read.table("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/markers2.3.tsv", row.names = 1, sep = "\t", header = T)

colnames(a1)
a2 <- a1 %>% dplyr::select(Chr5_16888172, ST1_R_FD_WA_2018_3)

a2 <- a1 %>% dplyr::select(Chr5_16888172, ST1_R_FD_WA_2018_3)
a2 <- a1 %>% dplyr::select(Chr5_16888158, ST1_R_FD_WA_2018_3)
a2 <- a1 %>% dplyr::select(Chr5_16888160, ST1_R_FD_WA_2018_3)
a2 <- a1 %>% dplyr::select(Chr5_16888161, ST1_R_FD_WA_2018_3)
a2 <- a1 %>% dplyr::select(Chr5_16888169, ST1_R_FD_WA_2018_3)
a2 <- a1 %>% dplyr::select(Chr5_16888170, ST1_R_FD_WA_2018_3)


a2 <- a1 %>% dplyr::select(Chr6_15133674, ST3_PH_ID)
a2[,1] <- as.factor(a2[,1])
ggplot(a2, aes(x=a2[,1], y=a2[,2])) + geom_boxplot()

a2 <- a1 %>% dplyr::select(Chr5_16888158,
Chr5_16888160,
Chr5_16888161,
Chr5_16888163,
Chr5_16888169,
Chr5_16888170,
Chr5_16888172,
Chr5_16888188,
Chr5_16888190,
Chr5_16888195,ST3_R_FD_WA)
a2 <- a2 %>% gather(key = "marker", value = "dosage", 1:10)


a2 <- a1 %>% dplyr::select(Chr5_16888158,
Chr5_16888160,
Chr5_16888161,
Chr5_16888169,
Chr5_16888170,
Chr5_16888172,
Chr5_16888188,
ST1_R_FD_WA_2018_3)

colnames(a2)
a2 <- a2 %>% gather(key = "marker", value = "dosage", 1:7)

a2 <- a1 %>% dplyr::select(Chr5_45222918,ST0_S_FD_WA_2018_3)



colnames(a2)
a2 <- a2 %>% gather(key = "trait", value = "PH", 2:4)
a2[,1] <- as.factor(a2[,1])
a2[,2] <- as.factor(a2[,2])

ggplot(a2, aes(x = a2[,1], y = a2[,3], fill = a2[,2])) + geom_boxplot(alpha = 0.6, outlier.shape = NA)  + scale_fill_manual(values = mycolors) + theme_ipsum(base_family = "Arial", base_size = 12) + theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 0, hjust = 0.95, vjust = 0.2)) + facet_grid( ~ a2[,2], scales = "free", space = "free") + labs(y = "FD_raw WA", x = "Allele dosage") + theme(panel.spacing = unit(0.3, "lines"))



a2[,2] <- as.factor(a2[,2])
a2[,3] <- as.factor(a2[,3])

a2[,2] <- gsub("Chr5_1688", "", a2[,2])
Chr5_16888158
P5 <- ggplot(a2, aes(x = a2[,3], y = a2[,1], fill = a2[,2])) + geom_boxplot(alpha = 0.6, outlier.shape = NA)  + scale_fill_manual(values = mycolors) + theme_ipsum(base_family = "Arial", base_size = 12) + theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 0, hjust = 0.95, vjust = 0.2)) + facet_grid( ~ a2[,2], scales = "free", space = "free") + labs(y = "FD_raw WA", x = "Allele dosage") + theme(panel.spacing = unit(0.3, "lines"))

ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/figures/GWAS/FD_G27495.pdf", plot = P5, width = 8, height = 3, device = cairo_pdf)

a2 <- a1 %>% dplyr::select(Chr1_11444898, ST0_PH_WA_2020_1)
a2[,1] <- as.factor(a2[,1])
ggplot(a2, aes(x=a2[,1], y=a2[,2])) + geom_boxplot()

colnames(a2) <- c("dosage", "PH")
# Box plot facetted by "dose"
ggboxplot(a2, x = "dosage", y = "PH", short.panel.labs = FALSE) + stat_compare_means(method = "anova", label.y = 40) + stat_compare_means(label = "p.signif", method = "t.test", ref.group = "2")  

# + geom_boxplot(alpha = 0.6) + scale_fill_viridis(discrete = T) + scale_color_viridis(discrete = T) + theme_ipsum(base_family = "Arial", base_size = 8) + theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank()) 


library(RColorBrewer)
# Define the number of colors you want
nb.cols <- length(unique(a2$trait))
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)
scale_fill_manual(values = mycolors)
