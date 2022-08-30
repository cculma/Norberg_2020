# highly correlated traits

rm(list = ls())

library(corrplot)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)
library(ggcorrplot)
library(ggpubr)
library(caret)
library(RColorBrewer)
library(viridis)
library(wesanderson)
library(ggdendro)
library(factoextra)

a1 <- read.csv("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/markers2.3.tsv", sep = '\t', row.names = 1)
colnames(a1)
colnames(a1)[239:254] <- gsub("_R_", "_", colnames(a1)[239:254])
colnames(a1)[135] <- gsub("_4", "_3", colnames(a1)[135])
colnames(a1)[154] <- gsub("_4", "_3", colnames(a1)[154])


a1 <- read.csv("~/Documents/git/big_files/pheno_fa.csv")
a2 <- read.csv("~/Documents/git/big_files/pheno.csv")

a1 <- read.csv("~/Documents/git/big_files/yield.csv")
a2 <- read.csv("~/Documents/git/big_files/Sum_yield.csv")
colnames(a1)
colnames(a2)
a1 <- a1[1:(length(colnames(a1))-3)]
a1 <- inner_join(a1, a2, by = "gen")
colnames(a1)

# lev1 <- subset(colnames(a06),  grepl("ID_2019_1$", colnames(a2)))
lev0 <- subset(colnames(a1),  grepl("^ST0_", colnames(a1)))
lev1 <- subset(colnames(a1),  grepl("^ST1_", colnames(a1)))

a00 <- a1[,lev0]
a01 <- a1[,lev1]
colnames(a00)
colnames(a01)


colnames(a00) <- gsub("ST0_", "", colnames(a00))
colnames(a01) <- gsub("ST1_", "", colnames(a01))
a00 <- a00[ , order(names(a00))]
a01 <- a01[ , order(names(a01))]


P00 <- cor(a00, use = "complete.obs")
P01 <- cor(a01, use = "complete.obs")

# P00[lower.tri(P00, diag=TRUE)] <- NA
# P01[lower.tri(P01, diag=TRUE)] <- NA

P00[lower.tri(P00)] <- P01[lower.tri(P01)]

P1 <- ggcorrplot(P00[,ncol(P00):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + theme_classic(base_family = "Arial", base_size = 12) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank()) + labs(title = "Single-Stage vs Stage-Wise")


P02 <- na.omit(c(P00))
P03 <- na.omit(c(P01))

P04 <- data.frame(ST0 = P02, ST1 = P03)
P05 <- P04 %>% gather(key = "Stage", value = "Pearson", 1,2)



ggcorrplot(P00[,ncol(P00):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + scale_fill_gradient(low = "white", high = "orangered") + theme_ipsum(base_family = "Arial", base_size = 10) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggcorrplot(P01[,ncol(P01):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + scale_fill_gradient(low = "white", high = "orangered") + theme_ipsum(base_family = "Arial", base_size = 10) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())


b00 <- a1[,Q10]
b01 <- a1[,P11]

P00 <- cor(b00, use = "complete.obs")
P01 <- cor(b01, use = "complete.obs")


?hclust

library(ggridges)

head(P05)

# here is the main change: 
P05 <- P05 %>% mutate(Stage = fct_relevel(Stage, levels = "ST0","ST1"))
ggplot(P05, aes(x = Pearson, y = Stage)) +
  geom_density_ridges(aes(fill = Stage)) +
  scale_fill_manual(values = c( "#E7B800","#00AFBB")) +
  theme_classic(base_family = "Arial", base_size = 12) + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.2)) + geom_vline(data=mu, aes(xintercept=grp.mean), linetype="dashed")

library(plyr)
mu <- ddply(P05, "Stage", summarise, grp.mean=mean(Pearson))
head(mu)  

# Change density plot fill colors by groups
ggplot(P05, aes(x=Pearson, fill=Stage)) + geom_density(alpha=0.6) + geom_vline(data=mu, aes(xintercept=grp.mean), linetype="dashed") +
  scale_fill_manual(values = c( "#00AFBB","#E7B800")) +
  theme_classic(base_family = "Arial", base_size = 12) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.2))


# 6 3

P00 <- cor(a00, use = "complete.obs")
P01 <- cor(a01, use = "complete.obs")

P06 <- findCorrelation(P00, cutoff = 0.7, verbose = T, names = T, exact = T)
P07 <- findCorrelation(P01, cutoff = 0.7, verbose = T, names = T, exact = T)
P06 <- P06[-c(4,8)]
P07 <- P07[-c(10,23)]

# P08 <- gsub("ST1", "ST0", P07)

Q00 <- a00[,P06]
Q01 <- a01[,P07]

Q02 <- cor(Q00, use = "complete.obs")
Q03 <- cor(Q01, use = "complete.obs")

ggcorrplot(Q02, hc.order = TRUE, type = "lower",lab = TRUE, lab_size = 2) + theme_ipsum(base_family = "Arial", base_size = 8) + scale_fill_gradient(low = "white", high = "#00AFBB") + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggcorrplot(Q03, hc.order = TRUE, type = "lower",lab = TRUE, lab_size = 2) + theme_ipsum(base_family = "Arial", base_size = 8) + scale_fill_gradient(low = "white", high = "#E7B800") + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

hc0 <- hclust(as.dist(1-Q02))
hc1 <- hclust(as.dist(1-Q03))



hc0 <- hclust(as.dist(1-P00), method = "complete")
hc1 <- hclust(as.dist(1-P01), method = "complete")
"complete"
"ward.D2"
"single"
"average"
plot(hc0, hang = -1, cex = 0.6)
plot(hc1, hang = -1, cex = 0.6)

ggdendrogram(hc0) + theme_ipsum(base_family = "Arial", base_size = 8) + theme(axis.text.x = element_text(angle = 0, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + coord_flip()

ggdendrogram(hc1, segments = T) + theme_ipsum(base_family = "Arial", base_size = 8) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + abline(v=0.35)



fviz_dend(hc1, cex = 0.3, k = 12, k_colors ="black", color_labels_by_k = FALSE, rect = TRUE) 
fviz_dend(hc0, cex = 0.3, k = 12, k_colors ="black", color_labels_by_k = FALSE, rect = TRUE)



lev4 <- c("ST1_FD_OR_2019_4","ST1_PH_OR_2019_3","ST1_FD_OR_2020_4",
          "ST1_FD_OR_2018_3")
lev5 <- c("ST1_Yi_OR_2019_2","ST1_Yi_OR_2020_4","ST1_Yi_OR_2020_2")

lev6 <- c("ST1_PH_WA_2019_5","ST1_PH_WA_2019_4","ST1_PH_WA_2019_3",
          "ST1_PH_WA_2020_3","ST1_PH_WA_2020_2","ST1_PH_WA_2019_1",
          "ST1_FD_WA_2019_5","ST1_PH_WA_2020_5")




a09 <- a1[,lev4]
a09 <- a1[,lev5]
a09 <- a1[,lev6]


colnames(a09)
a09 <- a09 %>% rownames_to_column(var = "gen") %>% gather(key = "env", value = "PH_WA", 2:9)
head(a09)
a09 <- a09[,c(1,3,2)]
write.csv(a09, "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/PH_WA.csv", row.names = F, quote = F)

