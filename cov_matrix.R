# plot covariance matrix
rm(list = ls())

library(corrplot)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)
library(ggcorrplot)
library(ggpubr)

library(RColorBrewer)
library(viridis)
library(wesanderson)

setwd("~/Documents/Cesar/git/Norberg_2020/BLUE_values/FA/6_cov_matrix/")
# mac
setwd("~/Documents/git/Norberg_2020/BLUE_values/FA/6_cov_matrix/")

data_COV <- list.files(pattern = ".csv", full.names = T)
list_2 <- gsub(".csv", "", gsub("./", "", data_COV))

COV_1 <- list()
for (i in 1:length(data_COV)) {
  data <- read.csv(data_COV[i], row.names = 1, check.names = F)
  COV_1[[length(COV_1)+1]] = data
}
names(COV_1) <- list_2

# DM_2 <-rbindlist(DM_1, use.names=TRUE, fill=TRUE, idcol="merged")
# write.csv(DM_2, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/2_DM_1stage.csv", row.names = F, quote = F)

################
# plots of different MET values

P2 <- cor(P1)
corrplot(P2, type="upper", method = 'number')
P3 <- COV_1[[1]]
dim(P3)
class(P3)
# plot 
ggcorrplot(P3[,13:1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = F) + scale_fill_gradient(low = "white", high = "steelblue") + theme_ipsum() + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2))



##################
library(metan)

a1 <- read.csv("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/pheno_fa.csv", row.names = 1)
a1 <- read.csv("~/Documents/git/pheno_fa2.csv", row.names = 1)
a1 <- read.csv("~/Documents/git/pheno_fa2.csv")
a1 <- a1[1:(length(colnames(a1))-3)]
colnames(a1)

a2 <- a1[,c(1:78)]
colnames(a2)
a2 <- a2 %>% dplyr::select(1,23:44,76:78)
a2 <- a2 %>% dplyr::select(1,23:44,76:78)
a2 <- a2 %>% dplyr::select(1,23:44,76:78)


a3 <- a2 %>% gather(key = "trait", value = "BLUE", 2:26)
str(a3)
a3[,c("gen","trait")] <- lapply(a3[,c("gen","trait")], factor)

inspect(a3)
gge1 <- a3 %>% gge(env = trait, gen = gen, centering = 1, scaling = 1, svp = 2) 
gge2 <- plot(gge1, type = 8, plot_theme = theme_metan_minimal())

class(gge2)

a2 <- a1[,c(1:77)]
colnames(a2)
a3 <- a2 %>% dplyr::select(1:21)
colnames(a2)[c(24,28,31)]

a3 <- a2 %>% dplyr::select(1:21)
a3 <- a2 %>% dplyr::select(22:77)
a3 <- a2 %>% dplyr::select(22:43,75:77)
a3 <- a2 %>% dplyr::select(1:21)
a3 <- a2 %>% dplyr::select(23:31)
a3 <- a2 %>% dplyr::select(24,28,31,75:77)


colnames(a3)
colnames(a3) <- c("ST1_FD_OR_2018_3", "ST1_FD_OR_2019_4", "ST1_FD_OR_2020_4", "ST1_FD_ID_2019_4", "ST1_FD_WA_2018_3","ST1_FD_WA_2019_5")

colnames(a3) <- c("OR_2018", "OR_2019", "OR_2020", "ID_2019", "WA_2018","WA_2019")
a3 <- a3[,c(4,1:3,5:6)]
P2 <- cor(a3, use = "complete.obs")
P3 <- cov(a3, use = "complete.obs")

ggcorrplot(P2[,ncol(P2):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 3, show.diag = T)  + scale_fill_gradient(low = "white", high = "steelblue") + theme_ipsum(base_family = "Arial", base_size = 12) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank())

ggcorrplot(P3[,ncol(P2):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 3, show.diag = T) + scale_fill_gradient(low = "white", high = "orangered") + theme_ipsum(base_family = "Arial", base_size = 12) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank())

# 5 X 5

####

a4 <- a3 %>% rownames_to_column(var = "gen")
a4 <- a4 %>% gather(key = "trait", value = "BLUE", 2:length(a4))
a4[,c("gen","trait")] <- lapply(a4[,c("gen","trait")], factor)

gge1 <- a4 %>% gge(env = trait, gen = gen, centering = 2, scaling = 0, svp = 2) 
plot(gge1, type = 3, col.gen = "steelblue", col.env = "black", 
    size.text.env = 3,size.text.gen = 2, size.shape.win = 3, size.text.win = 3,
     title = T, plot_theme = theme_metan_minimal()) + theme_ipsum(base_family = "Arial", base_size = 12)


?ge_winners
a5 <- a4 %>% dplyr::filter(gen %in% W1$BLUE)
W2 <- a4 %>% ge_winners(env = trait, gen = gen, resp = BLUE, better = "l")
W1 <- a4 %>% ge_winners(env = trait, gen = gen, resp = BLUE, better = "h")
head(W1)
W1$BLUE
W2$BLUE
c(W1$BLUE, W2$BLUE)
a5 <- a4 %>% dplyr::filter(gen %in% c(W1$BLUE, W2$BLUE))
a5 %>% ge_plot(env = trait, gen = gen, resp = BLUE, type = 1) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank())
a5 %>% ge_plot(env = trait, gen = gen, resp = BLUE, type = 2) + theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank())


####
# size.shape.win = 1, large_label = 1,

# MS = 1_MSC
P1 <- a1 %>% dplyr::select(1,2:14) %>% column_to_rownames(var = "gen")

# DM = 2_DM
P1 <- a1 %>% dplyr::select(1,15:22) %>% column_to_rownames(var = "gen")

# He = 3_Height
P1 <- a1 %>% dplyr::select(1,23,24,26:28,30,31,33:44) %>% column_to_rownames(var = "gen")
colnames(P1)
colnames(P1) <- gsub("ST1_He_", "", colnames(P1))

P2 <- cor(P1, use = "complete.obs")
P3 <- cov(P1, use = "complete.obs")

P2[upper.tri(P2, diag=TRUE)] <- NA
P3[lower.tri(P3, diag=FALSE)] <- NA


?upper.tri

(m2 <- matrix(1:20, 4, 5))
lower.tri(m2)
m2[lower.tri(m2)] <- NA
m2


ggcorrplot(P2[,ncol(P2):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T)  + scale_fill_gradient(low = "white", high = "steelblue") + theme_ipsum(base_family = "Arial", base_size = 8) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank())

ggcorrplot(P3[,ncol(P3):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + scale_fill_gradient(low = "white", high = "orangered") + theme_ipsum(base_family = "Arial", base_size = 8) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank())


# 6X6


# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(P2, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()




# Yi = 4_Yield
P1 <- a1 %>% dplyr::select(1,45:75) %>% column_to_rownames(var = "gen")

# FD = 5_FD
P1 <- a1 %>% dplyr::select(1,76:78) %>% column_to_rownames(var = "gen")

# ST3 
P1 <- a1 %>% dplyr::select(1,23,79:89) %>% column_to_rownames(var = "gen")
colnames(P1)
colnames(P1)[1] <- "ST3_He_ID"
P1 <- P1[,c(2:7,1,8:12)]

colnames(P1)
P1.1 <- P1[,c(1:3)]
P1.1 <- P1[,c(4:6)]
P1.1 <- P1[,c(7:9)]
P1.1 <- P1[,c(10:12)]
P2 <- cor(P1.1, use = "complete.obs")

# ST4
P1 <- a1 %>% dplyr::select(1,90:94) %>% column_to_rownames(var = "gen")



P3 <- COV_1[[1]]
P3 <- COV_1[[2]]
P3 <- COV_1[[3]]
P3 <- COV_1[[4]]
P3 <- COV_1[[5]]

list_1 <- gsub("BLUE_", "", colnames(P3))
colnames(P2) <- list_1
rownames(P2) <- list_1
colnames(P3) <- list_1
rownames(P3) <- list_1

ncol(P2)

C1 <- ggcorrplot(P2[,ncol(P2):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + scale_fill_gradient(low = "white", high = "steelblue") + theme_ipsum(base_family = "Arial", base_size = 8) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank())

V1 <- ggcorrplot(P3[,ncol(P2):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + scale_fill_gradient(low = "white", high = "orangered") + theme_ipsum(base_family = "Arial", base_size = 8) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank())

myplot1 <- ggarrange(C1, V1, labels = c("a", "b"), ncol = 2, nrow = 1)

myplot1 <- ggarrange(C1, V1, labels = c("a", "b"), ncol = 1, nrow = 2)
myplot1

# ggsave(filename = "ST4_cor.pdf", plot = C1, width = 5, height = 5, units = "in")

colnames(P2)
colnames(P3)

###########
colnames(a1)
list_1 <- c("stage", "trait", "loc", "year", "cut")
# MS = 1_MSC

P1 <- a1 %>% dplyr::select(1,2:14) %>% gather(key = "trait1", value = "BLUE", 2:14) %>% separate(2, list_1, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% unite("merged", c(year, cut), sep = "_", remove = F)
colnames(P1)

P2 <- a1 %>% dplyr::select(1,15:22) %>% gather(key = "trait1", value = "BLUE", 2:9) %>% separate(2, list_1, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% unite("merged", c(year, cut), sep = "_", remove = F)
colnames(P2)

P3 <- a1 %>% dplyr::select(1,23:44) %>% gather(key = "trait1", value = "BLUE", 2:23) %>% separate(2, list_1, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% unite("merged", c(year, cut), sep = "_", remove = F)
colnames(P3)


# Yi = 4_Yield
P4 <- a1 %>% dplyr::select(1,45:75) %>% gather(key = "trait1", value = "BLUE", 2:32) %>% separate(2, list_1, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% unite("merged", c(year, cut), sep = "_", remove = F)
colnames(P4)

P5 <- a1 %>% dplyr::select(1,76:78) %>% gather(key = "trait1", value = "BLUE", 2:4) %>% separate(2, list_1, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% unite("merged", c(year, cut), sep = "_", remove = F)
colnames(P5)

P6 <- a1 %>% dplyr::select(1,45:75) %>% gather(key = "trait1", value = "BLUE", 2:32) %>% separate(2, list_1, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% unite("merged", c(year, cut), sep = "_", remove = F)
colnames(P1)

P7 <- a1 %>% dplyr::select(1,45:75) %>% gather(key = "trait1", value = "BLUE", 2:32) %>% separate(2, list_1, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% unite("merged", c(year, cut), sep = "_", remove = F)
colnames(P1)

# boxplot

P1$merged <- as.factor(P1$merged)
levels(P1$merged)

G1 <- ggplot(P1, aes(x = merged, y = BLUE, fill = year)) + geom_boxplot(alpha = 0.6) + scale_fill_viridis(discrete = T) + scale_color_viridis(discrete = T) + theme_ipsum(base_family = "Arial", base_size = 8) + theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank()) + facet_grid(. ~ loc, scales = "free", space = "free")

G4 <- ggplot(P4, aes(x = merged, y = BLUE, fill = year)) + geom_boxplot(alpha = 0.6) + scale_fill_viridis(discrete = T) + scale_color_viridis(discrete = T) + theme_ipsum(base_family = "Arial", base_size = 8) + theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(. ~ loc, scales = "free", space = "free")

str(P4)
P4$merged <- as.factor(P4$merged)
levels(P4$merged)

G5 <- ggplot(P5, aes(x = merged, y = BLUE, fill = year)) + geom_boxplot(alpha = 0.6) + scale_fill_viridis(discrete = T) + scale_color_viridis(discrete = T) + theme_ipsum(base_family = "Arial", base_size = 8) + theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(. ~ loc, scales = "free", space = "free")

P1$merged <- as.factor(P1$merged)
G1 <- ggboxplot(P1, x = "merged", y = "BLUE", fill = "year", alpha = 0.6) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  theme_classic(base_family = "Arial", base_size = 16) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(. ~ loc, scales = "free", space = "free")
# 6

G2 <- ggboxplot(P2, x = "merged", y = "BLUE", fill = "year", alpha = 0.6) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  theme_classic(base_family = "Arial", base_size = 16) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(. ~ loc, scales = "free", space = "free")
# 5

P3$merged <- as.factor(P3$merged)
G3 <- ggboxplot(P3, x = "merged", y = "BLUE", fill = "year", alpha = 0.6) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  theme_classic(base_family = "Arial", base_size = 16) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(. ~ loc, scales = "free", space = "free")
# 8.5

P4$merged <- as.factor(P4$merged)
G4 <- ggboxplot(P4, x = "merged", y = "BLUE", fill = "year", alpha = 0.6) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  theme_classic(base_family = "Arial", base_size = 16) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(. ~ loc, scales = "free", space = "free")
# 12

G5 <- ggboxplot(P5, x = "merged", y = "BLUE", fill = "year", alpha = 0.6) + 
  scale_fill_manual(values=c("#999999", "#E69F00")) + 
  theme_classic(base_family = "Arial", base_size = 16) +
  theme(axis.title.x=element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(. ~ loc, scales = "free", space = "free")
# 3 X 3

ggarrange(G1, G4, labels = c("a", "b"), ncol = 1, nrow = 2)

###############


# ST3 
P1 <- a1 %>% dplyr::select(1,23,79:89) %>% column_to_rownames(var = "gen")
colnames(P1)
colnames(P1)[1] <- "ST3_He_ID"
P1 <- P1[,c(2:7,1,8:12)]

list_3 <- gsub("ST3_", "", colnames(P1))
colnames(P1) <- list_3
head(P1)

P1 <- P1[,c(1:3)] %>% rownames_to_column(var = "gen") %>% gather(key = "trait", value = "BLUE", 2:4)
P1 <- P1[,c(4:6)] %>% rownames_to_column(var = "gen") %>% gather(key = "trait", value = "BLUE", 2:4)
P1 <- P1[,c(7:9)] %>% rownames_to_column(var = "gen") %>% gather(key = "trait", value = "BLUE", 2:4)
P1 <- P1[,c(10:12)] %>% rownames_to_column(var = "gen") %>% gather(key = "trait", value = "BLUE", 2:4)



P1 <- a1 %>% dplyr::select(1,2:14) %>% gather(key = "trait1", value = "BLUE", 2:14) %>% separate(2, list_1, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% unite("merged", c(year, cut), sep = "_", remove = F)
colnames(P1)


# Yi
P1 <- a1 %>% dplyr::select(1,45:75)
P1 <- P1 %>% gather(key = "trait", value = "BLUE", 2:32) %>% separate(2, list_1, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% unite("merged", c(year, cut), sep = "_", remove = F)
colnames(P1)

P1$trait <- gsub("ST1_Yi_", "", P1$trait)


# ST4
P1 <- a1 %>% dplyr::select(1,90:94)
P1 <- P1 %>% gather(key = "trait", value = "BLUE", 2:6)
P1$trait <- gsub("_Overall", "", P1$trait)

P1 <- na.omit(P1)
P1$trait <- as.factor(P1$trait)
lev1 <- levels(P1$trait)
lev1
my_comparisons1 <- combn(lev1, 2, simplify = F)





MS <- ggboxplot(P1, x = "trait", y = "BLUE", fill = "trait", alpha = 0.6) + 
  scale_fill_manual(values = wes_palette("BottleRocket2")) +
  stat_compare_means(comparisons = my_comparisons1, method = "t.test", label = "p.signif") + 
  theme(legend.position = "none", axis.title.x=element_blank())

DM <- ggboxplot(P1, x = "trait", y = "BLUE", fill = "trait", alpha = 0.6) + 
  scale_fill_manual(values = wes_palette("BottleRocket2")) +
  stat_compare_means(comparisons = my_comparisons1, method = "t.test", label = "p.signif") + 
  theme(legend.position = "none", axis.title.x=element_blank())

He <- ggboxplot(P1, x = "trait", y = "BLUE", fill = "trait", alpha = 0.6) + 
  scale_fill_manual(values = wes_palette("BottleRocket2")) +
  stat_compare_means(comparisons = my_comparisons1, method = "t.test", label = "p.signif") + 
  theme(legend.position = "none", axis.title.x=element_blank())

Yi <- ggboxplot(P1, x = "trait", y = "BLUE", fill = "trait", alpha = 0.6) + 
  scale_fill_manual(values = wes_palette("BottleRocket2")) +
  stat_compare_means(comparisons = my_comparisons1, method = "t.test", label = "p.signif") + 
  theme(legend.position = "none", axis.title.x=element_blank())

myplot1 <- ggarrange(MS, DM, He, Yi,
                     labels = c("a", "b", "c", "d"), ncol = 2, nrow = 2)

ggsave(filename = "myplot1.jpg", plot = myplot1, width = 15, height = 9)
# scale_color_viridis(discrete = TRUE, option = "D") + scale_fill_viridis(discrete = TRUE) +
# scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
# scale_color_viridis(discrete = TRUE, option = "D") + scale_fill_viridis(discrete = TRUE) +
##########






# More than two groups
#:::::::::::::::::::::::::::::::::::::::::::::::::
# Pairwise comparisons: Specify the comparisons you want
my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "npg")+
  # Add pairwise comparisons p-value
  stat_compare_means(comparisons = my_comparisons, label.y = c(29, 35, 40))+
  stat_compare_means(label.y = 45)     # Add global Anova p-value

# Multiple pairwise test against a reference group
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "npg")+
  stat_compare_means(method = "anova", label.y = 40)+ # Add global p-value
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test", ref.group = "0.5")

# Multiple grouping variables
#:::::::::::::::::::::::::::::::::::::::::::::::::
# Box plot facetted by "dose"
p <- ggboxplot(ToothGrowth, x = "supp", y = "len",
               color = "supp", palette = "npg",
               add = "jitter",
               facet.by = "dose", short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
p + stat_compare_means(
  aes(label = paste0("p = ", ..p.format..))
)

head(ToothGrowth)
ggpaired(ToothGrowth, x = "supp", y = "len",
         color = "supp", line.color = "gray", line.size = 0.4,
         palette = "npg")+
  stat_compare_means(paired = TRUE)


##################



library(ggplot2)
library(ggrepel)

nba <- read.csv("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/pheno_fa.csv", sep = ",")
colnames(nba)


### geom_label_repel
ggplot(nba, aes(x= ST4_FD_Overall, y = ST4_Yi_Overall)) + geom_point(color = dplyr::case_when(nba$ST4_Yi_Overall > 4.5 ~ "#1b9e77", nba$ST4_Yi_Overall < 3.0 ~ "#d95f02", TRUE ~ "#7570b3"), size = 3, alpha = 0.8) + 
  geom_label_repel(aes(label = gen), 
                   box.padding   = 0.35, 
                   point.padding = 0.5, 
                   segment.color = 'grey50') + theme_classic()


### geom_label_repel
ggplot(nba, aes(x= ST4_He_Overall, y = ST4_Yi_Overall)) + geom_point(color = dplyr::case_when(nba$ST4_Yi_Overall > 4.5 ~ "#1b9e77", nba$ST4_Yi_Overall < 3.0 ~ "#d95f02", TRUE ~ "#7570b3"), size = 3, alpha = 0.8) + 
  geom_label_repel(aes(label = gen), 
                   box.padding   = 0.35, 
                   point.padding = 0.5, 
                   segment.color = 'grey50') + theme_classic()







ggplot(nba, aes(x= ST4_FD_Overall, y = ST4_Yi_Overall, label = gen)) + 
  geom_point(color = dplyr::case_when(nba$ST4_Yi_Overall > 4.5 ~ "#1b9e77", 
                                      nba$ST4_Yi_Overall < 3.0 ~ "#d95f02",
                                      TRUE ~ "#7570b3"), size = 3, alpha = 0.8)

+ geom_text_repel(data = subset(nba, ST4_Yi_Overall > 4.5), 
                  nudge_y = 5.0 - subset(nba, ST4_Yi_Overall > 4.5)$ST4_Yi_Overall,                 size = 4,
                  box.padding = 1.5,
                  point.padding = 0.5,
                  force = 100,
                  segment.size = 0.2,
                  segment.color = "grey50",
                  direction = "x") + 
  geom_label_repel(data = subset(nba, ST4_Yi_Overall > 4.5),nudge_y = 1.0 - subset(nba, ST4_Yi_Overall < 3.0)$ST4_Yi_Overall, 
                   size          = 4,
                   box.padding   = 0.5,
                   point.padding = 0.5,
                   force         = 100,
                   segment.size  = 0.2,
                   segment.color = "grey50",
                   direction     = "x")
  


ggplot(nba, aes(x= ST4_FD_Overall, y = ST4_Yi_Overall, label = gen)) + 
  geom_point(color = dplyr::case_when(nba$ST4_Yi_Overall > 4.5 ~ "#1b9e77", 
                                      nba$ST4_Yi_Overall < 3.0 ~ "#d95f02",
                                      TRUE ~ "#7570b3"), 
             size = 3, alpha = 0.8) +
  geom_text_repel(data          = subset(nba, ST4_Yi_Overall > 4.5),
                  size          = 4,
                  box.padding   = 1.5,
                  point.padding = 0.5,
                  force         = 100,
                  segment.size  = 0.2,
                  segment.color = "grey50",
                  direction     = "x") +
  geom_label_repel(data         = subset(nba, ST4_Yi_Overall > 4.5),
                   size          = 4,
                   box.padding   = 0.5,
                   point.padding = 0.5,
                   force         = 100,
                   segment.size  = 0.2,
                   segment.color = "grey50",
                   direction     = "x") +
  scale_x_continuous(expand = expansion(mult = c(0.2, .2))) +
  scale_y_continuous(expand = expansion(mult = c(0.1, .1))) +
  theme_classic(base_size = 16)


install.packages("fastmatrix")
library(fastmatrix)
id1 <- diag(10)

a2 <- a1[1:10, 1:3]
a3 <- cov(a2)


a4 <- kronecker.prod(a3, id1)

(0.8242275)
(0.678831)

0.769568
0.856239E-01

r = (0.8242275*0.678831)/sqrt(((0.8242275)^2+0.769568)*((0.678831)^2+0.856239E-01))

sqrt()


###########

corr_simple <- function(data=df,sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}
corr_simple(data = a2, sig = 0.5)



