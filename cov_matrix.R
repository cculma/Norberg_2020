# plot covariance matrix
rm(list = ls())

library(corrplot)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)
library(ggcorrplot)
library(ggpubr)

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
ggcorrplot(P3[,13:1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2) + scale_fill_gradient(low = "white", high = "steelblue") + theme_ipsum() + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2))



##################

a1 <- read.csv("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/pheno_fa.csv")
a1$gen

a1 <- a1[1:(length(colnames(a1))-3)]
colnames(a1)

# MS = 1_MSC
P1 <- a1 %>% dplyr::select(1,2:14) %>% column_to_rownames(var = "gen")

# DM = 2_DM
P1 <- a1 %>% dplyr::select(1,15:22) %>% column_to_rownames(var = "gen")

# He = 3_Height
P1 <- a1 %>% dplyr::select(1,23:44) %>% column_to_rownames(var = "gen")

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


P2 <- cor(P1, use = "complete.obs")

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

list_1 <- c("stage", "trait", "loc", "year", "cut")
# MS = 1_MSC

P1 <- a1 %>% dplyr::select(1,2:14) %>% gather(key = "trait1", value = "BLUE", 2:14) %>% separate(2, list_1, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% unite("merged", c(year, cut), sep = "_", remove = F)
colnames(P1)

P2 <- a1 %>% dplyr::select(1,2:14) %>% gather(key = "trait1", value = "BLUE", 2:14) %>% separate(2, list_1, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% unite("merged", c(year, cut), sep = "_", remove = F)
colnames(P1)

P3 <- a1 %>% dplyr::select(1,2:14) %>% gather(key = "trait1", value = "BLUE", 2:14) %>% separate(2, list_1, sep = "_", remove = TRUE, convert = FALSE, extra = "warn") %>% unite("merged", c(year, cut), sep = "_", remove = F)
colnames(P1)


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

G1 <- ggplot(P1, aes(x = merged, y = BLUE, fill = year)) + geom_boxplot(alpha = 0.6) + scale_fill_viridis(discrete = T) + scale_color_viridis(discrete = T) + theme_ipsum(base_family = "Arial", base_size = 8) + theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(. ~ loc, scales = "free", space = "free")

G4 <- ggplot(P4, aes(x = merged, y = BLUE, fill = year)) + geom_boxplot(alpha = 0.6) + scale_fill_viridis(discrete = T) + scale_color_viridis(discrete = T) + theme_ipsum(base_family = "Arial", base_size = 8) + theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(. ~ loc, scales = "free", space = "free")

G5 <- ggplot(P5, aes(x = merged, y = BLUE, fill = year)) + geom_boxplot(alpha = 0.6) + scale_fill_viridis(discrete = T) + scale_color_viridis(discrete = T) + theme_ipsum(base_family = "Arial", base_size = 8) + theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(. ~ loc, scales = "free", space = "free")

ggarrange(G1, G4, labels = c("a", "b"), ncol = 1, nrow = 2)
