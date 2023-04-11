# PCA generation of GWASpoly file

library(sommer)
library(tidyverse)
library(AGHmatrix)
library(caret)


# MPP_Ms2_GWASPoly.txt
G <- read.csv('~/Documents/git/big_files/AllSamples_Ms_filter_q30_imputed_GWASPoly_contigRemoved.txt', header = TRUE, row.names = 1, check.names = F)
G[1:5,1:5]
dim(G)

G1 <- G %>% unite(Chrom1, 1:2, remove = T)
G3 <- G[,c(1,2)]
G1[1:5,1:5]
G1 <- as.matrix(G1 %>% remove_rownames() %>% column_to_rownames(var = "Chrom1"))

G2 <- t(G1)
G2[1:5,1:5]
G2 <- as.data.frame(G2)
str(G2)
numo <- atcg1234(data=G2, ploidy=4); 
numo$M[1:5,1:5]
numo$ref.allele[,1:5]
dim(numo$M)

marks <- numo$M
marks[1:5,1:5]
dim(marks)
str(marks)
class(marks)
marks.2 <- marks
marks.2[1:10,1:10]
marks.2[marks.2 == 0] <- -1 # AAAA
marks.2[marks.2 == 1] <- 0  # AAAB
marks.2[marks.2 == 2] <- 0
marks.2[marks.2 == 3] <- 0
marks.2[marks.2 == 4] <- 1
dim(marks.2)

nzv <- nearZeroVar(marks.2)
marks.2 <- marks.2[, -nzv]
dim(marks.2)
map <- as.data.frame(colnames(marks.2))
map <- map %>% separate(1, into = c("LG", "Position"), sep = "_", remove = F, convert = F, extra = "merge")
colnames(map)[1] <- "Locus"
map$LG <- gsub("Chr", "", map$LG)
map$LG <- as.integer(map$LG)
map$Locus <- as.factor(map$Locus)
map$Position <- as.numeric(map$Position)
str(map)
class(marks.2)

res1 <- LD.decay(marks.2, map)

res3 <- LD.decay(markers=marks.2, map=map,
                 unlinked = TRUE,gamma = .95)


names(res1)
res2 <- res1
res2$all.LG <- res2$all.LG[which(res2$all.LG$p < .001),]

with(res2$all.LG, plot(r2~d,col=transp("cadetblue"),
                      xlim=c(0,55), ylim=c(0,1),
                      pch=20,cex=0.5,yaxt="n",
                      xaxt="n",ylab=expression(r^2),
                      xlab="Distance in bp"))
axis(1, at=seq(0,55,5), labels=seq(0,55,5))
axis(2,at=seq(0,1,.1), labels=seq(0,1,.1), las=1)



marks.2 <- as.data.frame(t(marks.2))
dim(marks.2)
marks.2 <- marks.2[,have.both]
colnames(marks.2)
colnames(marks.2) <- paste0("Ind-", colnames(marks.2))

marks.1 <- as.data.frame(t(marks))
marks.1[1:5,1:5]
dim(marks.1)
# marks.2 <- tibble::rownames_to_column(marks.1, "Marker1") %>% separate(1, c("Chrom", "Position"), sep = "_", remove = T, convert = F, extra = "warn")
# marks.2[1:5,1:5]
# write.table(marks.2, "~/Documents/Cesar/git/big_files/Norberg_1.txt", col.names = T, row.names = F, quote = FALSE, na = "", sep=",")



G4.1 <- numo$ref.alleles
class(G4.1)
G4.2 <- t(as.data.frame(G4.1))
G4.2 <- G4.2[,-1, drop = F]
head(G4.2)

G4.3 <- merge(G4.2, marks.2, by = "row.names")
class(G4.3)
G4.3 <- G4.3 %>% separate(1, c("Chrom", "Position"), sep = "_", remove = T, convert = F, extra = "warn")
G4.3[1:5, 1:10]
G4.3$rs <- 1:nrow(G4.3)
G4.3 <- G4.3 %>% mutate(rs = 1:n()) %>% select(rs, everything())
G4.3$Chrom <- gsub("Chr", "", G4.3$Chrom)
dim(G4.3)
G4.4 <- G4.3[1:100,]
colnames(G4.4)
dim(G4.4)
write.csv(G4.3, "~/Documents/git/big_files/G4.3.csv", row.names = F, quote = F)

############
# PCA

geno.scale <- scale(numo$M, center = T, scale = F)
svdgeno <- svd(geno.scale)
PCA <- geno.scale %*% svdgeno$v
PCA[1:5,1:5]

# screenplot to visualize the proportion of variance explained by PCA
plot(round((svdgeno$d)^2 / sum((svdgeno$d)^2), d = 7)[1:10], type = 'o', xlab = 'PCA', ylab = '% variance')

autoplot(pca_res)

?plot
# Proportion of variance explained by PCA1, PCA2, PCA3
PCA1 <- 100*round((svdgeno$d[1])^2 / sum((svdgeno$d)^2), d = 3); PCA1
PCA2 <- 100*round((svdgeno$d[2])^2 / sum((svdgeno$d)^2), d = 3); PCA2
PCA3 <- 100*round((svdgeno$d[3])^2 / sum((svdgeno$d)^2), d = 3); PCA3


PCA4 <- PCA[,c(1:5)]
head(PCA4)
class(PCA4)
colnames(PCA4) <- c("PC1", "PC2", "PC3", "PC4", "PC5")
PCA4 <- as.data.frame(PCA4)
PCA4 <- PCA4 %>% rownames_to_column(var = "gen")

setwd("~/medin297@umn.edu - Google Drive/My Drive/Samac_2022/Samac_2020/7_GWASpoly/")
write.csv(PCA4, "pca.csv", quote = F, row.names = F)

p1 <- read.csv("pheno3.csv")
p1 <- p1[,c(1,2)]
colnames(p1)[1] <- "gen"

p1 <- inner_join(p1, PCA4, by = "gen")

setwd("~/medin297@umn.edu - Google Drive/My Drive/Samac_2022/Samac_2020/7_GWASpoly/")
write.csv(p1, "pheno4.csv", quote = F, row.names = F)

