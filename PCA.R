# PCA generation of GWASpoly file


library(sommer)


# MPP_Ms2_GWASPoly.txt
G <- read.csv('~/Documents/Cesar/git/big_files/AllSamples_Ms_filter_q30_imputed_GWASPoly_contigRemoved.txt', header = TRUE, row.names = 1, check.names = F)
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
marks.1 <- as.data.frame(t(marks))
marks.1[1:5,1:5]
dim(marks.1)
marks.2 <- tibble::rownames_to_column(marks.1, "Marker1") %>% separate(1, c("Chrom", "Position"), sep = "_", remove = T, convert = F, extra = "warn")
marks.2[1:5,1:5]
write.table(marks.2, "~/Documents/Cesar/git/big_files/Norberg_1.txt", col.names = T, row.names = F, quote = FALSE, na = "", sep=",")


############
# PCA

geno.scale <- scale(numo$M, center = T, scale = F)
svdgeno <- svd(geno.scale)
PCA <- geno.scale %*% svdgeno$v
PCA[1:5,1:5]

# screenplot to visualize the proportion of variance explained by PCA
plot(round((svdgeno$d)^2 / sum((svdgeno$d)^2), d = 7)[1:10], type = 'o', xlab = 'PCA', ylab = '% variance')

# Proportion of variance explained by PCA1, PCA2, PCA3
PCA1 <- 100*round((svdgeno$d[1])^2 / sum((svdgeno$d)^2), d = 3); PCA1
PCA2 <- 100*round((svdgeno$d[2])^2 / sum((svdgeno$d)^2), d = 3); PCA2
PCA3 <- 100*round((svdgeno$d[3])^2 / sum((svdgeno$d)^2), d = 3); PCA3

PCA4 <- PCA[,c(1:3)]
head(PCA4)
class(PCA4)
colnames(PCA4) <- c("PC1", "PC2", "PC3")
PCA4 <- as.data.frame(PCA4)
PCA4 <- PCA4 %>% rownames_to_column(var = "gen")
