

data_5.3 <- set.threshold(PH_data_3.3, method= "Bonferroni", level=0.05)
k <- as.data.frame(data_5.3@scores)
k[is.na(k)] <- 0
rownames(k) <- paste(data_5.3@map$Chrom, data_5.3@map$Position, sep = "_")
colnames(k)

k <- k %>% rownames_to_column("marker")
k <- k %>% dplyr::filter(marker == "Chr2_49623557")
k <- k %>% column_to_rownames("marker")
k <- t(k)

?set.threshold
data_5.8 <- set.threshold(PH_data_3.3, method= "M.eff", level=0.05)

QTL_3 <- get.QTL(data_5.3)
QTL_4 <- get.QTL(data_5.8)
