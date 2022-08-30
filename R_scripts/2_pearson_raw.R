
library(tidyverse)
library(data.table)
library(goeveg)
library(ggcorrplot)

# Yi = 4_Yield
R_Yi <- list()
for (i in 1:length(data_ar4)) {
  data <- read.csv(data_ar4[i])
  data <- data[,c(3,5,6,7,8,14,19,24)]
  colnames(data) <- c("block", "ID", "gen", "row", "col", "resp", "cov1", "cov2")
  R_Yi[[length(R_Yi)+1]] = data
}
names(R_Yi) <- list_4
R_Yi <-rbindlist(R_Yi, use.names=TRUE, fill=TRUE, idcol="env")
head(R_Yi)

a4 <- R_Yi %>% dplyr::filter(!gen %in% c(201, 202)) %>% select(c(1,4,7))
a5 <- R_Yi %>% dplyr::filter(gen %in% c(201, 202)) %>% unite("gen", c(gen, block), sep = "_", remove = T) %>% select(c(1,2,6))

a6 <- rbind(a4, a5) %>% spread(key = env, value = resp) %>% remove_rownames() %>% column_to_rownames("gen")

P00 <- cor(a6, use = "complete.obs")

ggcorrplot(P00[,ncol(P00):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + theme_classic(base_family = "Arial", base_size = 12) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank()) + labs(title = "raw data")
