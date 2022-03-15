library(tidyverse)
library(GWASpoly)
library(data.table)
###########
# save(data_3, file = "~/Documents/Cesar/git/big_files/data_3.RData")
# load("~/Documents/Cesar/git/big_files/data_3.RData")
load("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/data_5.RData")

data_5.0 <- set.threshold(data_3, method= "Bonferroni", level=0.05)
data_5.1 <- set.threshold(data_3.1, method= "Bonferroni", level=0.05)
data_5.2 <- set.threshold(data_4, method= "Bonferroni", level=0.05)

data_6.0 <- get.QTL(data_5.0)
data_6.1 <- get.QTL(data_5.1)
data_6.2 <- get.QTL(data_5.2)

# data_6.0 <- data_6.0 %>% distinct(Marker, .keep_all = T) 

trait4 <- levels(as.factor(data_6.0$Trait))
trait4

data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_DM_ID_2019_1")) 
trait4.2 <- levels(as.factor(data_7.0$Trait))
fit.ans <- fit.QTL(data=data_5.0, trait = trait4.2,
                   qtl=data_7.0[,c("Marker","Model")],
                   fixed = NULL)
fit.ans$trait <- trait4.2


knitr::kable(fit.ans,digits=3)

data_7.0 <- data_6.1 %>% dplyr::filter(Trait %in% "ST3_Yi_OR")









trait4 <- levels(as.factor(data_6.0$Trait))
trait4


fit.ans_5.0 <- list()
for (i in 1:(length(trait4))) {
  data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% trait4[i]) %>% dplyr::filter(!Model %in% c("diplo-general", "diplo-additive"))
  trait4.2 <- levels(as.factor(data_7.0$Trait))
  fit.ans <- fit.QTL(data=data_5.0, trait = trait4.2,
                     qtl=data_7.0[,c("Marker","Model")],
                     fixed = NULL)
  fit.ans_5.0[[length(fit.ans_5.0)+1]] = fit.ans
}
names(fit.ans_5.0) <- trait4
fit.ans_5.0 <-rbindlist(fit.ans_5.0, use.names=TRUE, fill=TRUE, idcol="trait")


trait5 <- levels(as.factor(data_6.1$Trait))
trait5

data_7.0 <- data_6.1 %>% dplyr::filter(Trait %in% "ST3_MS_OR") %>% dplyr::filter(!Model %in% c("diplo-general", "diplo-additive"))

fit.ans_5.1 <- list()
for (i in 1:(length(trait5))) {
  data_7.0 <- data_6.1 %>% dplyr::filter(Trait %in% trait5[i]) %>% dplyr::filter(!Model %in% c("diplo-general", "diplo-additive"))
  trait4.2 <- levels(as.factor(data_7.0$Trait))
  fit.ans <- fit.QTL(data=data_5.1, trait = trait4.2,
                     qtl=data_7.0[,c("Marker","Model")],
                     fixed = NULL)
  fit.ans_5.1[[length(fit.ans_5.1)+1]] = fit.ans
}
names(fit.ans_5.1) <- trait5
fit.ans_5.1 <-rbindlist(fit.ans_5.1, use.names=TRUE, fill=TRUE, idcol="trait")
fit.ans_5.2 <- rbind(fit.ans_5.0, fit.ans_5.1)
fit.ans_5.2$pct <- fit.ans_5.2$R2 *100
fit.ans_5.2$R2 <- round(fit.ans_5.2$R2, digits=3)
fit.ans_5.2$pval <- round(fit.ans_5.2$pval, digits=3)
fit.ans_5.2$pct <- round(fit.ans_5.2$pct, digits=3)

fit.ans_5.2 <- fit.ans_5.2 %>% unite(col = "Marker1", 3:4, sep = "_", remove = T) 

write.csv(fit.ans_5.2, "~/Documents/git/fit.ans.csv", row.names = F, quote = F)
knitr::kable(fit.ans_5.2,digits=3)

#################
# end


# trait3 is used to data_5.2
# trait2 is used to data_5.1
# trait1 is used to data_5.0 



