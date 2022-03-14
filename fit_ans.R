data_5.0 <- set.threshold(data_3, method= "Bonferroni", level=0.05)
data_5.1 <- set.threshold(data_3.1, method= "Bonferroni", level=0.05)
data_5.2 <- set.threshold(data_4, method= "Bonferroni", level=0.05)

data_6.0 <- get.QTL(data_5.0)
data_6.1 <- get.QTL(data_5.1)
data_6.2 <- get.QTL(data_5.2)

data_6.0 <- data_6.0 %>% distinct(Marker, .keep_all = T) 

trait4 <- levels(as.factor(data_6.0$Trait))
trait5 <- levels(as.factor(data_6.0$Model))

data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_DM_ID_2019_1")) 

data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_FD_ID_2019_4")) %>% dplyr::filter(Model %in% c("general"))
trait4.2 <- levels(as.factor(data_7.0$Trait))
fit.ans <- fit.QTL(data=data_5.0, trait = trait4.2,
                   qtl=data_7.0[,c("Marker","Model")],
                   fixed = NULL)
class(fit.ans)
fit.ans <- knitr::kable(fit.ans,digits=3)
write.csv(fit.ans, "~/Documents/git/fit.ans.FD.csv", row.names = F, quote = F)



data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_FD_WA_2018_3")) #

data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_FD_WA_2019_5")) #

data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_He_OR_2018_1"))

data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_He_OR_2018_3"))
data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_He_OR_2019_3"))
data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_He_OR_2019_4"))
data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_He_WA_2019_1"))
data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_He_WA_2019_2"))
data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_He_WA_2019_4"))
data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_MS_OR_2019_1"))
data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_MS_WA_2019_2"))
data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_Yi_OR_2019_4"))

data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% c("ST1_FD_ID_2019_4")) %>% dplyr::filter(Model %in% c("general"))
                                   
data_7.0 <- data_6.0 %>% dplyr::filter(Model %in% c("general"))

data_7.0 <- data_6.0 %>% dplyr::filter(Model %in% c("1-dom-alt", "1-dom-ref", "2-dom-alt", "2-dom-ref", "additive", "general"))
trait4.1 <- levels(as.factor(data_7.0$Trait))
trait5.1 <- levels(as.factor(data_7.0$Model))
data_8.0 <- data_7.0
data_8.0 <- data_7.0 %>% dplyr::filter(Trait %in% c("ST4_FD_Overall"))

trait4.2 <- levels(as.factor(data_8.0$Trait))
fit.ans <- fit.QTL(data=data_5.0, trait = trait4.2,
                   qtl=data_8.0[,c("Marker","Model")],
                   fixed = NULL)
# fit.ans_1 <- fit.ans
fit.ans_2 <- rbind(fit.ans_1, fit.ans)




fit.ans_5.0 <- list()
for (i in trait4.1) {
  data_8.0 <- data_7.0 %>% dplyr::filter(Trait %in% trait4.1[i])
  trait5 <- levels(as.factor(data_8.0$Trait))
  fit.ans <- fit.QTL(data=data_5.0, trait = trait5,
                     qtl=data_8.0[,c("Marker","Model")],
                     fixed = NULL)
}

trait4 <- levels(as.factor(data_7.0$Trait))
fit.ans <- fit.QTL(data=data_5.0, trait = trait4,
                   qtl=data_7.0[,c("Marker","Model")],
                   fixed = NULL)

knitr::kable(fit.ans,digits=3)

trait4 <- levels(as.factor(data_6.0$Trait))
trait5 <- levels(as.factor(data_6.0$Model))

rm(fit.ans)

trait4 <- levels(as.factor(data_6.0$Trait))
fit.ans_5.0 <- list()
for (i in 1:length(trait4)) {
  data_7.0 <- data_6.0 %>% dplyr::filter(Trait %in% trait4[i])
  data <- fit.QTL(data=data_5.0, trait = trait4[i],
                     qtl=data_7.0[,c("Marker","Model")],
                     fixed = NULL)
  fit.ans_5.0[[length(fit.ans_5.0)+1]] = data
}
names(fit.ans_5.0) <- trait4
fit.ans_5.0 <-rbindlist(fit.ans_5.0, use.names=TRUE, fill=TRUE, idcol="trait")


trait5 <- levels(as.factor(data_6.1$Trait))
fit.ans_5.1 <- list()
for (i in 1:length(trait5)) {
  data <- fit.QTL(data=data_5.1, trait = trait5[i],
                  qtl=data_6.1[,c("Marker","Model")],
                  fixed = NULL)
  fit.ans_5.1[[length(fit.ans_5.1)+1]] = data
}
names(fit.ans_5.1) <- trait5
fit.ans_5.1 <-rbindlist(fit.ans_5.1, use.names=TRUE, fill=TRUE, idcol="trait")

# write.csv(fit.ans, "~/Documents/Cesar/git/Norberg_2020/BLUE_values/BLUE2/1_MSC_1stage.csv", row.names = F, quote = F)


#################
# end


# trait3 is used to data_5.2
# trait2 is used to data_5.1
# trait1 is used to data_5.0 



