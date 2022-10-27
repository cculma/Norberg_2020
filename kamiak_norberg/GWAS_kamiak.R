# GWASpoly using FA scores
# GWASpoly using corrected values by biplot

rm(list = ls()) # clean Global Environment

library(GWASpoly)


# workstation
setwd("/scratch/user/cesar.medinaculma/20220324_080110")

pheno <- read.csv("pheno_fa.csv", row.names = 1)
trait1 <- colnames(pheno)[1:(length(colnames(pheno))-3)]
trait1
params <- set.params(fixed=c("PC1","PC2","PC3"),
                     fixed.type=rep("numeric",3), n.PC = 3)
models_1 <- c("general", "additive", "1-dom", "2-dom",  "diplo-additive", "diplo-general")

# data_1 <- read.GWASpoly(ploidy=4, 
#                        pheno.file="pheno_fa.csv", 
#                        geno.file="AllSamples_Ms_filter_q30_imputed_GWASPoly_contigRemoved.txt", 
#                        format="ACGT", n.traits=length(trait1), delim=",")

# data_2 <- set.K(data = data_1, LOCO = F, n.core = 60)
# data_3.2 <- GWASpoly(data = data_2, models = models_1, traits = trait1, params = params, n.core = 100)
# save.image("/scratch/user/cesar.medinaculma/20220324_080110/a1.RData")
# save(data_3.2, file = "/scratch/user/cesar.medinaculma/20220324_080110/data_3.2.RData")

########
# FA1
pheno <- read.csv("pheno_fa1.csv", row.names = 1)
trait1 <- colnames(pheno)[1:(length(colnames(pheno))-3)]
trait1

data_1 <- read.GWASpoly(ploidy=4,
                        pheno.file="pheno_fa1.csv",
                        geno.file="AllSamples_Ms_filter_q30_imputed_GWASPoly_contigRemoved.txt",
                        format="ACGT", n.traits=length(trait1), delim=",")

data_2 <- set.K(data = data_1, LOCO = T, n.core = 60)
data_3.3 <- GWASpoly(data = data_2, models = models_1, traits = trait1, params = params, n.core = 100)
save.image("/scratch/user/cesar.medinaculma/20220324_080110/a1.RData")
save(data_3.3, file = "/scratch/user/cesar.medinaculma/20220324_080110/data_3.3.RData")
