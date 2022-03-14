# plotly figures

rm(list = ls()) # clean Global Environment
library(GWASpoly)
library(tidyverse)
library(vcfR)
library(GenomicRanges)
library(genomation)
library(plyranges)
library(Repitools)
library(data.table)
library(plotly)

############
load("~/Documents/Cesar/git/big_files/data_3.RData")
load("~/Documents/Cesar/git/big_files/data_3.1.RData")
load("~/Documents/Cesar/git/big_files/data_4.RData")


data_5.0 <- set.threshold(data_3, method= "Bonferroni", level=0.05)
data_5.1 <- set.threshold(data_3.1, method= "Bonferroni", level=0.05)
data_5.2 <- set.threshold(data_4, method= "Bonferroni", level=0.05)

data_6.0 <- get.QTL(data_5.0)
data_6.1 <- get.QTL(data_5.1)
data_6.2 <- get.QTL(data_5.2)

t_6.0 <- c("ST3_Yi_OR", "ST4_Yi_Overall")
t_6.1 <- c("ST1_Yi_OR_2020_2", "ST2_Yi_OR_2020", "ST3_Yi_OR", "ST4_Yi_Overall")

data_6.0 <- data_6.0 %>% dplyr::filter(!Trait %in% t_6.0)
data_6.1 <- data_6.1 %>% dplyr::filter(!Trait %in% t_6.1)

QTL_01 <- rbind(data_6.0, data_6.1)
t_6.3 <- c("ST4_MS_Overall", "ST4_DM_Overall", "ST4_He_Overall", "ST4_Yi_Overall", "ST4_FD_Overall")
data_6.3 <- QTL_01 %>% dplyr::filter(Trait %in% t_6.3) %>% distinct(Marker, .keep_all = T) 
data_6.3$Marker

QTL_02 <- QTL_01 %>% distinct(Marker, .keep_all = T) 

##################
trait1

models_1
qtl <- get.QTL(data=data_5.0)
qtl <- get.QTL(data=data_5.0, traits = "ST4_He_Overall", models = c("additive", "general"))
qtl <- get.QTL(data=data_5.0, traits = "ST4_He_Overall", models = models_1)

fit.ans <- fit.QTL(data=data_5.0,
                   qtl=qtl[,c("Marker","Model")], trait = "ST4_He_Overall",
                   fixed= NULL)

knitr::kable(fit.ans,digits=3)

##################

# to generate frequency of markers shared by trait
S2 <- QTL_01 %>% dplyr::select(1,4) %>% distinct(Marker, Trait, .keep_all = T) 
cc <- count(S2, Marker)
cc1 <- count(cc, n)

fig <- plot_ly(
  x = cc1$n,
  y = cc1$nn,
  name = "freq Markers",
  type = "bar", text = cc1$nn, textposition = 'auto') %>% layout(xaxis = list(autotypenumbers = 'strict', title = 'Trait'), yaxis = list(title = 'Markers')) %>% config(toImageButtonOptions = list(format = "svg",filename = "fig", width = 600, height = 300))

orca(fig, "~/Documents/git/Norberg_2020/GWAS_results/Figures/count_markers.svg", width = 5 * 96, height = 4 * 96)