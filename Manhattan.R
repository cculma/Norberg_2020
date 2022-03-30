# figure manhattan Norberg
# generate manhattan plot by trait

rm(list = ls(all = T))
library(plotly)
library(dplyr)
library(ggpubr)
library(GWASpoly)
library(ggplot2)

load("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/data_5.RData")
trait1
trait2
trait3 <- c("ST4_MS_Overall", "ST4_DM_Overall", "ST4_He_Overall", "ST4_Yi_Overall")
trait4 <- c("ST1_FD_WA_2018_3", "ST1_FD_ID_2019_4",	"ST1_FD_WA_2019_5", "ST4_FD_Overall")
trait5 <- c("ST1_DM_ID_2019_1", "ST3_DM_WA", "ST4_DM_Overall")

############### 

load("~/Documents/Cesar/git/big_files/data_3.RData")
load("~/Documents/Cesar/git/big_files/data_3.1.RData")
load("~/Documents/Cesar/git/big_files/data_4.RData")

load("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/data_3.RData")

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


###############


# DM Manhattan plot
P1 <- manhattan.plot(data = data_5.0, traits = trait5) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank()) 

# FD Manhattan plot
P2 <- manhattan.plot(data = data_5.0, traits= trait4) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank()) 

ggsave(filename = "~/Documents/git/Norberg_2020/figs/FD_manhattan.jpg", plot = P2, width = 8, height = 8)

trait6 <- c("ST2_He_OR_2018", "ST1_He_OR_2018_3", "ST1_Yi_OR_2019_3",
          "ST1_MS_WA_2019_2", "ST1_DM_ID_2019_1", "ST1_MS_OR_2019_1")

P3 <- manhattan.plot(data = data_5.0, traits = trait6) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank()) 


P4 <- manhattan.plot(data = data_5.1, traits=c ("ST3_MS_OR")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())

trait7 <- c("ST1_MS_WA_2020_2", "ST1_MS_WA_2020_5", "ST1_MS_WA_2020_1", "ST3_MS_OR")

P5 <- manhattan.plot(data = data_5.1, traits= trait7) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())


trait8 <- c("ST2_He_OR_2018","ST4_He_Overall","ST3_Yi_OR",
            "ST1_He_WA_2019_4")

P6 <- manhattan.plot(data = data_5.0, traits= trait8) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())


trait9 <- c("ST1_Yi_OR_2020_4", "ST3_Yi_WA", "ST1_Yi_OR_2020_4")

P7 <- manhattan.plot(data = data_5.1, traits= trait9) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())

#~~~~~~~~~

load("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/ST1_1MSC.RData")
data_6.2 <- set.threshold(ST1_1MSC_1, method= "Bonferroni", level=0.2)


trait10 <- c("ST1_MS_ID_2018_1", "ST1_MS_OR_2018_1", "ST1_MS_ID_2019_1", "ST1_MS_WA_2019_2", "ST1_MS_WA_2020_1", "ST1_MS_WA_2020_2", "ST1_MS_WA_2020_5")

P8 <- manhattan.plot(data = data_6.2, traits= trait10) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())

#~~~~~~~~~


P9 <- manhattan.plot(data = data_5, traits=c ("aug_21_1stage")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
P10 <- manhattan.plot(data = data_5, traits=c ("sep_21_1stage")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
P11 <- manhattan.plot(data = data_5, traits=c ("MET_may")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
P12 <- manhattan.plot(data = data_5, traits=c ("MET_jun")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
P13 <- manhattan.plot(data = data_5, traits=c ("MET_jul")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
P14 <- manhattan.plot(data = data_5, traits=c ("MET_aug")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
P15 <- manhattan.plot(data = data_5, traits=c ("MET_sep")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())


Q1 <- manhattan.plot(data = data_5, traits=c ("MET_20")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
Q2 <- manhattan.plot(data = data_5, traits=c ("MET_21")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
Q3 <- manhattan.plot(data = data_5, traits=c ("FA1_all")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
Q4 <- manhattan.plot(data = data_5, traits=c ("PH_19_1stage")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
Q5 <- manhattan.plot(data = data_5, traits=c ("PH_20_1stage")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
Q6 <- manhattan.plot(data = data_5, traits=c ("FA1_PH")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())
Q7 <- manhattan.plot(data = data_5, traits=c ("CT_20_1stage")) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("aquamarine4","azure4")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_text(size = 12), plot.tag = element_blank())


myplot1 <- ggarrange(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15,
                     labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o"), ncol = 5, nrow = 3)
myplot2 <- ggarrange(Q1, Q2, Q3, Q4, Q5, Q6, Q7, 
                     labels = c("a", "b", "c", "d", "e", "f", "g"), ncol = 3, nrow = 3)

ggsave(filename = "myplot1.jpg", plot = myplot1, width = 15, height = 9)
ggsave(filename = "myplot2.jpg", plot = myplot2, width = 9, height = 9)


myplot1 <- LD.plot(data_5) + theme_classic(base_family = "Arial", base_size = 12) + ggtitle("LD plot")

ggsave(filename = "myplot1.jpg", plot = myplot1, width = 6, height = 6)
