# plot covariance matrix
rm(list = ls())

library(corrplot)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)
library(ggcorrplot)

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
P3 <- P3 %>% gather(key="cov", value="value", 1:length(P3))

# plot
str(P3)
ggplot(P3, aes(x=value, color=cov, fill=cov)) + geom_histogram(alpha=0.6) + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + theme_ipsum() + theme(legend.position="none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8)) + facet_wrap(~cov)

# plot 
ggcorrplot(P3, hc.order = T, type = "lower", lab = T, lab_col = "grey3", lab_size = 2) + scale_fill_viridis(discrete = F) + scale_color_viridis(discrete = F) + theme_ipsum() + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2))
