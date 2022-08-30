# boxplot FD
rm(list = ls())

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(brew)
library(ggstatsplot)
library(ggcorrplot)

a1 <- read.table("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/markers2.3.tsv", row.names = 1, sep = "\t", header = T)
a3 <- read.csv("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/RData/FD.csv")

a3 <- a3[,c(1,30:33)]
a3$gen <- as.character(a3$gen)

str(a3)
a2 <- a1 %>% dplyr::select(Chr5_16888158,
                           Chr5_16888160,
                           Chr5_16888161,
                           Chr5_16888163,
                           Chr5_16888169,
                           Chr5_16888170,
                           Chr5_16888188,
                           Chr5_16888190)
lev1 <- colnames(a2)
a2 <- a2 %>% rownames_to_column(var = "gen")
a2 <- inner_join(a2, a3, "gen")
head(a2)

# a2 <- a2 %>% gather(key = "marker", value = "dosage", 1:8)
str(a2)
a2[,lev1] <- lapply(a2[,lev1], factor)

ggbetweenstats(data = a2, x = Chr5_16888158, y = ST3_S_FD_ID,
               plot.type = "box",
               type = "robust",
               xlab = "158", 
               ylab = "FD ID", 
               pairwise.comparisons = TRUE,
               p.adjust.method = "fdr",
               package = "ggsci",
               palette = "nrc_npg", 
               point.args = list(alpha = 0),
               ggtheme = ggplot2::theme_classic(),
               outlier.tagging = F, 
               results.subtitle = F) + 
  ggplot2::scale_y_continuous(limits = c(2, 6), breaks = seq(from = 2, to = 7, by = 0.5)) + theme_ipsum(base_family = "Arial", base_size = 12)



colnames(a3)
cor(a3$ST1_S_FD_ID_2019_4, a3$ST1_S_FD_WA_2019_5, use = "complete.obs")

a3$ST3_S_FD_ID
a4 <- as.data.frame(round(a3$ST3_S_FD_ID, digits = 0))
a5 <- as.data.frame(round(a3$ST3_S_FD_WA, digits = 0))
a6 <- as.data.frame(round(a3$ST3_S_FD_OR, digits = 0))

cc <- count(a4, `round(a3$ST3_S_FD_ID, digits = 0)`)
cc <- count(a5, `round(a3$ST3_S_FD_WA, digits = 0)`)
cc <- count(a6, `round(a3$ST3_S_FD_OR, digits = 0)`)

100 * cc$n / sum(cc$n)


ggscatterstats(
  data = a3, 
  x = ST3_S_FD_ID,
  y = ST3_S_FD_WA,
  xlab = "FD ID", 
  ylab = "FD WA",
  label.var = gen, 
  label.expression = ST3_S_FD_WA > 4.5 & ST3_S_FD_ID > 4.5,
  point.label.args = list(alpha = 0.7, size = 4, color = "grey50"),
  xfill = "#CC79A7", ## fill for marginals on the x-axis
  yfill = "#009E73", ## fill for marginals on the y-axis

)



library(ggplot2)
library(ggsignif)

# converting to factor
mtcars$cyl <- as.factor(mtcars$cyl)


# creating the base plot
p <- ggbetweenstats(mtcars, cyl, wt, pairwise.comparisons = FALSE)
str(mtcars)
# using `pairwise_comparisons()` function to create a data frame with results



