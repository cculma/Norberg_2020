rm(list = ls())
library(asreml)
library(data.table)
library(tidyverse)
library(asremlPlus)

wheat <- wheat
str(wheat) # order data by row then col

data <- wheat 
data <- data[order(data$Row, data$Column), ] # order data by row then col (original)
data <- data[order(data$Column, data$Row), ] # order data by col then row

# Column : Factor w/ 10
# Row    : Factor w/ 67

asreml.options(gammaPar = TRUE)
# In single section models will be fitted using the gamma parameterization irrespective of whether the residual formula specifies a correlation or variance model. 

wheat1.asr <- asreml(yield ~ weed, random = ~idv(Variety), residual =
                       ~ar1v(Row):id(Column), data = wheat)
wheat1.asr$loglik

asreml.options(gammaPar = TRUE)
wheat2.asr <- asreml(yield ~ weed, random = ~idv(Variety), residual =
                       ~ar1v(Row):ar1(Column), data = wheat)
wheat2.asr$loglik

# REML log-likelihood -----------------------------------------------------


lrt(wheat1.asr, wheat2.asr, boundary = F)
REMLRT(wheat1.asr, wheat2.asr, bound.test.parameters = "none")
# bound.exclusions = c("F","B","S","C")

# The change in REML log-likelihood is significant (\chi^2_1 = 12.46,P < 0.001) with the inclusion of the autoregressive parameter for Column. 

lrt(m2, m3, boundary = F)
# linear regression coefficient -------------------------------------------

# Column : Factor w/ 10
asreml.options(gammaPar = TRUE)
wheat3.asr <- asreml(yield ~ weed + lin(Column), 
                     random = ~idv(Variety), 
                     residual =~ar1v(Row):ar1(Column), 
                     data = wheat)

summary(wheat3.asr)$varcomp
wald(wheat3.asr)

# The linear regression of column number on yield is significant (Wald statistic = 8.74).

# including a nugget effect -----------------------------------------------

asreml.options(gammaPar = TRUE)
wheat4.asr <- asreml(yield ~ lin(Column),
                     random = ~idv(Variety) + idv(units),
                     sparse = ~weed,
                     residual = ~ar1v(Row):ar1(Column), 
                     data = wheat)

infoCriteria.asreml(wheat4.asr)
infoCriteria.asreml(wheat3.asr)

# The increase in REML log-likelihood from adding the units term is significant. wheat4.asr = -4221.76, wheat3.asr = -4227.13. Also the AIC is wheat4.asr < wheat3.asr.

# predict variety (gen) means ---------------------------------------------

wheat4.pv <- predict(wheat4.asr, classify = "Variety:Column", levels = list(Column = 5.5))$pvals
blue <- predict.asreml(wheat4.asr, classify='Variety', vcov=TRUE)$pvals
cor(blue$predicted.value, wheat4.pv$predicted.value)

# The replicated check lines have lower SEs than the unreplicated test lines.

wheat5.pv <- predict(wheat4.asr, classify = "Variety:Column", levels = list(Variety = seq(1, 525), Column = 5.5))
wheat6.pv <- predict(wheat4.asr, classify = "Variety:Column", levels = list(Variety = seq(526, 532), Column = 5.5), sed = T)

head(wheat5.pv$avsed, 20)
head(wheat6.pv$avsed, 20)

hist(blue$predicted.value)





# DArT --------------------------------------------------------------------

C1 <- read.csv("~/Downloads/Cage20_DArT.csv", row.names = 1)
C1[1:5,1:5]
dim(C1)

# end ---------------------------------------------------------------------


