library(ASRtriala)
library(asreml)
library(asremlPlus)
library(dae)
library(rcompanion)
library(multcompView)

sessionInfo()

shf <- shf
str(shf)
asreml.options(gammaPar = TRUE)

wheat4.asr <- asreml(yield ~ lin(Column),
                     random = ~idv(Variety) + idv(units),
                     sparse = ~weed,
                     residual = ~ar1v(Row):ar1(Column), 
                     data = wheat)

m4 <- asreml()

wheat1.asr <- asreml(yield ~ weed, random = ~idv(Variety), residual =
                       ~ar1v(Row):id(Column), data = wheat)

data <- FD1[[4]]
data <- data[order(data$row, data$col), ]
str(data)

data$check <- recode_factor(data$gen, "201" = "control", "202" = "control", .default = "test")
data$check <- as.factor(data$check)

(11 + 11 + 5)*3
nrow(data)
data %>% dplyr::count(gen)
data %>% dplyr::count(gen, block)
data %>% dplyr::count(cut, year)
data %>% dplyr::count(gen, cut)


data <- a8[[1]] # "ID_2018_ADF"

m1 <- asreml::asreml(fixed = raw ~ 1 + at(check, "control"):gen + cut, 
                     random = ~ at(check, "test"):gen, 
                     data = data,
                     na.action = list(x = "include", y = "include"))

m1 <- asreml::asreml(fixed = raw ~ gen*cut, 
                     random = ~ + block, 
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m1 <- asreml::asreml(fixed = resp ~ gen + cov1 + cov2, 
                     random = ~ + block , 
                     residual = ~ar1(row):ar1(col), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m1 <- update.asreml(m1)
summary(m1)$varcomp
wald.asreml(m1)
?as.asrtests
current.asrt <- as.asrtests(m1, NULL, NULL)
current.asrt <- as.asrtests(m1, NULL, NULL, IClikelihood = "full")
current.asrt <- rmboundary.asrtests(current.asrt, update = T)


preds <- predict.asreml(m1, classify="gen", vcov=TRUE, aliased = T) 
pvals <- preds$pvals
pvals <- pvals[1:4]
pvals$weight <- (1/pvals$std.error)^2

hist(pvals$weight)
hist(pvals$predicted.value)

data1 <- na.omit(FD2)
data <- FD2
data <- data[order(data$row, data$col), ]
data$cons_days1 <- as.factor(data$cons_days1)
head(data)
data$check <- recode_factor(data$gen, "201" = "control", "202" = "control", .default = "test")

# residual = ~dsum(ar1(row):ar1(col) | year),
data <- na.omit(data)
levels(data$year)

m2 <- asreml::asreml(fixed = raw ~ 1 + at(check, "control"):gen, 
                     random = ~ corgh(year):at(check, "test"):gen,
                     residual = ~ ante(year):ar1(row):ar1(col),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m2 <- update.asreml(m2)
asreml::wald.asreml(m2, denDF = "algebraic", ssType = "incremental")$Wald

wald.asreml(m2, ssType = "incremental")

m3 <- asreml::asreml(fixed = resp ~ block + at(check, "control"):gen,
                     random = ~ at(check, "test"):gen + block,
                     residual = ~ar1(row):ar1(col),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m4 <- asreml::asreml(fixed = resp ~ gen + cov1 + cov2, 
                     random = ~block + idv(units), 
                     residual = ~ar1(row):ar1(col), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m5 <- asreml::asreml(fixed = resp ~ gen + cov1 + cov2, 
                     random = ~block, 
                     residual = ~ar1(row):ar1(col), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))

d1 <- infoCriteria.asreml(m1)
d2 <- infoCriteria.asreml(m2)
d3 <- infoCriteria.asreml(m3)
d4 <- infoCriteria.asreml(m4)
d5 <- infoCriteria.asreml(m5)
d6 <- rbind(d1,d2,d3,d4,d5)

plot(m1)
plot(m2)
plot(m3)
plot(m4)
plot(m5)

wald.asreml(m1)

lrt(m2, m3, boundary = F)


m4.pv <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals
m1.pv <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals



# stagewise ---------------------------------------------------------------

data1 <- na.omit(FD5)
data1 <- droplevels(data1)
str(data1)
head(data1)
summary(data1$cons_days)

data1 %>% dplyr::count(loc, cons_days1, sort = F)
data1 %>% dplyr::count(loc, year, harv_days3, sort = F) %>% rename(gen = n)

data1$harv_days3 <- as.factor(data1$harv_days3)
summary(data1$harv_days3)


data1$harv_days3 <- as.factor(data1$harv_days3)
summary(data1$harv_days3)


data1$harv_days4 <- scale(data1$cons_days1, scale=FALSE)
hist(data1$harv_days4)
data1$cons_days <- as.factor(data1$cons_days)
data1$env <- as.factor(data1$env)

levels(data1$cons_days)
summary(data1$env)

count(data1$env, data1$harv_days4)


M1 <- asreml::asreml(fixed = BLUE ~ loc + gen + cons_days1,
                     random = ~fa(env,1):gen,
                     data = data1,
                     weights = weight1,
                     na.action = list(x = "include", y = "include"),
                     family = asreml::asr_gaussian(dispersion = 1))


M2 <- asreml::asreml(fixed = BLUE ~ loc,
                     random = ~corgh(loc):gen:cons_days1,
                     data = data1,
                     weights = weight1,
                     na.action = list(x = "include", y = "include"),
                     family = asreml::asr_gaussian(dispersion = 1))



lrt(M1, M2, boundary = F)
infoCriteria.asreml(M1)
infoCriteria.asreml(M2)

summary(M1)$varcomp
summary(M2)$varcomp

wald(M1)
wald(M2)

if (solver=="SPATS") {
  predans <- predict.SpATS(object=ans,which="id",predFixed="marginal",
                           return.vcov.matrix = TRUE)
  tmp <- predans[,c("id","predicted.values")]
  colnames(tmp) <- c("id","BLUE")
  tmp2 <- Matrix(spam::as.dgCMatrix.spam(attr(predans,"vcov")))
  vcov[[j]] <- as(forceSymmetric(tmp2),"dspMatrix")
}



blue <- predict.asreml(m1, classify='gen', vcov=TRUE)
blue1 <- blue$pvals
blue1$weight <- (1/blue1$std.error)^2
blue1$weight <- diag(solve(vcov))
vcov <- blue$vcov
vcov[sel == 1, sel == 1]
?sel
solve(blue$vcov)

class(blue$vcov)

summary(m1)

blue1[ , ':='(var=se^2, smith.w=diag(solve(blue$vcov)))] # calculate the Smith's weight

## set up a factor and convert it to a numeric vector
a <- factor(rep(1:3, 4))
x <- as.numfac(a)
class(x)
class(a)
levels(x)

head(blue1)
class(vcov)
colnames(vcov)
dimnames(vcov) <- list(blue1$gen,blue1$gen)
class(vcov@x)
dimnames(vcov[[j]]) <- list(blue1$gen,blue1$gen)

lev1 <- c("env","loc","year","cut","check")
class(FD2)
FD2 <- as.data.frame(FD2)
FD2$check <- recode_factor(FD2$gen, "201" = "control", "202" = "control", .default = "test")
FD2[,lev1] <- lapply(FD2[,lev1], factor)
str(FD2)
data <- FD2


data <- data[order(data$row, data$col), ] # order data by row then col
head(data)
summary(data$check)


m3 <- asreml::asreml(fixed = resp ~ gen:at(check, "control") + lin(col), 
                     random = ~ block + gen:at(check, "test"),
                     residual = ~ar1(row):ar1(col), 
                     na.action = list(x = "include", y = "include"),
                     data = data)

m3 <- asreml::asreml(fixed = resp ~ gen, 
                     random = ~ block + row + col + units,
                     residual = ~ar1(row):ar1(col), 
                     na.action = list(x = "include", y = "include"),
                     data = data)

asreml.options(maxit=100) # Set asreml iteration
current.asrt <- as.asrtests(m3, NULL, NULL)
as.asrtests(asreml.obj, wald.tab = NULL, test.summary = NULL, 
            denDF = "numeric", label = NULL, 
            IClikelihood = "none", bound.exclusions = c("F","B","S","C"), ...)
wald(m3)

wald.asreml(m3)

?as.asrtests

summary(m3)$aic
summary(m1)$aic
?predictPlus.asreml
blue <- predict(m3, classify="gen", vcov=TRUE, aliased = T) # get the lsmeans

diag(blue$vcov)
solve(blue$vcov)
vcov <- as.matrix(blue$vcov)

vcov[is.na(vcov)] = 1
diag(vcov)
diag(solve(vcov))


head(predict.asreml(m3, classify='gen', vcov=TRUE)$pval)
blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pval


hist(blue$std.error)
hist(pvals$weight1)

# StageWise ---------------------------------------------------------------



diffs1 <- predictPlus(classify = "gen", 
                      asreml.obj = m1, 
                      wald.tab = NULL, 
                      present = c("gen","block"))

head(pvals)
ggplot(FD3, aes(x = reorder(gen, -BLUE), y = BLUE)) + geom_point(size = 0.5) + geom_errorbar(aes(ymin = BLUE-std.error, ymax = BLUE+std.error)) + theme_classic(base_family = "Arial", base_size = 12) + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + labs(title = "FD by loc with CI", x = "gen", y = "FD") + facet_wrap(. ~ env, ncol = 6)

FD8 <- FD3 %>% dplyr::filter(env == "ID_2019_4")
hist(FD8$weight1)

cor(FD8$weight1, FD8$weight, use = "complete.obs")

FD3$env <- as.factor(FD3$env)
levels(FD3$env)

ggplot(FD3, aes(x=weight1)) + geom_histogram() + facet_wrap(. ~ env, ncol = 6)

FD8 <- na.omit(FD8)
ggplot(FD8, aes(x = reorder(gen, -BLUE), y = BLUE)) + geom_point(size = 0.5) + geom_errorbar(aes(ymin = BLUE-std.error, ymax = BLUE+std.error)) + theme_classic(base_family = "Arial", base_size = 12) + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + labs(title = "FD by loc with CI", x = "gen", y = "FD")

####

# read genotypic data
G1 <- read.csv("~/Documents/", header = TRUE, row.names = 1, check.names = F)
G1[1:5,1:5]
dim(G1)
G2 <- G1 %>% unite(Chrom1, 1:2, remove = T)
G2 <- as.matrix(G2 %>% remove_rownames() %>% column_to_rownames(var = "Chrom1"))
have.both = intersect(rownames(pheno), colnames(G2))
G3 <- G2[,have.both]
G3[1:5,1:5]
dim(G3)
pheno1 <- pheno[have.both,]
colnames(pheno1)
pheno1 <- pheno1[1:(length(pheno)-4)]

# transform GWASpoly format "ACTG" to numeric format "0-4"
G3 <- as.data.frame(G3)
G3 <- t(G3)
numo <- atcg1234(data=G3, ploidy=4) 

G4 <- numo$M
dim(G4) # 187 3521
G4[1:5,1:5]
which(apply(G4, 2, var) == 0)

order1 <- match(rownames(pheno), rownames(G4))
G4  <- G4[order1,]




# all cuts FQ -------------------------------------------------------------
names(ST03)
data <- ST03[[1]] #ADF

data1 <- na.omit(data)
head(data1)
data1$cons_days <- as.factor(data1$cons_days)
levels(data1$cons_days)

data1 %>% dplyr::count(gen, cut, year)
nrow(data1)
data1 %>% dplyr::count(loc, cons_days, gen)

lev4 <- c("loc","cut","cons_days","gen")
data1[,lev4] <- lapply(data1[,lev4], factor)

hist(data1$predicted.value)
hist(data1$weight)

residual  = ~id(gen):id(cut):id(year):corgh(loc)

M1 <- asreml::asreml(fixed = predicted.value ~ loc * cut * FD,
                     random = ~ diag(loc):ar1(cons_days):gen,
                     data = data1, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1))
M1 <- update.asreml(M1)


M2 <- asreml::asreml(fixed = predicted.value ~ loc + cut + FD,
                     random = ~ diag(loc):ar1(cons_days):id(gen),
                     data = data1, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1))

M2 <- update.asreml(M2)


lrt(M1, M2, boundary = F)

wald.asreml(M1)
wald.asreml(M2)
infoCriteria.asreml(M1)
infoCriteria.asreml(M2)


summary(M1)$varcomp
summary(M2)$varcomp


current.asrt <- as.asrtests(M1, NULL, NULL)
current.asrt <- rmboundary.asrtests(current.asrt, update = T)
current.asrt$wald.tab

denDF = "numeric"

current.asrt <- as.asrtests(M2, NULL, NULL, denDF = "numeric")
current.asrt <- rmboundary.asrtests(current.asrt, update = T)
current.asrt$wald.tab


## Not run:
?recalcWaldTab
wald.tab <- recalcWaldTab(current.asrt, recalc.wald = F, denDF = "numeric",
                          dDF.na = "none",
                          dDF.values = NULL)

wald.tab


summary(current.asrt$asreml.obj)$varcomp

diffs4 <- predictPlus(classify = "cons_days:loc:FD", 
                      asreml.obj = M1, 
                      wald.tab = current.asrt$wald.tab, 
                      present = c("FD","gen","loc","cons_days","cut"))



ST2 <- diffs4[[1]]

ST2 <- ST2 %>% inner_join(., b2, by = "env")

ST2 <- ST2 %>% separate(2, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge") %>% inner_join(., b2, by = c("env","loc", "year"))
head(ST2)

ST2 <- ST2 %>% dplyr::filter(!year == "2021")
levels(ST2$harv_days3)
ST2$harv_days3 <- as.numeric(as.character(ST2$harv_days3))

ggplot(data = ST2, aes(x=harv_days3, y=predicted.value, group=FD)) + labs(col="loc") + theme_classic() + geom_point(aes(color=FD), size = 1) + geom_line(aes(color=FD), linetype = "dashed") + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + scale_x_continuous(limits = c(120, 300), breaks = seq(120, 300, by = 10)) + facet_grid(loc ~ year) 


ggplot(data = ST2, aes(x=cut, y=predicted.value, group=FD)) + labs(col="loc") + theme_classic() + geom_point(aes(color=FD), size = 1) + geom_line(aes(color=FD), linetype = "dashed") + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(loc ~ .) 

ggplot(data = ST2, aes(x=loc, y=predicted.value, group=FD)) + labs(col="loc") + theme_classic() +  geom_point(aes(color=FD), size = 1) + geom_line(aes(color=FD), linetype = "dashed") + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2))


current.asrt <- as.asrtests(M1, NULL, NULL)

current.asrt <- as.asrtests(M1, NULL, NULL,
                            label = "Maximal model", IClikelihood = "full")

current.asrt <- rmboundary.asrtests(current.asrt)
current.asrt <- rmboundary(current.asrt, IClikelihood = "full")


predictions <- predictPlus(classify = "FD:cut:loc", 
                     asreml.obj = M1, 
                     wald.tab = current.asrt$wald.tab, 
                     pairwise = T,
                     present = c("FD","gen","loc","cons_days","cut"))

pdif <- as.matrix(predictions[["p.differences"]])
class(pdif)
pdif[pdif < 0.001] <- "***"
pdif[pdif > 0.05] <- "ns"
pdif[pdif < 0.05 & pdif > 0.01] <- "*"
pdif[pdif < 0.01 & pdif > 0.001] <- "**"

pdif
pdif[upper.tri(pdif)] <- NA
pdif <- reshape2::melt(pdif, na.rm = T)
head(pdif)
# pdif <- pdif[,c(2,1,3)]
colnames(pdif) <- c("FD_A","FD_B","p.signif")

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

plotPredictions(classify="FD", y = "predicted.value",
                data = predictions[["predictions"]],
                present = c("FD","gen","loc","cons_days","cut"),
                error.intervals = "Confidence")



pdif <- as.matrix(predictions[["p.differences"]])

colnames(pdif)


pdif 
pdif <- pdif[rownames(pdif) %in% my_vec, colnames(pdif) %in% my_vec]

class(pdif)
pdif[pdif < 0.001] <- "***"
pdif[pdif > 0.05] <- "ns"
pdif[pdif < 0.05 & pdif > 0.01] <- "*"
pdif[pdif < 0.01 & pdif > 0.001] <- "**"

pdif
pdif[upper.tri(pdif)] <- NA
pdif <- reshape2::melt(pdif, na.rm = T)
pdif <- pdif[,c(2,1,3)]
colnames(pdif) <- c("FD_A","FD_B","p.signif")

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



plot1 <- ggplot(data = predictions[["predictions"]], aes(x=loc, y=predicted.value, group=FD)) + labs(col="loc") + theme_classic(base_family = "Arial", base_size = 12) + geom_point(aes(color=FD), size = 1) + geom_line(aes(color=FD), linetype = "dashed") + geom_linerange(aes(color=FD, ymin = lower.Confidence.limit, ymax = upper.Confidence.limit)) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), legend.key.size = unit(0.3, 'cm')) + labs(y = "", color = "FD")

ggsave(filename = "M2_Cprot.jpg", plot = plot1, width = 6, height = 6)


pdif <- pdif %>% separate(2, c("FD2", "cut2", "loc2"), sep = ",", remove = T, convert = FALSE, extra = "merge") %>% separate(1, c("FD1", "cut1", "loc1"), sep = ",", remove = T, convert = FALSE, extra = "merge") %>% dplyr::select(1,4,7) %>% unite("FD", c(FD2, FD1), sep = " - ", remove = T) %>% mutate(cut_loc = paste0("cut","_",j,"_","OR"))
head(pdif)

paste0("cut","_",j,"_","OR")


colnames(pdif)[c(1,4)] <- c("FD_cut4_OR1","FD_cut4_OR2")
pdif <- pdif[,c(1,4,7)]

# pdif <- pdif %>% dplyr::filter(loc2 == "OR") %>% dplyr::filter(loc1 == "OR") %>% dplyr::filter(cut2 == "4") %>% dplyr::filter(cut1 == "4")

a1 <- read.csv("~/Downloads/BLUPs_Yi_Roza2021.csv", row.names = 1)
cor(a1, use = "complete.obs")


library(ggfortify)

PCs <- prcomp(P00, scale=TRUE)
head(PCs)
class(PCs)
PCA1 <- autoplot(PCs, data = P00, label = T)
PCA1 + theme_bw()

b3$gen <- as.factor(b3$gen)


library(ggpubr)
library(rstatix)

class(predictions[["predictions"]])
p2 <- as.data.frame(predictions[["predictions"]])
str(p2)
p2$FD <- as.character(p2$FD)

p1 <- ggplot(data = p2, aes(x=FD, y=predicted.value, fill = FD)) + geom_bar(stat="identity") + theme_classic() + geom_linerange(aes(color=FD, ymin = lower.Confidence.limit, ymax = upper.Confidence.limit))

data <- ST1[[1]]
head(data[["FD"]][["predictions"]])

plot1 <- ggplot(data[["FD"]][["predictions"]], aes(x=FD, y=predicted.value)) + geom_bar(stat="identity", width=0.6) + theme_classic(base_family = "Arial", base_size = 12) + geom_linerange(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit)) + labs(y = "")

plot2 <- ggplot(data[["loc"]][["predictions"]], aes(x=loc, y=predicted.value)) + geom_bar(stat="identity", width=0.6) + theme_classic(base_family = "Arial", base_size = 12) + geom_linerange(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit)) + labs(y = "")

for (i in 1:length(ST1)) {
  data <- ST1[[i]]
  # yes 1
  plot1 <- ggplot(data = data[[4]][["predictions"]], aes(x=loc, y=predicted.value, group=FD)) + labs(col="loc") + theme_classic(base_family = "Arial", base_size = 12) + geom_point(aes(shape=FD), size = 1, alpha = 0.6) + geom_line(aes(linetype=FD), alpha = 0.6) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), legend.key.size = unit(0.3, 'cm'), legend.position = c(0.95, 0.4)) + labs(y = "", color = "FD")
  
  # yes 2
  plot2 <- ggplot(data = data[[6]][["predictions"]], aes(x=cut, y=predicted.value, group=FD)) + labs(col="loc") + theme_classic(base_family = "Arial", base_size = 12) + geom_point(aes(shape=FD), size = 1, alpha = 0.6) + geom_line(aes(linetype=FD), alpha = 0.6) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), legend.position = "none") + facet_grid(. ~ loc, scales = "free_x", space = "free_x", switch = "x") + labs(y = "", x = "cut")
  
  setwd("~/Documents/git/Norberg_2020/figs/second-stage1/")
  p1 <- ggarrange(plot1, plot2, ncol = 1, nrow = 2, align = "hv")
  ggsave(filename = paste0("2_", lev1[i], ".jpg"), plot = p1, width = 5, height = 5)

}


for (i in 1:length(ST1)) {
  data <- ST1[[i]]
  data1 <- data[[7]][["predictions"]]
  data1 <- inner_join(data1, b2, by = c("cons_days","loc"))
  data1$cons_days <- as.numeric(as.character(data1$cons_days))
  
  setwd("~/Documents/git/Norberg_2020/figs/second-stage1/")
  plot3 <- ggplot(data = data1, aes(x=date, y=predicted.value, group=FD)) + labs(col="loc") + theme_classic(base_family = "Arial", base_size = 12) + geom_point(aes(shape=FD), size = 1, alpha = 0.6) + geom_line(aes(linetype=FD), alpha = 0.6) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(loc ~ ., scales = "free_x", space = "free_x", switch = "y") + labs(y = "", x = "") + scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
  
  ggsave(filename = paste0("3_", lev1[i], ".jpg"), plot = plot3, width = 10, height = 6)
}



for (i in 1:length(ST1)) {
  data <- ST1[[i]]
  data1 <- data[[7]][["predictions"]]
  data1 <- inner_join(data1, b2, by = c("cons_days","loc"))
  data1$cons_days <- as.numeric(as.character(data1$cons_days))
  
  setwd("~/Documents/git/Norberg_2020/figs/second-stage1/")
  plot3 <- ggplot(data = data1, aes(x=date, y=predicted.value, group=FD)) + labs(col="loc") + theme_classic(base_family = "Arial", base_size = 12) + geom_point(aes(shape=FD), size = 1, alpha = 0.6) + geom_line(aes(linetype=FD), alpha = 0.6) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(loc ~ ., scales = "free_x", space = "free_x", switch = "y") + labs(y = "", x = "") + scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
  
  ggsave(filename = paste0("3_", lev1[i], ".jpg"), plot = plot3, width = 10, height = 6)
}




str(data[[7]][["predictions"]])
data1 <- data[[7]][["predictions"]]

data1 <- inner_join(data1, b2, by = c("cons_days","loc"))
str(data1)
data1$cons_days <- as.numeric(as.character(data1$cons_days))

# yes
ggplot(data = data1, aes(x=date, y=predicted.value, group=FD)) + labs(col="loc") + theme_classic(base_family = "Arial", base_size = 12) + geom_point(aes(shape=FD), size = 1, alpha = 0.6) + geom_line(aes(linetype=FD), alpha = 0.6) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), legend.position = "none") + facet_grid(loc ~ ., scales = "free_x", space = "free_x", switch = "y") + labs(y = "", x = "") + scale_x_date(date_breaks = "months" , date_labels = "%b-%y")




ggplot(data = data[[7]][["predictions"]], aes(x=harv_days3, y=predicted.value, color = FD)) + labs(col="loc") + theme_classic(base_family = "Arial", base_size = 12) + geom_point(aes(color=FD), size = 1) + geom_line(aes(color=FD), linetype = "dashed") + geom_linerange(aes(color=FD, ymin = lower.Confidence.limit, ymax = upper.Confidence.limit)) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), legend.position = "none") + scale_x_continuous(limits = c(120, 300), breaks = seq(120, 300, by = 10)) + facet_grid(loc ~ year) + labs(y = "", x = "Days")


