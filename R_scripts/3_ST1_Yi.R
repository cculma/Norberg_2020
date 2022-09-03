lev2 <- c("block", "ID", "gen", "row", "col")
M_Yi <- list()
BLUE_Yi <- list()

Y2 <- c(R_Yi, Y1)

data <- Y2[[1]] 
head(data)
str(data)

for (i in 1:length(Y2)) {
  data <- Y2[[i]]
  data[,lev2] <- lapply(data[,lev2], factor)
  data <- data[order(data$row, data$col), ]
  
  m1 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2,  
                       random = ~ + block + spl(row), 
                       residual = ~sar(row):sar(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block + spl(row), 
                       residual = ~ar1(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block + spl(row), 
                       residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  info1 <- infoCriteria.asreml(m1)
  info2 <- infoCriteria.asreml(m2)
  info3 <- infoCriteria.asreml(m3)
  
  info1$model <- "sar_sar"
  info2$model <- "ar1_id"
  info3$model <- "ar1_ar1"
  
  data1 <- rbind(info1, info2, info3)
  M_Yi[[length(M_Yi)+1]] = data1
  
  ifelse(info1$AIC < info2$AIC && info1$AIC < info3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(info2$AIC < info1$AIC && info2$AIC < info3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(info3$AIC < info1$AIC && info3$AIC < info2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
  BLUE_Yi[[length(BLUE_Yi)+1]] = blue
}

names(M_Yi) <- names(Y2)
M_Yi <-rbindlist(M_Yi, use.names=TRUE, fill=TRUE, idcol="trait")
M_Yi[ , .SD[which.min(AIC)], by = trait]

names(BLUE_Yi) <- names(Y2)

BLUE_Yi <-rbindlist(BLUE_Yi, use.names=TRUE, fill=TRUE, idcol="trait")
head(BLUE_Yi)

BLUE_Yi1 <- BLUE_Yi %>% dplyr::select(1:3) %>% spread(key = trait, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

colnames(BLUE_Yi1)[2:length(BLUE_Yi1)] <- gsub("^", "ST0_", colnames(BLUE_Yi1)[2:length(BLUE_Yi1)])


BLUE_Yi2 <- BLUE_Yi %>% dplyr::filter(!gen %in% c(201, 202)) %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")

BLUE_Yi2 <- BLUE_Yi %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")

# colnames(BLUE_Yi1)[2:length(BLUE_Yi1)] <- gsub("^", "ST0_Yi_", colnames(BLUE_Yi1)[2:length(BLUE_Yi1)])


a6 <- BLUE_Yi1 %>% remove_rownames() %>% column_to_rownames("gen")

Y4 <- cor(a6, use = "complete.obs")

ggcorrplot(Y4[,ncol(Y4):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + theme_classic(base_family = "Arial", base_size = 12) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank()) + labs(title = "ST0")

# save.image("~/Documents/git/big_files/tidy_Norberg_Yi.RData")
# load("~/Documents/git/big_files/tidy_Norberg_Yi.RData")
############

# ST1

hist(BLUE_Yi2$BLUE)
head(BLUE_Yi2)


data <- BLUE_Yi2
colnames(data)[1] <- "env"
lev3 <- colnames(data)[1:5]
data <- as.data.frame(data)
data[,lev3] <- lapply(data[,lev3], factor)
data <- data[order(data$gen, data$env), ]
str(data)
levels(data$env)
names(R_Yi)
names(Y1)

data <- dplyr::filter(env %in% names(R_Yi))
data <- dplyr::filter(env %in% names(Y1))

data1 <- data %>% drop_na(BLUE)
head(data1)

FA_3 <- asreml::asreml(fixed = BLUE ~ 1 +  loc, 
                       random = ~ + fa(env, 3):id(gen),
                       data = data1, na.action = list(x = "include", y = "include"), 
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))
FA_3 <- update.asreml(FA_3)
# comp1 <- summary(FA_1)$varcomp

# BLUP1 <- predict.asreml(FA_1, classify='gen:env', vcov=TRUE, )$pvals
# asreml.options(workspace="1024mb")

preds <- predict(FA_3, classify = "gen:env", vcov = F)
BLUP2 <- preds$pvals

BLUP3 <- predictPlus(classify = "gen:loc", asreml.obj = FA_3, 
                     wald.tab = NULL, 
                     present = c("env", "loc", "gen"))$predictions

BLUP4 <- predictPlus(classify = "gen", asreml.obj = FA_3, 
                     wald.tab = NULL, 
                     present = c("env", "loc", "gen"))$predictions


a6 <- BLUP2 %>% select(1:3) %>% spread(key = env, value = predicted.value) %>% remove_rownames() %>% column_to_rownames("gen")

Y3 <- cor(a6, use = "complete.obs")

Y4[lower.tri(Y4)] <- Y3[lower.tri(Y3)]

ggcorrplot(Y4[,ncol(Y4):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + theme_classic(base_family = "Arial", base_size = 12) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank()) + labs(title = "Single-stage vs Stagewise Yield")

ST1 <- BLUP2 %>% select(1:3) %>% spread(key = env, value = predicted.value)
ST2 <- BLUP3 %>% select(1:3) %>% spread(key = loc, value = predicted.value)
ST3 <- BLUP4 %>% select(1:2)
colnames(ST3)[2] <- "Yi"

BLUE_Yi1

Yi1 <- inner_join(BLUE_Yi1, ST1, by = "gen") %>% inner_join(., ST2, by = "gen") %>% inner_join(., ST3, by = "gen") %>% inner_join(., PCA, by = "gen") 

write.csv(Yi1, "~/Documents/git/big_files/Sum_yield.csv", quote = F, row.names = F)

###########
PCA <- read.csv("~/Documents/git/big_files/pheno.csv")
PCA <- PCA[,c(1,109:111)]
head(PCA)
PCA$gen <- as.factor(PCA$gen)
##########

BLUE_Yi2

ggplot(BLUE_Yi2, aes(x = trait, y = BLUE)) + geom_boxplot(alpha = 0.6, width=0.6, position = position_dodge(width=0.8, preserve = "single")) + theme_bw(base_family = "Arial") + theme(legend.position = "none", panel.spacing = unit(0.3, "lines"), strip.text.x = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title = element_text(size = 12)) 

box1 <- list()
for (i in 1:length(ST1)) {
  a6 <- ST1[[i]] %>% separate(2, c("loc", "year"), sep = "_", remove = F, convert = FALSE, extra = "merge")
  lev3 <- names(ST1)[i]
  g1 <- ggplot(a6, aes(x = env, y = predicted.value)) + geom_boxplot(alpha = 0.6, width=0.6, position = position_dodge(width=0.8, preserve = "single")) + theme_bw(base_family = "Arial") + theme(legend.position = "none", panel.spacing = unit(0.3, "lines"), strip.text.x = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title = element_text(size = 12)) + labs(title = paste(lev3, "ST1"), y = "", x = "")
  box1[[length(box1)+1]] <-  g1
}
names(box1) <- names(ST1)

box1[[7]] + box1[[6]] + box1[[1]] + box1[[3]] + plot_layout(ncol = 2)
box1[[4]] + box1[[9]] + box1[[11]] + box1[[15]] + plot_layout(ncol = 2)
box1[[5]] + box1[[14]] + box1[[10]] + box1[[12]] + plot_layout(ncol = 2)
box1[[8]] + box1[[16]] + box1[[2]] + box1[[13]] + plot_layout(ncol = 2)
