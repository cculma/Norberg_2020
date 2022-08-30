

lev0 <- subset(colnames(a1),  grepl("^ST1_FD", colnames(a1)))
lev1 <- subset(colnames(a1),  grepl("^ST1_DM", colnames(a1)))
lev3 <- subset(colnames(a1),  grepl("^ST3", colnames(a1)))
lev4 <- subset(colnames(a1),  grepl("^ST4", colnames(a1)))

lev3.1 <- lev3[-c(13:15)]
lev4.1 <- lev4[-5]

lev2 <- c(lev0, lev1)
a00 <- a1[,lev2]
a00 <- a1[,lev3]
a00 <- a1[,lev4]

a00 <- a1[,lev3.1]
a00 <- a1[,lev4.1]

colnames(a00)
colnames(a3)
a3.1 <- a3[,c(1,5)]

a00 <- a00 %>% rownames_to_column("gen")
a00 <- inner_join(a00, a3.1, by = "gen")


colnames(a00) <- gsub("ST3_", "", colnames(a00))
head(a00)
colnames(a00)
a00 <- a00[,-17]
a00 <- a00 %>% column_to_rownames("gen")
P00 <- cor(a00, use = "complete.obs")

P1 <- ggcorrplot(P00, hc.order = TRUE, type = "lower",lab = TRUE, lab_size = 2)
 
ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/figures/GWAS/cor_ST4.pdf", plot = a02, width = 8, height = 6, device = cairo_pdf)
          
P1 <- ggcorrplot(P00[,ncol(P00):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + scale_fill_gradient(low = "white", high = "orangered") + theme_ipsum(base_family = "Arial", base_size = 7) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/figures/GWAS/Cor_ST3.pdf", plot = P1, width = 5, height = 5, device = cairo_pdf)


library(GGally)
library(ggplot2)
data(iris)

lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  p
}

ggpairs(
  iris[, 1:4], lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "blue")),
  upper = list(continuous = wrap("cor", size = 10))
)





head(a00)
colnames(a00)
a01 <- a00 %>% rownames_to_column("gen") %>% gather(key = "trait", "BLUP", 2:16) %>% separate(col = 2, into = c("trait", "loc"), sep = "_", remove = T, convert = FALSE, extra = "merge") %>% spread(key = trait, value = BLUP)

a03 <- aggregate(a01$BLUP ~ a01$trait, FUN=mean)
a04 <- aggregate(a01$BLUP ~ a01$trait, FUN=sd)
colnames(a03) <- c("trait", "mean")
colnames(a04) <- c("trait", "sd")
a03 <- inner_join(a03, a04, by = "trait")

head(a01)
str(a01)
a01$loc <- as.factor(a01$loc)

a02 <- a01 %>% ggpairs(., legend = 1, columns = c(3:7),
                aes(color = factor(loc)), 
                upper = list(continuous = wrap('cor', size = 3, stars = F)),
                lower = list(continuous = wrap("points", alpha = 0.4, size=0.5), combo = wrap("dot_no_facet")),
                diag = list(continuous = wrap("densityDiag", alpha = 0.5 ))) + theme_bw(base_family = "Arial", base_size = 12) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()) 

ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/figures/GWAS/ggpairs_ST3.pdf", plot = a02, width = 8, height = 6, device = cairo_pdf)

ggcorr(a00, method = c("everything", "pearson")) 
