# stagewise
rm(list = ls())
library(StageWise)
library(asreml)
library(ggplot2)

geno.file <- system.file("vignette_data", "geno2.csv", package = "StageWise")
geno <- read_geno(filename=geno.file,ploidy=4,map=FALSE)

pheno.file <- system.file("vignette_data", "pheno2.csv", package = "StageWise")
pheno <- read.csv(pheno.file)
head(pheno)

ans1 <- Stage1(filename=pheno.file,
               traits='Yield.Mg.ha')

head(ans1$blue)

# ggplot(data=ans1$H2,aes(x=loc,y=H2)) + stat_boxplot(outlier.color="red") + xlab("Location") + ylab(expression(paste("Broad-sense ",H^2," (plot basis)")))

ans2a <- Stage2(data=ans1$blue, vcov=ans1$vcov)
summary(ans2a$vars)


# plot(hclust(as.dist(1-summary(ans2a$vars)[[2]])), hang = -1,xlab="")
# ans2a$uniplot

locs <- c("WI","MI","OR","NY","NC","MO")
blues <- ans1$blue[ans1$blue$loc %in% locs,]
head(blues)
tmp <- sapply(strsplit(names(ans1$vcov),split="_"),"[[",1)
vcov <- ans1$vcov[tmp %in% locs]

ans2b <- Stage2(data=blues,vcov=vcov,geno=geno)
summary(ans2b$vars)

# ans2b$uniplot

prep1 <- blup_prep(data=blues,vcov=vcov,geno=geno,vars=ans2b$vars)

WI.index <- c(WI=1, NC=0, OR=0, NY=0, MO=0, MI=0)
NC.index <- c(WI=0, NC=1, OR=0, NY=0, MO=0, MI=0)  

WI.pred <- blup(data=prep1,geno=geno,index.weights=WI.index,what="id")
NC.pred <- blup(data=prep1,geno=geno,index.weights=NC.index,what="id")

pred <- merge(NC.pred[,c("id","BV","BV.r2")],WI.pred[,c("id","BV","BV.r2")],by="id")
colnames(pred) <- c("id","BV.NC","r2.NC","BV.WI","r2.WI")

# ggplot(pred,aes(x=r2.NC,y=r2.WI)) + geom_point() + coord_fixed(ratio=1) + xlim(0.3,0.8) + ylim(0.3,0.8) + geom_line(data=data.frame(x=c(0.3,0.8),y=c(0.3,0.8)),mapping=aes(x=x,y=y),linetype=2) + theme_bw() + xlab("NC") + ylab("WI") + ggtitle("BV Reliability")

cor(pred$BV.NC, pred$BV.WI)
# 

WI.env <- blues$env[blues$loc=="WI"]

prep2 <- blup_prep(data=blues,vcov=vcov,geno=geno,vars=ans2b$vars,
                   mask=data.frame(env=WI.env))

pred2 <- blup(data=prep2,geno=geno,index.weights=WI.index,what="id")

plot.data <- merge(pred2[,c("id","BV.r2")],WI.pred[,c("id","BV.r2")],by="id")
colnames(plot.data) <- c("id","without.WI.pheno","with.WI.pheno")

# ggplot(plot.data,aes(x=without.WI.pheno,y=with.WI.pheno)) + geom_point() + coord_fixed(ratio=1) + xlim(0.3,0.8) + ylim(0.3,0.8) + geom_line(data=data.frame(x=c(0.3,0.8),y=c(0.3,0.8)),mapping=aes(x=x,y=y),linetype=2) + theme_bw() + xlab("Without WI phenotypes") + ylab("With WI phenotypes") + ggtitle("WI BV Reliability")

save.image("~/Documents/Cesar/git/big_files/stagewise_vigenette2.RData")

