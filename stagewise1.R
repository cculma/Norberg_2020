# stagewise
rm(list = ls())
library(StageWise)
library(asreml)

pheno1a.file <- system.file("vignette_data", "pheno1a.csv", package = "StageWise")
pheno1a <- read.csv(pheno1a.file)
knitr::kable(head(pheno1a))
str(pheno1a)
head(pheno1a)
trait1 <- colnames(pheno1a)[1:(length(colnames(pheno1a))-5)]
trait1 <- c("env", "id", "block", "stand.count")
pheno1a[, trait1] <- lapply(pheno1a[, trait1], factor)


pheno1b.file <- system.file("vignette_data", "pheno1b.csv", package = "StageWise")
pheno1b <- read.csv(pheno1b.file)
knitr::kable(head(pheno1b))

effects <- data.frame(name=c("block","stand.count"),
                      fixed=c(FALSE,TRUE),
                      factor=c(TRUE,FALSE))
effects
head(pheno1a.file)

ans1a <- Stage1(filename=pheno1a.file,traits="total.yield",
                effects=effects,solver="asreml")

head(ans1a$blue)
st1 <- ans1a$blue
str(st1)
trait2 <- c("env", "id")
st1[, trait2] <- lapply(st1[, trait2], factor)

##############

effects <- data.frame(name=c("row","range","stand.count","trial"),
                      fixed=c(FALSE,FALSE,TRUE,TRUE),
                      factor=c(TRUE,TRUE,FALSE,TRUE))
effects

model1 <- Stage1(filename=pheno1b.file, traits="total.yield",
                 effects=effects, solver="asreml")

model1$H2

model2 <- Stage1(filename=pheno1b.file, traits="total.yield",
                 effects=effects[3:4,], solver="spats", spline=c("row","range"))

# compare <- merge(model1$blue,model2$blue,by=c("id","env"))
# ggplot(data=compare,aes(x=BLUE.x,y=BLUE.y)) + geom_point() + xlab("i.i.d. Random Effects") + ylab("2D Spline") + theme_bw() + geom_abline(intercept=0,slope=1) + coord_cartesian(xlim=c(25,90),ylim=c(25,90)) + ggtitle("2020 Yield BLUEs (Mg/ha)")
# model2$resid$spatial

stage1.blue <- rbind(ans1a$blue,model1$blue)
head(stage1.blue)
str(stage1.blue)
trait2 <- c("env", "id")
stage1.blue[, trait2] <- lapply(stage1.blue[, trait2], factor)
levels(stage1.blue$env)
levels(stage1.blue$id)

stage1.vcov <- c(ans1a$vcov,model1$vcov)


####################
# marker data

geno.file <- system.file("vignette_data", "geno1.csv", package = "StageWise")
geno <- read.csv(geno.file,check.names=F)
geno[1:5,1:5]
dim(geno)
geno <- read_geno(filename=geno.file, ploidy=4, map=TRUE, min.minor.allele=5)

ans2a <- Stage2(data=stage1.blue,vcov=NULL)
ans2b <- Stage2(data=stage1.blue,vcov=stage1.vcov)
data.frame(vcov=c(FALSE,TRUE),AIC=c(ans2a$aic,ans2b$aic))

ans2c <- Stage2(data=stage1.blue,vcov=stage1.vcov,geno=geno,silent=FALSE)
ans2c$aic

summary(ans2a$vars)
summary(ans2b$vars)
summary(ans2c$vars)

##############
# Pedigree data

ped.file <- system.file("vignette_data", "ped.csv", package = "StageWise")
ped <- read.csv(ped.file)
geno2 <- read_geno(geno.file,ploidy=4,map=TRUE,ped=ped,w=0.1)
ans2d <- Stage2(data=stage1.blue,vcov=stage1.vcov,geno=geno2)
ans2d$aic
summary(ans2d$vars)

w.vec <- c(1e-5, seq(0.1,0.8,by=0.1))
geno <- read_geno(geno.file,ploidy=4,map=TRUE,ped=ped,w=w.vec)

result <- data.frame(w=w.vec, aic=numeric(9), h2=numeric(9))
for (i in 1:9) {
  ans2 <- Stage2(data=stage1.blue,vcov=stage1.vcov,geno=geno[[i]])
  result$aic[i] <- ans2$aic
  result$h2[i] <- summary(ans2$vars)[1,1]
}
axis.scaling <- diff(range(result$h2))/diff(range(result$aic))
result$y2 <- (result$aic-min(result$aic))*axis.scaling + min(result$h2)
y2lab <- round(seq(min(result$aic),max(result$aic),length.out=5))
y2axis <- y2lab-min(result$aic) + min(result$h2)/axis.scaling

# ggplot(result) + geom_line(mapping=aes(x=w,y=h2)) + geom_line(mapping=aes(x=w,y=y2),colour="red") + scale_y_continuous(name="Genomic h2",sec.axis=sec_axis(trans~./axis.scaling,name="AIC",breaks=y2axis,labels=y2lab)) + theme_bw() +  
#   theme(axis.text.y.right=element_text(colour="red"),axis.title.y.right=element_text(colour="red")) + ggtitle("Blending G and A for Yield")

ans1 <- Stage1(pheno1a.file,traits="vine.maturity",
               effects=data.frame(name=c("block","stand.count"),
                                  fixed=c(FALSE,TRUE),
                                  factor=c(TRUE,FALSE)))

for (i in 1:9) {
  ans2 <- Stage2(data=ans1$blue,vcov=ans1$vcov,geno=geno[[i]])
  result$aic[i] <- ans2$aic
  result$h2[i] <- summary(ans2$vars)[1,1]
}
axis.scaling <- diff(range(result$h2))/diff(range(result$aic))
result$y2 <- (result$aic-min(result$aic))*axis.scaling + min(result$h2)
y2lab <- round(seq(min(result$aic),max(result$aic),length.out=5))
y2axis <- y2lab-min(result$aic) + min(result$h2)/axis.scaling

# ggplot(result) + geom_line(mapping=aes(x=w,y=h2)) + geom_line(mapping=aes(x=w,y=y2),colour="red") + scale_y_continuous(name="Genomic h2",sec.axis=sec_axis(trans~./axis.scaling,name="AIC",breaks=y2axis,labels=y2lab)) + theme_bw() + theme(axis.text.y.right=element_text(colour="red"),axis.title.y.right=element_text(colour="red")) + ggtitle("Blending G and A for Vine Maturity")

###############
# BLUP reliability

geno1 <- geno[[6]] #geno[[5]] corresponds to w=0.5
ans2 <- Stage2(data=stage1.blue,vcov=stage1.vcov,geno=geno1)
prep <- blup_prep(data=stage1.blue,
                  vcov=stage1.vcov,
                  geno=geno1,
                  vars=ans2$vars)
pred.id <- blup(prep,geno=geno,what="id")
knitr::kable(head(pred.id),digits=2)

prep <- blup_prep(data=stage1.blue,
                  vcov=stage1.vcov,
                  vars=ans2b$vars)
pred2.id <- blup(prep,what="id")

plot.data <- merge(pred2.id,pred.id,by="id")
ggplot(plot.data,aes(x=GV.r2.x,y=GV.r2.y)) + geom_point() + ggtitle("GV Reliability") + theme_bw() + 
  xlab("Without markers") + ylab("With markers") + coord_fixed(ratio=1) + ylim(0.4,1) + xlim(0.4,1) + geom_line(data=data.frame(x=c(0.4,1),y=c(0.4,1)),mapping=aes(x=x,y=y),linetype=2)


##############
# marker and GWAS

geno1 <- geno[[1]]  #w = 0
ans2 <- Stage2(data=ans1$blue,vcov=ans1$vcov,geno=geno1)

prep <- blup_prep(data=ans1$blue,vcov=ans1$vcov,geno=geno1,vars=ans2$vars)
pred.marker <- blup(data=prep,geno=geno1,what="marker",gwas.ncore= 32)
head(pred.marker)

#compare with G-BLUP
RR_BLUP <- predict(geno1,pred.marker)
G_BLUP <- blup(data=prep,geno=geno,what="id")
# plot.data <- merge(RR_BLUP,G_BLUP,by="id")
# ggplot(plot.data,aes(x=BV.x,y=BV.y)) + geom_point() + xlab("RR-BLUP") + ylab("G-BLUP") + theme_bw()

geno1 <- geno[[3]] #w = 0.2
ans2 <- Stage2(data=ans1$blue,vcov=ans1$vcov,geno=geno1)
prep <- blup_prep(data=ans1$blue,vcov=ans1$vcov,geno=geno1,vars=ans2$vars)
pred.marker <- blup(data=prep,geno=geno1,what="marker",gwas.ncore=2)

RR_BLUP <- predict(geno1,pred.marker)
H_BLUP <- blup(data=prep,geno=geno,what="id")
plot.data <- merge(RR_BLUP,H_BLUP,by="id")
# ggplot(plot.data,aes(x=BV.x,y=BV.y)) + geom_point() + xlab("RR-BLUP") + ylab("H-BLUP") + theme_bw()

gwas_threshold(geno1,alpha=0.05,n.core=2)

# manhattan_plot(pred.marker,thresh=5.1,rotate.label=TRUE)

k <- which.max(pred.marker$gwas.score)
pred.marker[k,]

ans2f <- Stage2(data=ans1$blue,
                vcov=ans1$vcov,
                geno=geno1,
                fix.eff.marker="solcap_snp_c2_22964")

# Fixed effect for the marker
ans2f$fixed$marker

# Proportion of variance
summary(ans2f$vars)

save.image("~/Documents/Cesar/git/big_files/stagewise_vigenette1.RData")
load("~/Documents/Cesar/git/big_files/stagewise_vigenette1.RData")
