library(sommer)


p5.1 <- LD.plot(data_5.1)
p5.2 <- LD.plot(data_5.2)
p5.3 <- LD.plot(data_5.3)
p5.4 <- LD.plot(data_5.4)

ls()
p5.5 <- LD.plot(data_5.5)
class(p5.5)
ggsave(filename = "~/Documents/Cesar/git/big_files/FD_LD.jpg", plot = p5.5, width = 6, height = 6)




data(DT_cpdata)
#### get the marker matrix
CPgeno <- GT_cpdata
CPgeno[1:5,1:5]
dim(CPgeno)
class(CPgeno)
#### get the map
mapCP <- MP_cpdata 
head(mapCP)

names(mapCP) <- c("Locus","Position","LG")
#### with example purposes we only do 3 chromosomes
mapCP <- mapCP[which(mapCP$LG <= 3),]

### run the function
res <- LD.decay(CPgeno, mapCP)
names(res)
#### subset only markers with significant LD
res$all.LG <- res$all.LG[which(res$all.LG$p < .001),]
#### plot the LD decay
with(res$all.LG, plot(r2~d,col=transp("cadetblue"),
                     xlim=c(0,55), ylim=c(0,1),
                     pch=20,cex=0.5,yaxt="n",
                     xaxt="n",ylab=expression(r^2),
                     xlab="Distance in cM")
                     )
axis(1, at=seq(0,55,5), labels=seq(0,55,5))
axis(2,at=seq(0,1,.1), labels=seq(0,1,.1), las=1)

#### if you want to add the loess regression lines
#### this could take a long time!!!
mod <- loess(r2 ~ d, data=res$all.LG)
par(new=T)
lilo <- predict(mod,data.frame(d=1:55))
plot(lilo, bty="n", xaxt="n", yaxt="n", col="green",
     type="l", ylim=c(0,1),ylab="",xlab="",lwd=2)

res3 <- LD.decay(markers=CPgeno, map=mapCP,
                unlinked = TRUE,gamma = .95)

abline(h=res3$all.LG, col="red")

?LD.decay

