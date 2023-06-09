# Calculations for expected values of r2 under drift equilibrium [Hill and Weir (1988)
# implemented in Remington, et al. (2001)]

# Calculate and save LD statistics from TASSEL using sliding window of 50 SNPs and maf = 0.05
# read in table with NaN and distance calculated from TASSEL or wherever


###############################################################################
### 		For more detail, please see following reference
###############################################################################
## Remington, D. L., Thornsberry, J. M., Matsuoka, Y.,
## Wilson, L. M., Whitt, S. R., Doebley, J., ... & Buckler, E. S. (2001). Structure of linkage disequilibrium
## and phenotypic associations in the maize genome. 
## Proceedings of the national academy of sciences, 98(20), 11479-11484. 
## https://doi.org/10.1073/pnas.201394398
###############################################################################

setwd("~/Documents/git/Norberg_2020/LD_decay/")
# import TASSEL LD output file
ld <- read.delim(file.choose(),stringsAsFactors = FALSE,header=TRUE, sep = "\t") 
str(ld)

##remove sites that have NaN for distance or r2
ld_sub <- ld[ld$R.2 != "NaN",]
ld_sub$dist <- as.numeric(ld_sub$Dist_bp)
ld_sub2 <- ld_sub[ld_sub$dist != "NaN",]
ld_sub2$rsq <- ld_sub2$R.2

colnames(ld_sub2)
file <- ld_sub2[,c(1,2,7,8,15:19)]

# C values range from about 0.5 to 2, start with 0.1
Cstart <- c(C=0.1)


# fit a non linear model using the arbitrary C value, 
# N is the number of the genotypes that have the SNP site
modelC <- nls(rsq ~ ( (10+C*dist)/( (2+C*dist) * (11+C*dist) ) ) * 
                ( 1+( (3+C*dist) * (12+12*C*dist+(C*dist)^2) ) / ( 2*N*(2+C*dist) * (11+C*dist) ) ), 
              data=file, start=Cstart, control=nls.control(maxiter=100))

# extract rho, the recombination parameter in 4Nr
rho <- summary(modelC)$parameters[1]

# feed in the new value of rho to obtain LD values adjusted for their distances along the chromosome/genome
newrsq <- ( (10+rho*file$dist) / ( (2+rho*file$dist) * (11+rho*file$dist) ) ) *
  ( 1 + ( (3+rho * file$dist) * (12+12*rho*file$dist + (rho*file$dist)^2) ) / 
      (2*file$N*(2+rho*file$dist) * (11+rho*file$dist) ) )

newfile <- data.frame(file$dist, newrsq)

maxld <- max(newfile$newrsq,na.rm=TRUE) #using max LD value from adjusted data
halfdecay = maxld*0.5
halfdecaydist <- newfile$file.dist[which.min(abs(newfile$newrsq-halfdecay))]
newfile <- newfile[order(newfile$file.dist),]

# plotting the values
pdf("LD_decay_Norberg_CM.pdf", height=5, width = 5)
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 4, 0, 0)) 
plot(file$dist, file$rsq, pch=".", cex=2, xlab="Distance (bp)", ylab=expression(LD ~ (r^2)), col="grey")
lines(newfile$file.dist, newfile$newrsq, col="red", lwd=2)
abline(h=0.1, col="blue") # if you need to add horizontal line
abline(v=halfdecaydist, col="green")
mtext(round(halfdecaydist,2), side=1, line=0.05, at=halfdecaydist, cex=0.75, col="green")
dev.off()


#######
length(distance)
distance=c(19,49,1981,991,104,131,158,167,30,1000,5000,100,150,11,20,33,1100,1300,1500,100,1400,900,300,100,2000,100,1900,500,600,700,3000,2500,400,792)
LD.data=c(0.6,0.47,0.018,0.8,0.7,0.09,0.09,0.05,0,0.001,0,0.6,0.4,0.2,0.5,0.4,0.1,0.08,0.07,0.5,0.06,0.11,0.3,0.6,0.1,0.9,0.1,0.3,0.29,0.31,0.02,0.01,0.4,0.5)
n=52

HW.st=c(C=0.1)

HW.nonlinear=nls(LD.data~((10+C*distance)/((2+C*distance)*(11+C*distance)))*(1+((3+C*distance)*(12+12*C*distance+(C*distance)^2))/(n*(2+C*distance)*(11+C*distance))),start=HW.st,control=nls.control(maxiter=100))

tt=summary(HW.nonlinear)
new.rho=tt$parameters[1]

fpoints=((10+new.rho*distance)/((2+new.rho*distance)*(11+new.rho*distance)))*(1+((3+new.rho*distance)*(12+12*new.rho*distance+(new.rho*distance)^2))/(n*(2+new.rho*distance)*(11+new.rho*distance)))
