
library(foreach)
library(doParallel)

score.list <- parLapply(cl,X=data2,fun=function(x,other){.score.calc(data@geno[,x$markers,drop=FALSE],y=other$y,Z=other$Z,X=other$X,K=x$K,Hinv=NULL,ploidy=other$ploidy,model=other$model,min.MAF=other$min.MAF,max.geno.freq=other$max.geno.freq)},other=list(y=y,Z=Z,X=X2,ploidy=data@ploidy,model=models[k],min.MAF=params$min.MAF,max.geno.freq=params$max.geno.freq))


data <- PH_WA_HC_data_3.3

# Start the clock!
ptm <- proc.time()
class(r2)


for_test <- function(x){
  ix <- which(data@map[,2]==i)
  r2[[i]] <- as(cor(data@geno[,ix])^2,"dspMatrix")
}

r2
for (i in chrom) {
  ix <- which(data@map[,2]==i)
  r2[[i]] <- as(cor(data@geno[,ix])^2,"dspMatrix")
}

class(rownames(r2[[1]]))

model.mse <- function(x) {
  id <- sample(1:nrow(Boston), 200, replace = T)
  mod <- lm(medv ~ ., data = Boston[id,])
  mse <- mean((fitted.values(mod) - Boston$medv[id])^2)
  return(mse)
}
class(r2[[1]])
class(rownames(r2[[1]]))
row.names(rownames(r2[[1]])) <- attr(r2[[1]], "row.names")
rownames(r2[[1]]) <- as.integer(rownames(r2[[1]]))
r2[[1]][1:5,1:5]
colnames(r2[[1]]) <- as.integer(colnames(r2[[1]]))

for_test <- function(i) {
    ix <- which(data@map[,2]==i)
    r2[[1]] <- as(cor(data@geno[,ix])^2,"dspMatrix")
}
str(for_test())

cl <- parallel::makeCluster(6)
doParallel::registerDoParallel(cl)
foreach(i = seq_along(chrom)) %dopar% {
  ix <- which(data@map[,2]==i)
  r2[[i]] <- as(cor(data@geno[,ix])^2,"dspMatrix")
}







system.time(
  for (i in chrom) {
    ix <- which(data@map[,2]==i)
    r2[[1]] <- as(cor(data@geno[,ix])^2,"dspMatrix")
  }
  
)

system.time(r2 <- parLapply(cl, r2, for_test))



# for
system.time({
  model.mse.output <- rep(NA, 1e4)
  for(k in 1:1e4){
    model.mse.output[k] <- model.mse()
  }})
##    user  system elapsed 
##   14.23    0.00   14.23
# foreach

n.core <- parallel::makeForkCluster(6)
doParallel::registerDoParallel(n.core)
foreach(i = 1:3, .combine = 'c') %dopar% {
  sqrt(i)
}


system.time({
  doParallel::registerDoParallel(n.core)
  foreach(r2) %dopar% for_test(chrom)
})

?foreach
?registerDoParallel



# Stop the clock
proc.time() - ptm

data(Boston)


model.mse <- function(x) {
  id <- sample(1:nrow(Boston), 200, replace = T)
  mod <- lm(medv ~ ., data = Boston[id,])
  mse <- mean((fitted.values(mod) - Boston$medv[id])^2)
  return(mse)
}
x.list <- sapply(1:10000, list)
class(r2) # list



system.time({
  clust <- makeCluster(n.core)
  clusterExport(clust, "Boston")
  a <- parLapply(clust, x.list, model.mse)})



cls <- makeCluster(length(rows))
parLapply(cls, rows, for_test)
stopCluster(cls)


r3 <- vector("list",n.chrom)
names(r3) <- chrom




Keff <- function(r2) {
  m <- nrow(r2)
  if (m > 1) {
    Q <- sqrt(r2)
    Q[upper.tri(Q,diag=T)] <- NA
    rmax <- apply(Q[-1,],1,max,na.rm=T)
    kappa <- sqrt(1-rmax^(-1.31*log10(0.05)))
    return(1+sum(kappa))
  } else {
    return(1)
  }
}

n.core=6
if (n.core > 1) {
  cl <- makeCluster(n.core)
  clusterExport(cl=cl,varlist=NULL)
}


########

set.threshold.1 <- function(data,method="M.eff",level=0.05,n.permute=1,n.core=8) {
  
  stopifnot(inherits(data,"GWASpoly.fitted"))
  traits <- names(data@scores)
  n.trait <- length(traits)
  models <- colnames(data@scores[[1]])
  model2 <- gsub("-ref","",models,fixed=T)
  model2 <- unique(gsub("-alt","",model2,fixed=T))
  n.model <- length(models)
  methods <- c("M.eff","Bonferroni","FDR","permute")
  stopifnot(is.element(method,methods))
  threshold <- matrix(NA,n.trait,n.model)
  colnames(threshold) <- models
  rownames(threshold) <- traits
  
  if (method=="M.eff") {
    chrom <- levels(data@map[,2])
    n.chrom <- length(chrom)
    r2 <- vector("list",n.chrom)
    
    names(r2) <- chrom
    
    for (i in chrom) {
      ix <- which(data@map[,2]==i)
      r2[[i]] <- as(cor(data@geno[,ix])^2,"dspMatrix")
    }
  }
  
  for (i in 1:n.trait) {
    trait <- traits[i]
    if (method=="permute") {
      print(paste("Trait:",trait),quote=F)
      #y <- data@pheno[,trait]
      #ix <- which(!is.na(y))
      max.scores <- matrix(NA,n.permute,n.model)
      colnames(max.scores) <- models
      for (q in 1:n.permute) {
        print(paste("Permutation",q),quote=F)
        data2 <- data
        data2@pheno[,1] <- sample(data@pheno[,1]) #permute id
        data2 <- GWASpoly(data2,models=model2,traits=trait,params=data@params,quiet=T,n.core=n.core)
        for (j in 1:n.model) {max.scores[q,j] <- max(data2@scores[[trait]][,models[j]],na.rm=T)}				
      }
    }
    for (j in 1:n.model) {
      model <-  models[j]
      iv <- which(!is.na(data@scores[[trait]][,model]))
      scores <- as.vector(data@scores[[trait]][iv,model])
      m <- length(scores)
      if (method=="Bonferroni") {threshold[i,j] <- -log10(level/m)}
      level <- 0.05
      if (method=="M.eff") {
        me <- 0
        for (chr in chrom) {
          ix <- data@map[intersect(iv,which(data@map[,2]==chr)),1]
          ix <- as.character(ix)
          class(ix)
          if (length(ix)>1) {
            me <- parLapply(cl, r2, Keff)
#            me <- me + Keff(r2=r2[["Chr1"]][ix,ix],alpha=level)
          } else {
            me <- me + 1
          }
        }
        threshold[i,j] <- -log10(level/me)
      }
      if (method=="FDR") {
        tmp <- cbind(10^(-scores),.qvalue(10^(-scores)))
        tmp <- tmp[order(tmp[,2]),]
        if (tmp[1,2] > level) {
          threshold[i,j] <- -log10(tmp[1,1])*1.2
        } else {
          k <- max(which(tmp[,2] < level))
          threshold[i,j] <- -log10(mean(tmp[k:(k+1),1]))
        }
      }
      if (method=="permute") {
        threshold[i,j] <- sort(max.scores[,model],decreasing=TRUE)[max(floor(level*n.permute),1)]
      }	
    }
  }
  cat("Thresholds\n")
  print(round(threshold,2))
  return(new("GWASpoly.thresh",map=data@map,pheno=data@pheno,fixed=data@fixed,geno=data@geno,ploidy=data@ploidy,K=data@K,scores=data@scores,effects=data@effects,params=data@params,threshold=threshold))
}
