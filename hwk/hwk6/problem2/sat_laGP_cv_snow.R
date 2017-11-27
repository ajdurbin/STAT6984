library(laGP, lib.loc = lib.Paths())
 
## data file for GRACE satellite
graceHe <- read.table("graceHe.dat", header=TRUE)

## extract predictors and responses
di <- ncol(graceHe)-1   #dimension of input space
X <- graceHe[,1:di]     #input
Y <- graceHe[,di+1]     #response

## scale to (0, 1):
## not scaling Ys (drag coeff) because they have a reasonable range
maxX <- apply(X, 2, max)
minX <- apply(X, 2, min)
for (j in 1:ncol(X)){
     X[,j] <- X[,j] - minX[j]	
     X[,j] <- X[,j]/(maxX[j]-minX[j])
}


## generating cross-validation folds
cv.folds <- function(n, folds = 10){
   split(sample(1:n), rep(1:folds, length = n))
}


## fold.do
##
## code that handles each CV fold, ideally in parallel

fold.do <- function(o, X, Y, nth){
  
  Xtest <- X[o,]   #test input
  Xtrain <- X[-o,] #train input
  Ytest <- Y[o]    #test response
  Ytrain <- Y[-o]  #train response
  
  scores <- rmspe <- rmse <- rep(NA, 14)
  YY <- matrix(NA, ncol=14, nrow=length(o))

  ## for random and blhs
  da.orig <- darg(list(mle=TRUE), Xtrain, samp.size=10000)  
  
  #not estimating nuggets at this time
  
  da.sub <- da.orig
  da.sub$start <- c(rep(1, 5), 0.01, 0.1)

  sub <- sample(1:nrow(Xtrain), 1000, replace=FALSE)  #1
  gpsepi <- newGPsep(Xtrain[sub,], Ytrain[sub], d=da.sub$start, g=1/1000, dK=TRUE) 
  mle <- mleGPsep(gpsepi, tmin=da.sub$min, tmax=10*da.sub$max, ab=da.sub$ab) #length-scale from random sampling
  
  psub <- predGPsep(gpsepi, Xtest, lite=TRUE)
  YY[,1] <- psub$mean
  scores[1] <- mean(-(psub$mean - Ytest)^2/psub$s2 - log(psub$s2))
  rmse[1] <- sqrt(mean((psub$mean - Ytest)^2))
  rmspe[1] <- sqrt(mean((100*(psub$mean - Ytest)/Ytest)^2))
  deleteGPsep(gpsepi)
  
  nn <- aGP(Xtrain, Ytrain, Xtest, d=da.orig, method="nn", omp.threads=nth, verb=0)  #2
  YY[,2] <- nn$mean
  scores[2] <- mean(-(nn$mean - Ytest)^2/nn$var - log(nn$var))
  rmse[2] <- sqrt(mean((nn$mean - Ytest)^2))
  rmspe[2] <- sqrt(mean((100*(nn$mean - Ytest)/Ytest)^2))
  
  alc <- aGP(Xtrain, Ytrain, Xtest, d=da.orig, omp.threads=nth, verb=0)   #3
  YY[,3] <- alc$mean
  scores[3] <- mean(-(alc$mean - Ytest)^2/alc$var - log(alc$var))
  rmse[3] <- sqrt(mean((alc$mean - Ytest)^2))
  rmspe[3] <- sqrt(mean((100*(alc$mean - Ytest)/Ytest)^2))
  
  da <- da.orig
  da$start <- alc$mle$d
  da$start[da$start > 0.9*alc$d$max] <- 0.9*alc$d$max
  da$start[da$start < 1.1*alc$d$min] <- 1.1*alc$d$min
  
  alc2 <- aGP(Xtrain, Ytrain, Xtest, d=da, omp.threads=nth, verb=0)    #4
  YY[,4] <- alc2$mean
  scores[4] <- mean(-(alc2$mean - Ytest)^2/alc2$var - log(alc2$var))
  rmse[4] <- sqrt(mean((alc2$mean - Ytest)^2))
  rmspe[4] <- sqrt(mean((100*(alc2$mean - Ytest)/Ytest)^2))
  
  nnsep <- aGPsep(Xtrain, Ytrain, Xtest, d=da.orig, method="nn", omp.threads=nth, verb=0)  #5
  YY[,5] <- nnsep$mean
  scores[5] <- mean(-(nnsep$mean - Ytest)^2/nnsep$var - log(nnsep$var))
  rmse[5] <- sqrt(mean((nnsep$mean - Ytest)^2))
  rmspe[5] <- sqrt(mean((100*(nnsep$mean - Ytest)/Ytest)^2))
  
  alcsep <- aGPsep(Xtrain, Ytrain, Xtest, d=da.orig, omp.threads=nth, verb=0)  #6
  YY[,6] <- alcsep$mean
  scores[6] <- mean(-(alcsep$mean - Ytest)^2/alcsep$var - log(alcsep$var))
  rmse[6] <- sqrt(mean((alcsep$mean - Ytest)^2))
  rmspe[6] <- sqrt(mean((100*(alcsep$mean - Ytest)/Ytest)^2))
  
  da <- da.orig
  da$start <- as.matrix(alcsep$mle[,1:ncol(Xtrain)])
  da$start[da$start > 0.9*da$max] <- 0.9*da$max
  da$start[da$start < 1.1*da$min] <- 1.1*da$min
  
  alcsep2 <- aGPsep(Xtrain, Ytrain, Xtest, d=da, omp.threads=nth, verb=0)  #7
  YY[,7] <- alcsep2$mean
  scores[7] <- mean(-(alcsep2$mean - Ytest)^2/alcsep2$var - log(alcsep2$var))
  rmse[7] <- sqrt(mean((alcsep2$mean - Ytest)^2))
  rmspe[7] <- sqrt(mean((100*(alcsep2$mean - Ytest)/Ytest)^2))
  
  ## pre-scale from Block LatinHypercube sub-sampling
  theta_hat.med <- blhs.loop(X=Xtrain, y=Ytrain, K=10, m=3, da=da.sub)$that
 
  gpsepi <- newGPsep(Xtrain[sub,], Ytrain[sub], d=theta_hat.med, g=1/1000, dK=TRUE)  #8 
  psub <- predGPsep(gpsepi, Xtest, lite=TRUE)       
  YY[,8] <- psub$mean
  scores[8] <- mean(-(psub$mean - Ytest)^2/psub$s2 - log(psub$s2))
  rmse[8] <- sqrt(mean((psub$mean - Ytest)^2))
  rmspe[8] <- sqrt(mean((100*(psub$mean - Ytest)/Ytest)^2))
  deleteGPsep(gpsepi)

  ## perform scaling
  for (j in 1:ncol(Xtrain)){           
       Xtrain[,j] <- Xtrain[,j]/sqrt(theta_hat.med[j])
       Xtest[,j] <- Xtest[,j]/sqrt(theta_hat.med[j])
  }
  
  da.sb <- darg(list(mle=TRUE), Xtrain, samp.size=10000)
  da.sb$start <- 1
  if(da.sb$max < 2) da.sb$max <- 2
  
  nn.sb <- aGP(Xtrain, Ytrain, Xtest, method="nn", d=da.sb, omp.threads=nth, verb=0) #9
  YY[,9] <- nn.sb$mean
  scores[9] <- mean(-(nn.sb$mean - Ytest)^2/nn.sb$var - log(nn.sb$var))
  rmse[9] <- sqrt(mean((nn.sb$mean - Ytest)^2))
  rmspe[9] <- sqrt(mean((100*(nn.sb$mean - Ytest)/Ytest)^2))
  
  alc.sb <- aGP(Xtrain, Ytrain, Xtest, d=da.sb, omp.threads=nth, verb=0)  #10
  YY[,10] <- alc.sb$mean
  scores[10] <- mean(-(alc.sb$mean - Ytest)^2/alc.sb$var - log(alc.sb$var))
  rmse[10] <- sqrt(mean((alc.sb$mean - Ytest)^2))
  rmspe[10] <- sqrt(mean((100*(alc.sb$mean - Ytest)/Ytest)^2))
  
  da <- da.sb
  da$start <- alc.sb$mle$d
  da$start[da$start > 0.9*da$max] <- 0.9*da$max
  da$start[da$start < 1.1*da$min] <- 1.1*da$min
  
  alc2.sb <- aGP(Xtrain, Ytrain, Xtest, d=da, omp.threads=nth, verb=0)  #11
  YY[,11] <- alc2.sb$mean
  scores[11] <- mean(-(alc2.sb$mean - Ytest)^2/alc2.sb$var - log(alc2.sb$var))
  rmse[11] <- sqrt(mean((alc2.sb$mean - Ytest)^2))
  rmspe[11] <- sqrt(mean((100*(alc2.sb$mean - Ytest)/Ytest)^2))
  
  nnsep.sb <- aGPsep(Xtrain, Ytrain, Xtest, method="nn", d=da.sb, omp.threads=nth, verb=0) #12
  YY[,12] <- nnsep.sb$mean
  scores[12] <- mean(-(nnsep.sb$mean - Ytest)^2/nnsep.sb$var - log(nnsep.sb$var))
  rmse[12] <- sqrt(mean((nnsep.sb$mean - Ytest)^2))
  rmspe[12] <- sqrt(mean((100*(nnsep.sb$mean - Ytest)/Ytest)^2))
  
  alcsep.sb <- aGPsep(Xtrain, Ytrain, Xtest, d=da.sb, omp.threads=nth, verb=0)  #13
  YY[,13] <- alcsep.sb$mean
  scores[13] <- mean(-(alcsep.sb$mean - Ytest)^2/alcsep.sb$var - log(alcsep.sb$var))
  rmse[13] <- sqrt(mean((alcsep.sb$mean - Ytest)^2))
  rmspe[13] <- sqrt(mean((100*(alcsep.sb$mean - Ytest)/Ytest)^2))
  
  da <- da.sb
  da$start <- as.matrix(alcsep.sb$mle[,1:ncol(Xtrain)])
  da$start[da$start > 0.9*da$max] <- 0.9*da$max
  da$start[da$start < 1.1*da$min] <- 1.1*da$min
  
  alcsep2.sb <- aGPsep(Xtrain, Ytrain, Xtest, d=da, omp.threads=nth, verb=0)  #14
  YY[,14] <- alcsep2.sb$mean
  scores[14] <- mean(-(alcsep2.sb$mean - Ytest)^2/alcsep2.sb$var - log(alcsep2.sb$var))
  rmse[14] <- sqrt(mean((alcsep2.sb$mean - Ytest)^2))
  rmspe[14] <- sqrt(mean((100*(alcsep2.sb$mean - Ytest)/Ytest)^2))
  
  return(list(YY=YY, scores=scores, rmse=rmse, rmspe=rmspe))
}


## cv.parallel:
##
## parallel version of CV where each fold is done on a different node
## of a cluster

cv.parallel <- function(cls, X, Y, nth, folds=length(cls)) {

  all.folds <- cv.folds(nrow(X), folds)

  ## compute in parallel
  clusterEvalQ(cls, { library(laGP, lib="~/R/lib") })
  
  all.outs <- clusterApply(cls, all.folds, fold.do, X=X, Y=Y, nth=nth)
  
  ## allocate RMSE and RMSPE
  nas <- rep(NA, folds)
  rmse <- data.frame(sub=nas, nn=nas, alc=nas, alc2=nas,
            nnsep=nas, alcsep=nas, alcsep2=nas,
            subb=nas, nn.sb=nas, alc.sb=nas, alc2.sb=nas,
            nnsep.sb=nas, alcsep.sb=nas, alcsep2.sb=nas)
  scores <- rmspe <- rmse

  ## space for predicted Y-values from all folds
  YY <- data.frame(matrix(NA, ncol=ncol(rmse), nrow=length(Y)))
  names(YY) <- names(rmse)
               
  ## remember which fold the YYs are in
  Fi <- rep(NA, length(Y))

  ## loop over each output and added data into the combined output object
  for (i in 1:length(all.outs)){
    scores[i,] <- all.outs[[i]]$scores
    rmse[i,] <- all.outs[[i]]$rmse
    rmspe[i,] <- all.outs[[i]]$rmspe
    YY[all.folds[[i]],] <- all.outs[[i]]$YY
    Fi[all.folds[[i]]] <- i
  }
  
  ## save to a file rather than returning
  save(YY, Fi, scores, rmse, rmspe, file="sat_graceHe_laGP_cv.RData") #13
}


## cv.serial:
##
## serial version that works the same way as the parallel version
## it is mostly for debugging purposes

cv.serial <- function(X, Y, nth, folds) {

  all.folds <- cv.folds(nrow(X), folds)
  
  ## allocate scores, rmse and rmspe
  nas <- rep(NA, folds)
  rmse <- data.frame(sub=nas, nn=nas, alc=nas, alc2=nas,
            nnsep=nas, alcsep=nas, alcsep2=nas,
            subb=nas, nn.sb=nas, alc.sb=nas, alc2.sb=nas,
            nnsep.sb=nas, alcsep.sb=nas, alcsep2.sb=nas)
  scores <- rmspe <- rmse
  
  ## space for predicted Y-values from all folds
  YY <- data.frame(matrix(NA, ncol=ncol(rmse), nrow=length(Y)))
  names(YY) <- names(rmse)
  
  ## remember which fold the YYs are in
  Fi <- rep(NA, length(Y))
  
  ## loop over each output and added data into the combined output object
  for (i in 1:folds) {
    print(i)
    out <- fold.do(o=all.folds[[i]], X=X, Y=Y, nth=nth)
    scores[i,] <- out$scores
    rmse[i,] <- out$rmse
    rmspe[i,] <- out$rmspe
    YY[all.folds[[i]],] <- out$YY
    Fi[all.folds[[i]]] <- i
    
    ## save results in each iteration rather than returning
    save(YY, Fi, scores, rmse, rmspe, file="sat_graceHe_laGP_cv.RData")
  }
}


## parallel version
library(Rmpi)
library(parallel)
## np <- mpi.universe.size()##-1 
np <- as.numeric(Sys.getenv("PBS_NUM_NODES"))
cl <- makeCluster(np, type="MPI")
print(length(cl))
## nth <- 16
nth <- as.numeric(Sys.getenv("PBS_NUM_PPN"))
cv.parallel(cl, X, Y, nth, np)

## serial version
# cv.serial(X=X, Y=Y, nth=32, folds=5)

stopCluster(cl)
mpi.exit()
