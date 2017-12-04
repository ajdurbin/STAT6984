## specify number of threads
seed <- 2
## specify the number of repetitions
reps <- 5
## specify number of CV folds
folds <- 5

## read in the command line arguments
## run with: R CMD BATCH '--args seed=0 reps=5' spam_mc.R
args <- commandArgs(TRUE)
if(length(args) > 0) 
    for(i in 1:length(args)) 
        eval(parse(text=args[[i]]))

## arguments
cat("nth is ", nth, "\n", sep="")
cat("folds is ", folds, "\n", sep="")
cat("reps is ", reps, "\n", sep="")

## read in the data
spam <- read.csv("spam.csv")

## create cross validation folds
cv.folds <- function (n, folds = 10)
    split(sample(1:n), rep(1:folds, length = n))


## spamfold:
##
## running a bake-off with a particular fold (i.e., a set
## of indices into the spam data);  this is a useful subroutine
## for running on a cluster/in parallel with snow

spamfold <- function(fold, spam)
{
    library(rpart, lib.loc = "~/R/lib")
    library(mda, lib.loc = "~/R/lib")
    library(MASS, lib.loc = "~/R/lib")
    library(randomForest, lib.loc = "~/R/lib")

    ## training and testing set
  	test <- spam[fold,]
    train <- spam[-fold,]

    ## get rid of colums that are all the same
    one <- rep(1, nrow(train))
    zo <- which(sqrt(drop(one %*% (train^2))) == 0)
    if(length(zo) > 0) {
      train <- train[,-zo]; test <- test[,-zo]
    }
    nt <- nrow(train)

    ## defining the universe of variables
    null <- glm(spam ~ 1, data=train, family="binomial")
    pnull <- round(predict(null, newdata=test, type="response"))
    hnull <- pnull == test$spam

    ## the full glm
    full <- suppressWarnings(glm(spam ~ ., data=train, 
              family="binomial")) ## will warn
    pfull <- round(predict(full, newdata=test, type="response"))
    hfull <- pfull == test$spam

    ## go forward to get the best model greedily
    fwd <- suppressWarnings(step(null, scope=formula(full), 
                            k=log(nt), trace=0))
    pfwd <- round(predict(fwd, newdata=test, type="response"))
    hfwd <- pfwd == test$spam

    ## now try adding in interactions
    fwdi <- suppressWarnings(step(fwd, scope=~.+.^2, k=log(nt), trace=0))
    ## this takes a long time (about an hour on a fast machine)
    pfwdi <- round(predict(fwdi, newdata=test, type="response"))
    hfwdi <- pfwdi == test$spam

    ## LDA
    spam.lda <- lda(spam~., data=train)
    plda <- as.numeric(predict(spam.lda, newdata=test)$class)-1
    hlda <- plda == test$spam

    ## QDA
    hqda <- rep(NA, nrow(test))
    try({  ## necessary because quadratic expansion may not work
      spam.qda <- qda(spam~., data=train);
      pqda <- as.numeric(predict(spam.qda, newdata=test)$class)-1
      hqda <- pqda == test$spam
    }, silent=TRUE)

    ## FDA
    spam.fda <- fda(spam~., data=train)
    pfda <- as.numeric(predict(spam.fda, newdata=test, type="class"))-1
    hfda <- pfda == test$spam

    ## rpart
    ## otherwise rpart will do regression
    train <- transform(train, spam=as.factor(spam))
    spam.rp <- rpart(spam~., data=train)
    prp <- as.numeric(predict(spam.rp, newdata=test, type="class"))-1
    hrp <- prp == test$spam

    ## random forests
    rf <- randomForest(spam~., data=train)
    prf <- as.numeric(predict(rf, newdata=test))-1
    hrf <- prf == test$spam

    ## return predictions
    return(data.frame(null=hnull, full=hfull, fwd=hfwd, fwdi=hfwdi,
		lda=hlda, qda=hqda, fda=hfda, rp=hrp, rf=hrf))
}


## spamcv.snow
##
## a master snow function that distributes folds and repetitions
## out to a cluster whose node info is in the cls input

spamcv.snow <- function(cls, spam, folds=5, reps=2)
{
    ## create folds for all reps
    all.folds <- list()
    for(r in 1:reps) 
      all.folds <- c(all.folds, cv.folds(nrow(spam), folds))

    ## distributed computation for each fold
    hits <- clusterApply(cls, all.folds, spamfold, spam)	

    ## allocate space for hitrate calculation
    hitrate <- data.frame(matrix(NA, nrow=reps, ncol=ncol(hits[[1]])))
    colnames(hitrate) <- colnames(hits[[1]])

    ## loop over reps and folds
    i <- 1
    for(r in 1:reps) { ## reps
    	dfhit <- matrix(NA, nrow=nrow(spam), ncol=ncol(hits[[1]]))
    	for(j in 1:folds) { ## folds
	   dfhit[all.folds[[i]],] <- as.matrix(hits[[i]])
	   i <- i + 1
    	}
    	## aggretate
    	hitrate[r,] <- apply(dfhit, 2, mean)
    }

    ## return the collected hitrates
    return(hitrate)
}


library(Rmpi, lib.loc = "~/R/lib")
library(parallel, lib.loc = "~/R/lib")
np <- as.numeric(Sys.getenv("PBS_NUM_NODES"))
cl <- makeCluster(np, type = "MPI")
nth <- as.numeric(Sys.getenv("PBS_NUM_PPN")
hitrate <- spamcv.snow(cl, spam)
stopCluster(cl)
mpi.exit()

save.image("spam_snow.RData")
