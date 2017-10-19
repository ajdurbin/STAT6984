## load libraries
library(textir)
library(rpart)
library(mda)
library(MASS)
library(randomForest)

## 
## begin command line processing
##

## set the seed
seed <- 0
## specify the number of repetitions
reps <- 5

## read in the command line arguments
## run with: R CMD BATCH '--args seed=0 reps=5' spam_mc.R
args <- commandArgs(TRUE)
if(length(args) > 0) 
    for(i in 1:length(args)) 
        eval(parse(text=args[[i]]))

## print seed
cat("seed is ", seed, "\n", sep="")
set.seed(seed)
cat("reps is ", reps, "\n", sep="")

## 
## begin MC analysis 
##

## get the data
spam <- read.csv("spam.csv")

## function for creating CV folds
cv.folds <- function (n, folds = 10)
    split(sample(1:n), rep(1:folds, length = n))

## allocate space for the predictor
pnull <- matrix(NA, nrow=reps, ncol=nrow(spam))
pfull <- pfwd <- pfwdi <- plda <- pqda <- pnull
pfda <- prp <- pmnlm <- prf <- pnull

## loop over repetitions
for(r in 1:reps) {

  ## generate CV folds
  all.folds <- cv.folds(nrow(spam), folds=5)

  ## loop over folds
  for(i in 1:length(all.folds)) {
    
    ## get the ith fold, and print progress
    o <- all.folds[[i]]
    cat("(r,i)=(", r, ",", i, ")\n", sep="")

    ## training and testing set
    test <- spam[o,]
    train <- spam[-o,]

    ## get rid of colums that are all the same
    one <- rep(1, nrow(train))
    zo <- which(sqrt(drop(one %*% (train^2))) == 0)
    if(length(zo) > 0) {
      train <- train[,-zo]; test <- test[,-zo]
    }
    nt <- nrow(train)

    ## defining the universe of variables
    null <- glm(spam ~ 1, data=train, family="binomial")
    pnull[r,o] <- round(predict(null, newdata=test, type="response"))

    ## the full glm
    full <- suppressWarnings(glm(spam ~ ., data=train, 
              family="binomial")) ## will warn
    pfull[r,o] <- round(predict(full, newdata=test, type="response"))

    ## go forward to get the best model greedily
    fwd <- suppressWarnings(step(null, scope=formula(full), 
                            k=log(nt), trace=0))
    pfwd[r,o] <- round(predict(fwd, newdata=test, type="response"))

    ## now try adding in interactions
    fwdi <- suppressWarnings(step(fwd, scope=~.+.^2, k=log(nt), trace=0))
    ## this takes a long time (about an hour on a fast machine)
    pfwdi[r,o] <- round(predict(fwdi, newdata=test, type="response"))

    ## LDA
    spam.lda <- lda(spam~., data=train)
    plda[r,o] <- as.numeric(predict(spam.lda, newdata=test)$class)-1

    ## QDA
    try({  ## necessary because quadratic expansion may not work
      spam.qda <- qda(spam~., data=train);
      pqda[r,o] <- as.numeric(predict(spam.qda, newdata=test)$class)-1
    }, silent=TRUE)

    ## FDA
    spam.fda <- fda(spam~., data=train)
    pfda[r,o] <- as.numeric(predict(spam.fda, newdata=test, type="class"))-1

    ## rpart
    ## otherwise rpart will do regression
    train <- transform(train, spam=as.factor(spam))
    spam.rp <- rpart(spam~., data=train)
    prp[r,o] <- as.numeric(predict(spam.rp, newdata=test, type="class"))-1

    ## random forests
    rf <- randomForest(spam~., data=train)
    prf[r,o] <- as.numeric(predict(rf, newdata=test))-1

    ## save results to a file
    save(pnull, pfull, pfwd, pfwdi, plda, pqda, pfda, prp, prf, 
         file=paste("spam_", seed, ".RData", sep=""))
  }
}
