library(parallel)
library(textir)
library(rpart)
library(mda)
library(MASS)
library(randomForest)


# function declarations ---------------------------------------------------


# generate folds
cv.folds <- function (n, folds = 10) {
  split(sample(1:n), rep(1:folds, length = n))
}

# unpack list after parallel
# takes list of lists from spam_mc
# rbinds corresponding matricies together
# returns list of matrices for each method
unpack <- function(par_reps){
  # get names of each matrix
  unique_names <- names(par_reps[[1]])
  # take first group of matrices
  pckg <- par_reps[[1]]
  
  # check if there are more than 1 group of matrices
  if (length(par_reps) > 1) {
    # loop through groups of matrices and rbind them together 
    # based on method
    for(i in 2:length(par_reps)){
      tmp <- par_reps[[i]]
      for (n in unique_names) {
        pckg[[n]] <- rbind(pckg[[n]], tmp[[n]])
      }
    }
  }
  
  # return compact list object
  # each element is matrix of each methods predictions
  # row is repetition, column is prediction
  return(pckg)
  
}

# basically same code from class wrapped in function
# looping slightly different though
# vec is a vector of indicies from splt_reps
# but we don't care what the actual indicies are, just that a given core
# is to do that many total repetitions
# returns list of lists
# sublists are named matricies for each method
spam_mc <- function(vec, spam){
  
  # dynamic storage
  pnull <- matrix(NA, nrow = length(vec), ncol = nrow(spam))
  pfull <- pfwd <- pfwdi <- plda <- pqda <- pnull
  pfda <- prp <- pmnlm <- prf <- pnull
  
  # loop over repetitions
  for(r in 1:length(vec)) {
    
    # generate CV folds
    all.folds <- cv.folds(nrow(spam), folds = 5)
    
    # loop over folds
    for(i in 1:length(all.folds)) {
      
      # get the ith fold, and print progress
      # o <- all.folds[[i]]
      # cat("(r,i) = (", r, ",", i, ")\n", sep = "")
      
      # training and testing set
      test <- spam[o, ]
      train <- spam[-o, ]
      
      # get rid of colums that are all the same
      one <- rep(1, nrow(train))
      zo <- which(sqrt(drop(one %*% (train^2))) == 0)
      if(length(zo) > 0) {
        train <- train[, -zo]; test <- test[, -zo]
      }
      nt <- nrow(train)
      
      # defining the universe of variables
      null <- glm(spam ~ 1, data = train, family = "binomial")
      pnull[r, o] <- round(predict(null, newdata = test, type = "response"))
      
      # the full glm
      full <- suppressWarnings(glm(spam ~ ., data = train,
                                   family = "binomial")) # will warn
      pfull[r,o] <- round(predict(full, newdata = test, type = "response"))

      # go forward to get the best model greedily
      fwd <- suppressWarnings(step(null, scope = formula(full),
                                   k = log(nt), trace = 0))
      pfwd[r,o] <- round(predict(fwd, newdata = test, type = "response"))

      # now try adding in interactions
      fwdi <- suppressWarnings(step(fwd, scope = ~.+.^2, k = log(nt), trace = 0))
      # this takes a long time (about an hour on a fast machine)
      pfwdi[r,o] <- round(predict(fwdi, newdata = test, type = "response"))

      # LDA
      spam.lda <- lda(spam~., data = train)
      plda[r,o] <- as.numeric(predict(spam.lda, newdata = test)$class)-1
      
      # QDA
      try({  # necessary because quadratic expansion may not work
        spam.qda <- qda(spam~., data = train);
        pqda[r,o] <- as.numeric(predict(spam.qda, newdata = test)$class)-1
      }, silent = TRUE)

      # FDA
      spam.fda <- fda(spam~., data = train)
      pfda[r,o] <- as.numeric(predict(spam.fda, newdata = test, type = "class"))-1
      
      # rpart
      # otherwise rpart will do regression
      train <- transform(train, spam = as.factor(spam))
      spam.rp <- rpart(spam~., data = train)
      prp[r,o] <- as.numeric(predict(spam.rp, newdata = test, type = "class"))-1
      
      # random forests
      rf <- randomForest(spam~., data = train)
      prf[r,o] <- as.numeric(predict(rf, newdata = test))-1

      # save results to a file
      save(pnull, pfull, pfwd, pfwdi, plda, pqda, pfda, prp, prf,
           file = paste("spam_", seed, ".RData", sep = ""))
      
    }
  }
  
  # list object of each matrix
  # need to unlist and recombine after
  pckg <- list(pnull = pnull, pfull = pfull, pfwd = pfwd, pfwdi = pfwdi,
               plda = plda, pqda = pqda, pfda = pfda, prp = prp, 
               pmnlm = pmnlm, prf = prf)
  return(pckg)
  
}


# main --------------------------------------------------------------------


spam <- read.csv("spam.csv")

# validate arguments
# R CMD BATCH '--args cores=x' spam_mc.R
args <- commandArgs(TRUE)
eval(parse(text=args[[1]]))
if(cores > detectCores()){
  cat("Warning: specified cores greater than logical cores, using 4 cores")
  cores <- 4
}

# make cluster
cl <- makeCluster(type = "FORK", cores)
total_reps <- 40

# split repetition to parallelize as in class
# splt_reps is a list of indicies for each core
reps <- 1:total_reps
suppressWarnings(splt_reps <- split(reps, 1:cores))

# run in parallel
# returns a list of lists
# first list is core
# sublists are matricies from spam_mc function
par_reps <- clusterApply(cl, splt_reps, spam_mc, spam)
stopCluster(cl)

# unlist and recombine
par_reps <- unpack(par_reps)
