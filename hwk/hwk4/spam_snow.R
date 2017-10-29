## load libraries
library(textir)
library(rpart)
library(mda)
library(MASS)
library(randomForest)
library(parallel)

seed <- 0
reps <- 5
spam <- read.csv("spam.csv")


# storage
pnull <- matrix(NA, nrow=reps, ncol=nrow(spam))
pfull <- pfwd <- pfwdi <- plda <- pqda <- pnull
pfda <- prp <- pmnlm <- prf <- pnull

num_cores <- detectCores() - 1

# split up the repetitions into groups for each core to do
obj <- suppressWarnings(split(1:reps, 1:num_cores))

# write function to take this obj
# label is what core is doing it, vector is what rows it should be doing
# but this is serial program and wont do what i need

# want some sort of parlapply statement and to just collect the outputs after


cl <- makeCluster(no_cores, type = "FORK")
clusterExport(cl = cl, ls())
# function etc
# collect etc
stopCluster(cl)
