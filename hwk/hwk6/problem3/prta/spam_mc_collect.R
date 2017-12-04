## in case its not already loaded
spam <- read.csv("spam.csv")

## find the spam saved outputs in the CWD
files <- list.files("./", pattern="spam_[0-9]*.RData")

## empty initialization
pfulls <- pfwds <- pfwdis <- pldas <- pqdas <- NULL
pnulls <- pfdas <- prps <- pmnlms <- prfs <- pfulls 

## loop over found RData files
for (i in 1:length(files))
{
  ## read file i and print to screen
  f <- files[i]
  load(f)
  cat(" ", f, sep="")

  ## bind files
  pnulls <- rbind(pnulls, pnull)
  pfulls <- rbind(pfulls, pfull)
  pfwds <- rbind(pfwds, pfwd)
  pfwdis <- rbind(pfwdis, pfwdi)
  pldas <- rbind(pldas, plda)
  pqdas <- rbind(pqdas, pqda)
  pfdas <- rbind(pfdas, pfda)
  prps <- rbind(prps, prp)
  prfs <- rbind(prfs, prf)
}

cat("\n")

hit <- function(x, y=spam$spam)
  mean(x == y, na.rm=TRUE)
df <- data.frame(null=apply(pnulls, 1, hit),
                 full=apply(pfulls, 1, hit),
                 fwd=apply(pfwds, 1, hit),
                 fwdi=apply(pfwdis, 1, hit),
                 lda=apply(pldas, 1, hit),
                 qda=apply(pqdas, 1, hit),
                 fda=apply(pfdas, 1, hit),
                 rp=apply(prps, 1, hit),
                 rf=apply(prfs, 1, hit))

## show the data
boxplot(df[,-1])

## order by averages and augment table with 
## paired t-test
dfmean <- apply(df, 2, mean)
o <- order(dfmean, decreasing=TRUE)
tt <- rep(NA, length(dfmean))
for(i in 1:(length(o)-1)) {
  tto <- t.test(df[,o[i]], df[,o[i+1]], alternative="greater", paired=TRUE)
  tt[o[i]] <- tto$p.value
}

## display ordered results with t-test info
cbind(data.frame(dfmean), data.frame(tt))[o,]
