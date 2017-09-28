library(quantmod)
?getSymbols

## get.quotes:
##
## downloads the most recent price info for
## a particular ticker over the last 365 days

get.quotes <- function(ticker, crumb, from=(Sys.Date()-365), to=(Sys.Date()), 
                       interval=c("1d", "1wk", "1mo"))
{
  ## define parts of the URL
  base <- "https://query1.finance.yahoo.com/v7/finance/download/"
  
  ## create period
  from <- paste("?period1=", as.numeric(as.POSIXlt(from)), sep="")
  to <- paste("&period2=", as.numeric(as.POSIXlt(to)), sep="")
  
  ## sampling interval and back matter
  interval <- match.arg(interval)
  inter <- paste("&interval=", interval, sep="")
  ## last <- "&ignore=.csv"
  last <- paste("&events=history&crumb=", crumb, sep="")
  
  ## put together the url
  url <- paste(base, ticker, from, to, inter, last, sep="");
  print(url)
  file <- paste("~/Downloads/", ticker, ".csv", sep="")
  if(file.exists(file)) unlink(file)
  
  ## get the file
  utils::browseURL(url)
  while(! file.exists(file)) Sys.sleep(time=1)
  
  ## copy it from the downloads folder
  tmp <- read.csv(file)
  unlink(file)
  
  cbind(symbol=ticker,tmp)
}

## get.multiple.quotes:
##
## calls get.quotes on a vector of character tickers

get.multiple.quotes <- function(tkrs, crumb, from=(Sys.Date()-365),
                                to=(Sys.Date()), interval=c("1d", "1wk", "1mo"))
{
  interval <- match.arg(interval)
  tmp <- NULL
  for (tkr in tkrs) {  
    if (is.null(tmp))
      tmp <- get.quotes(tkr,crumb,from,to,interval)
    else tmp <- rbind(tmp,get.quotes(tkr,crumb,from,to,interval))
  }
  tmp
}

## get the tickers
dow30.tickers <-
  c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX","CSCO", "KO", 
    "DIS", "XOM", "GE", "GS", "HD", "IBM", "INTC", "JNJ",
    "JPM", "MCD", "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", 
    "UTX", "UNH", "VZ", "V", "WMT")
Sys.Date()

## you need to get the crumb from the browser
dow30 <- get.multiple.quotes(dow30.tickers, crumb="LnmO2Uf5KK5")
## try/not in code
dim(dow30)
names(dow30)


dow30[dow30$Symbol == 'MMM', ]
y <- head(dow30[dow30$Symbol == 'MMM', ], -1)
x <- tail(dow30[dow30$Symbol == 'MMM', ], -1)