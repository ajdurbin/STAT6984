
# bisection method function -----------------------------------------------

## bisection method function; must have xl <- xr
## and f(xl)*f(xr) < 0;  output is a root of f
## in the interval (xl, xr)
bisection <- function(f, xl, xr, tol=sqrt(.Machine$double.eps),
                      verb=0, maxiter=100)
{
  ## create an output object
  out <- list(f=f, tol=tol)
  class(out) <- "bisection"
  
  ## check inputs
  if(xl > xr) stop("must have xl < xr")
  
  ## setup and check outputs
  fl <- f(xl)
  fr <- f(xr)
  i <- 0
  out$int_prog <- data.frame(xl=xl, xr=xr, fl=fl, fr=fr)
  if(fl == 0) { out$ans <- xl; return(out) }
  else if(fr == 0) { out$ans <- xr; return(out) }
  else if(fl * fr > 0){
    
    if(verb > 0 ) cat("Warning: fl * fr > 0\nIterating to expand interval\n")
    # expand interval until fl * fr > 0 or max iterations reached
    while(fl * fr > 0){
      
      i <- i + 1
      xm <- (xr + xl)/2
      w <- xr - xl
      xl <- xm - w
      xr <- xm + w
      fl <- f(xl)
      fr <- f(xr)
      out$int_prog[i,] <- c(xl, xr, fl, fr)
      
      if(verb > 0)
        cat("i=", i, ", (xl, xr)=(", xl, ", ", xr, ")\n", sep="")
      
      if(i >= maxiter) { stop('Max bracket iterations reached') }
      
    }  
    
  }
    
  out$brckt_iter <- i
  out$root_prog <- data.frame(xl=xl, xr=xr, fl=fl, fr=fr)
  
  ## successively refine xl and xr
  if(verb > 0)
    cat("Iterating to find root\n")
  n <- 1
  while((xr - xl) > tol) {
    xm <- (xl + xr)/2
    fm <- f(xm)
    if(fm == 0) { out$ans <- xm; return(out) }
    else if (fl * fm < 0) {
      xr <- xm; fr <- fm
    } else { xl <- xm; fl <- fm }
    
    ## next iteration
    n <- n + 1
    out$root_prog[n,] <- c(xl, xr, fl, fr)
    if(verb > 0)
      cat("n=", n, ", (xl, xr)=(", xl, ", ", xr, ")\n", sep="")
  }
  
  ## return (approximate) root
  out$ans <- (xl + xr)/2
  return(out)
}



# printing method ---------------------------------------------------------

## now make a printing method
print.bisection <- function(x, ...) {
  
  if(x$brckt_iter > 0){
    cat("Root of:\n")
    print(x$f)
    cat("in (", tail(x$int_prog$xl, n = 1), ", ", tail(x$int_prog$xr, n = 1),") found after ", 
        nrow(x$root_prog), " iterations: ",x$ans, "\n", "to a tolerance of ", 
        x$tol, "\n","after expanding the original interval \n(", x$int_prog$xl[1], 
        ", ", x$int_prog$xr[1], ") to (", tail(x$int_prog$xl, n = 1), ", ", 
        tail(x$int_prog$xr, n = 1),")\nin ", nrow(x$int_prog), " iterations", 
        sep = "")
    
  } else {
    
    cat("Root of:\n")
    print(x$f)
    cat("in (", x$root_prog$xl[1], ", ", x$root_prog$xr[1],
        ") found after ", nrow(x$root_prog), " iterations: ",
        x$ans, "\n", "to a tolerance of ", x$tol, "\n", sep="")
    
  }
  
}


# summary method ----------------------------------------------------------

## and a summary method
summary.bisection <- function(object, ...)
{
  print(object, ...)
  if(object$brckt_iter > 0){
    cat("\nInterval expansion progress is as follows\n")
    print(object$int_prog) 
  }
  cat("\nRoot finding progress is as follows\n")
  print(object$root_prog)
}


# plotting method ---------------------------------------------------------

## and a plotting method; gridlen specifies the grid on which
## the function is plotted for comparison, and after provides
## the ability to zoom in
plot.bisection <- function(object, gridlen=1000, after=NULL, ...)
{
  ## check if there is something to do
  if(nrow(object$root_prog) == 0) {
    cat("nothing to plot\n")
    invisible(NULL)
  }
  
  ## check after
  if(!is.null(after)) {
    if(after < 0 || after >= nrow(object$root_prog))
      stop("after must be in 1:(nrow(objext$prog)-1)")
    object$root_prog <- object$root_prog[after:nrow(object$root_prog),]
  } else after <- 0
  
  ## plots the true function
  xgrid <- seq(object$root_prog$xl[1], object$root_prog$xr[1], length=gridlen)
  xgrid <- sort(union(xgrid, c(object$root_prog$xl, object$root_prog$xr)))
  y <- object$f(xgrid) ## assumes a vectorized function
  plot(xgrid, y, type="l", col="gray", ...)
  
  ## indicates (only) the changes in xl and xr
  n <- nrow(object$root_prog)
  dl <- duplicated(object$root_prog$xl)
  text(object$root_prog$xl[!dl], object$root_prog$fl[!dl], ((1:n)+after)[!dl])
  dr <- duplicated(object$root_prog$xr)
  text(object$root_prog$xr[!dr], object$root_prog$fr[!dr], ((1:n)+after)[!dr], col=2)
  
  ## adds a reference line
  abline(h=0, col=3, lty=3)
}
