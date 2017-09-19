
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
  out$root_prog <- data.frame(xl=xl, xr=xr, fl=fl, fr=fr)
  out$int_prog <- data.frame(xl=xl, xr=xr, fl=fl, fr=fr)
  if(fl == 0) { out$ans <- xl; return(out) }
  else if(fr == 0) { out$ans <- xr; return(out) }
  else if(fl * fr > 0){
    
    if(verb > 0 ) cat("Warning: fl * fr > 0\nIterating to expand interval\n")
    iter <- 0
    while(fl * fr > 0){
      
      iter <- iter + 1
      xm <- (xr + xl)/2
      w <- xr - xl
      xl <- xm - w
      xr <- xm + w
      fl <- f(xl)
      fr <- f(xr)
      out$int_prog[iter,] <- c(xl, xr, fl, fr)
      
      if(verb > 0)
        cat("iter=", iter, ", (xl, xr)=(", xl, ", ", xr, ")\n", sep="")
      
      if(iter >= maxiter) { stop('Max bracket iterations reached') }
      
    }  
    
    out$brckt_iter <- iter
    
  }
    
  
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
    cat("Took ", x$brckt_iter," iterations to expand interval for root\n")
    cat("Root of:\n")
    print(x$f)
    cat("in (", x$int_prog$xl[-1], ", ", x$int_prog$xr[-1],
        ") found after ", nrow(x$root_prog), " iterations: ",
        x$ans, "\n", "to a tolerance of ", x$tol, "\n", sep="") 
    
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
  cat("\nInterval progress is as follows\n")
  print(object$int_prog)
  cat("\nRoot progress is as follows\n")
  print(object$prog_root)
}


# plotting method ---------------------------------------------------------

## and a plotting method; gridlen specifies the grid on which
## the function is plotted for comparison, and after provides
## the ability to zoom in
plot.bisection <- function(object, gridlen=1000, after=NULL, ...)
{
  ## check if there is something to do
  if(nrow(object$prog) == 0) {
    cat("nothing to plot\n")
    invisible(NULL)
  }
  
  ## check after
  if(!is.null(after)) {
    if(after < 0 || after >= nrow(object$prog))
      stop("after must be in 1:(nrow(objext$prog)-1)")
    object$prog <- object$prog[after:nrow(object$prog),]
  } else after <- 0
  
  ## plots the true function
  xgrid <- seq(object$prog$xl[1], object$prog$xr[1], length=gridlen)
  xgrid <- sort(union(xgrid, c(object$prog$xl, object$prog$xr)))
  y <- object$f(xgrid) ## assumes a vectorized function
  plot(xgrid, y, type="l", col="gray", ...)
  
  ## indicates (only) the changes in xl and xr
  n <- nrow(object$prog)
  dl <- duplicated(object$prog$xl)
  text(object$prog$xl[!dl], object$prog$fl[!dl], ((1:n)+after)[!dl])
  dr <- duplicated(object$prog$xr)
  text(object$prog$xr[!dr], object$prog$fr[!dr], ((1:n)+after)[!dr], col=2)
  
  ## adds a reference line
  abline(h=0, col=3, lty=3)
}
