## This file contains three functions to compare computation time for matrix multiplication.
## mm_gpuR.gM: use gpuMatrix in library(gpuR)
## mm_gpuR.vcl: use vclMatrix in library(gpuR)
## mm_cublas: a R wrapper function for the self-implemented cuda programming

mm_gpuR.gM <- function(m, k, n)
{
  A <- matrix(runif(m * k), ncol=k)
  B <- matrix(runif(k * n), ncol=n)
  A <- gpuMatrix(A, type="float") 
  B <- gpuMatrix(B, type="float")
  C <- A %*% B
  
  return(C)
}

mm_gpuR.vcl <- function(m, k, n)
{
  A <- matrix(runif(m * k), ncol=k)
  B <- matrix(runif(k * n), ncol=n)
  A <- vclMatrix(A, type="float")
  B <- vclMatrix(B, type="float")
  C <- A %*% B
  
  return(C)
}

## matrix multiplication in GPU: calling cublas library
mm_cublas <- function(m, k, n)
{  
   ## load shared object
   if(!is.loaded("cublas")){
      dyn.load("src/cublas.so")
   }
   res <- .C("cublas",
             m=as.integer(m),
             k=as.integer(k),
             n=as.integer(n),
             ## pre-allocate memory for C
             C=double(m*n))
   ## since cuda programming is column-major argument, we 
   ## do not need to pass the transposed.
   return(matrix(res$C, nrow=m, ncol=n))
}
