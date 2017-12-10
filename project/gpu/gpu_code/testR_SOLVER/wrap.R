## cuMM(int *nr_A, int *nc_A, int *nc_B, float *A, float *B, float *C, float *X, float *a, float *b)
cuMM <- function(A, B)
{
    if(!is.loaded("cuMM")) {
        dyn.load("cuBLAS_SOLVER_UNIFIED.so")
    }
    C <- matrix(0,nrow=nrow(A),ncol=ncol(A))
    rst <- .C("cuMM",
              as.integer(nrow(A)),
              as.integer(ncol(A)),
              as.integer(ncol(A)),
              as.double(A),
              as.double(B),
	      as.double(C),
              as.double(C),
	      as.double(1.0),as.double(0.0))
    #print(rst)
    F <- matrix(rst[[7]],nrow=nrow(A),byrow=F)
    return(F)
}

