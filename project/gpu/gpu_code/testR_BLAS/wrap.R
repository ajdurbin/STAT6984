## cuMM(int *nr_A, int *nc_A, int *nc_B, float *A, float *B, float *C, float *a, float *b)
cuMM <- function(A, B)
{
    if(!is.loaded("cuMM")) {
        dyn.load("cuBLAS_MM.so")
    }
    C <- matrix(0,nrow=nrow(A),ncol=ncol(B))
    rst <- .C("cuMM",
              as.integer(nrow(A)),
              as.integer(ncol(A)),
              as.integer(ncol(B)),
              as.double(A),
              as.double(B),
	      as.double(C),
	      as.double(1.0),as.double(0.0))
    #print(rst)
    C <- matrix(rst[[6]],nrow=nrow(A),ncol=ncol(B))
    return(C)
}

