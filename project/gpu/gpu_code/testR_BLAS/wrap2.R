## cuMM(int *nr_A, int *nc_A, int *nc_B, float *A, float *B, float *C, float *a, float *b)
    if(!is.loaded("cuMM")) {
        dyn.load("cuBLAS_MM.so")
    }
    N<-c(25,50,500,1000,5000,7500,10000,12500)
    for(n in seq_along(N)){
       A<-matrix(1:N[n]*2,nrow=N[n],ncol=N[n])
       B<-A
       C <- matrix(0,nrow=nrow(A),ncol=ncol(B))
       time1<-system.time({

    rst <- .C("cuMM",
              as.integer(nrow(A)),
              as.integer(ncol(A)),
              as.integer(ncol(B)),
              as.double(A),
              as.double(B),
              as.double(C),
              as.double(1.0),as.double(0.0))
       })
       cat('nrow=ncol=',N[n],' time ',time1[3],'\n')
}
