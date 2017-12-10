module purge
module load cuda/8.0.61 gcc/5.2.0 openblas R/3.4.1 R-gpu/3.4.1 

Rscript -e "suppressMessages(library('gpuR'));N<-c(25,50,500,1000,5000,7500,10000,12500); for(n in seq_along(N)){A<-matrix(1:N[n]*2,nrow=N[n],ncol=N[n]);A<-gpuMatrix(A);time1<-system.time(temp<-t(A)%*%A); cat('nrow=ncol=',N[n],' time ',time1[3],'\n');}"

Rscript -e "suppressMessages(library('gpuR'));N<-c(25,50,500,1000,5000,7500,10000,12500); for(n in seq_along(N)){A<-matrix(1:N[n]*2,nrow=N[n],ncol=N[n]);A<-vclMatrix(A);time1<-system.time(temp<-t(A)%*%A); cat('nrow=ncol=',N[n],' time ',time1[3],'\n');}"

Rscript -e "suppressMessages(library('gpuR'));N<-c(25,50,500,1000,5000,7500,10000,12500); for(n in seq_along(N)){A<-matrix(1:N[n]*2,nrow=N[n],ncol=N[n]);time1<-system.time({A<-vclMatrix(A);temp<-t(A)%*%A});cat('nrow=ncol=',N[n],' time ',time1[3],'\n');}"
