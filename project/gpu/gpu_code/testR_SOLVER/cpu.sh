Rscript -e "source('wrap.R'); set.seed(1234); N<-5000; A<-matrix(rnorm(N^2,5,1),nrow=N,ncol=N);system.time({A2<-t(A)%*%A;X<-solve(A2)});print(max(A2%*%X-diag(N)));#print(A2);#print(X);"

