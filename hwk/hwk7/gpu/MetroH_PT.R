### 3 implementations of parallel tempuring
### 1. cpu bound PT, no parallelizations
### 2. cpu bound PT, dopar
### 3. gpu offloading of compute

options(bitmapType = "cairo")

## load libraries
suppressWarnings(suppressMessages(library(doParallel)))
suppressWarnings(suppressMessages(library(OpenCL)))
suppressWarnings(suppressMessages(library(doRNG)))
library(data.table)

## set the defaults
seed <- 12147
compute <- "CPU"

##
## begin command line processing
##
## read in the command line arguments
## run with: Rscript MetroH_PT.R CPU
args <- commandArgs(trailingOnly=TRUE)
if(length(args) < 1 || length(args) > 1){
    cat("Error: wrong number of args \n")
    cat("Expecting: Rscript MetroH_PT.R compute_type \n")
    cat("where compute_type is one of CPU, CPU-parallel, or GPU \n")
    stop()
}
if(!(args[1] %in% c("CPU", "CPU-parallel", "GPU"))){
    cat("Expecting: Rscript MetroH_PT.R compute_type \n")
    cat("where compute_type is one of CPU, CPU-parallel, or GPU \n")
    stop()
}
compute <- args[1]
cat("compute type is ", compute, "\n", sep="")

## Normal MH, adapted from Darren Wilkinson
# chains_parallel_noT_optimum=function(tune=0.1, init=1, iters=1e6){
#     temp_len<-length(temps)
#     x=rep(init,temp_len)
#     xmat=matrix(0,iters,temp_len)
#     for (i in 1:iters) {
#         x1<-(x*x-1)*(x*x-1)
#         can=x+rnorm(temp_len,0,tune)
#         x2<-(can*can-1)*(can*can-1)
#         logA=temps*(x1-x2)
#         accept=(log(runif(temp_len))<logA)
#         x[accept]=can[accept]
#         xmat[i,]=x
#     }
#     colnames(xmat)=paste("gamma=",temps,sep="")
#     xmat
# }
# MH_mat<-chains_parallel_noT_optimum(tune = 0.1,init = 1)

#### CPU bound parallel tempering
chains_PT_optimum=function(chain_links=1e6, swap_prob=0.5, tune=0.1, init=1, 
                           temps=2^seq(0,4,length.out = 16)){
    chain_length=chain_links
    temp_len<-length(temps)
    x=rep(init,temp_len)
    xmat=matrix(0,chain_length,temp_len)
    for (i in 1:chain_length) {
        x1<-(x*x-1)*(x*x-1)
        can=x+rnorm(temp_len,0,tune)
        x2<-(can*can-1)*(can*can-1)
        logA=temps*(x1-x2)
        accept=(log(runif(temp_len))<logA)
        x[accept]=can[accept]
        # now the coupling update
        if(runif(1)<swap_prob){ #propose a swap 
            rev_x<-rev(x)
            x1<-(x*x-1)*(x*x-1)
            x2<-(rev_x*rev_x-1)*(rev_x*rev_x-1)
            logA=sum(temps*x1)-sum(temps*x2)
            if (log(runif(1))<logA){
                x=rev_x
            }
        }
        # end of the coupling update
        xmat[i,]=x
    }
    colnames(xmat)=paste("gamma=",temps,sep="")
    xmat
}

### CPU bound parallel tempering using doPAR
chains_PT_optimum_doPar=function(chain_links=1e6, swap_prob=0.1, tune=0.1, 
                                 temps=2^seq(0,4,length.out = 16), grid_up=1.5, grid_low=-1.5){
    set.seed(123)
    vtemps<-temps
    chain_length=chain_links
    temp_len<-length(vtemps)
    xmat=matrix(0,chain_links,length(temps))
    no_cores <- detectCores() - 1
    cl<-makeCluster(no_cores, type="FORK")
    registerDoParallel(cl)
    inits<-seq(grid_low,grid_up,length.out = no_cores)
    xmat<-foreach(j = iter(inits), .combine = rbind, .inorder = F) %dorng% {
        xmat_temp=matrix(0,chain_length/no_cores,temp_len)
        x=rep(j,temp_len)
        for (i in 1:(chain_length/no_cores)) {
            x1<-(x*x-1)*(x*x-1)
            can=x+rnorm(temp_len,0,tune)
            x2<-(can*can-1)*(can*can-1)
            logA=vtemps*(x1-x2)
            accept=(log(runif(temp_len))<logA)
            x[accept]=can[accept]
            # now the coupling update
            if(runif(1)<swap_prob){ #propose a swap 
                rev_x<-rev(x)
                x1<-(x*x-1)*(x*x-1)
                x2<-(rev_x*rev_x-1)*(rev_x*rev_x-1)
                logA=sum(vtemps*x1)-sum(vtemps*x2)
                if (log(runif(1))<logA){
                    x=rev_x
                }
            }
            # end of the coupling update
            xmat_temp[i,]=x
        }
        return(xmat_temp)
    }
    stopCluster(cl)
    #colnames(xmat)=paste("gamma=",vtemps,sep="")
    xmat
}

### GPU bound parallel tempering using OpenCL
### have to write our own RNG due to OpenCL oddities
### did this on MAC vid GPU, should revisit just passing in the RNGs

code = c("
         __kernel void gpuPT(
         __global float *output,
         const uint count,
         __global float *input, __global float *swap,
         const int chain_links, const float swap_prob, const float tune)
         {
         uint thread_id = get_global_id(0);
         int chainLength = chain_links;
         uint pairedT = thread_id + 1;
         
         private float threadRNG = (input[thread_id]+1)*(input[thread_id]+1);
         private float seed1;
         private float seed2;
         private long M1 = 2147483647;
         private long M2 = 2147483399;
         private long M12 = M1 + M2;
         private long A1 = 40015;
         private long A2 = 40692;
         private float Q1 = M1 / A1 ;
         private float Q2 = M2 / A2 ;
         private float R1 = M1 - A1*floor((float) M1/A1);
         private float R2 = M2 - A2*floor((float) M2/A2);
         
         seed1 = A1*fmod(threadRNG,Q1) - R1*(threadRNG/Q1);
         seed2 = A2*fmod(threadRNG,Q2) - R2*(threadRNG/Q2);
         
         if(pairedT > 15){
            pairedT = 0;
         }
         
         if(thread_id < count){
             private float pvt_scale = sqrt(tune);
             private float x=swap[thread_id];
             swap[thread_id] = x;
             private float x1=0;
             private float x2=0;
             private float can=0;
             private float logA=0;
             private uint temps1 = input[thread_id];
             private uint temps2 = input[thread_id];
             int i =0;
             while(i<chainLength){
                 x1 = x*x;
                 x1 = (x1-1)*(x1-1);
                 seed1 = A1*fmod(seed1,Q1) - R1*(seed1/Q1);
                 seed2 = A2*fmod(seed2,Q2) - R2*(seed2/Q2);
                 threadRNG = seed1 - seed2;
                 can = threadRNG/M12*pvt_scale;
                 can = x + can;
                 x2 = can*can;
                 x2 = (x2-1)*(x2-1);
                 logA = temps1 * (x1 - x2);
                 seed1 = A1*fmod(seed1,Q1) - R1*(seed1/Q1);
                 seed2 = A2*fmod(seed2,Q2) - R2*(seed2/Q2);
                 if(log(fabs(seed2)/M2)<logA){
                     x=can;
                     swap[thread_id] = x;
                 }
                 seed1 = A1*fmod(seed1,Q1) - R1*(seed1/Q1);
                 seed2 = A2*fmod(seed2,Q2) - R2*(seed2/Q2);
                 if(fabs(seed2)/M2 < swap_prob){
                     x1 = x2; 
                     can = swap[pairedT];
                     x2 = can*can;
                     x2 = (x2-1)*(x2-1);
                     logA = temps1 * x1 - temps2 * x2;
                     seed1 = A1*fmod(seed1,Q1) - R1*(seed1/Q1);
                     seed2 = A2*fmod(seed2,Q2) - R2*(seed2/Q2);
                     if(log(fabs(seed2)/M2) < logA){
                         x=can;
                         swap[thread_id] = x;
                     }
                 }
                 if(thread_id < 1){
                    output[i] = swap[thread_id];
                 }
                 i++;
             }
         }
         };")
    

###########################

if(compute=="CPU"){
    time1<-system.time({
        mat1<-chains_PT_optimum(chain_links = 1e6, 
                                swap_prob = .10, tune = 0.1,init = 1)})
    png(paste0(compute,".png"))
    hist(as.numeric(mat1[,ncol(mat1)]),xlim=c(-2,2),xlab="",main=compute, breaks=50)
    dev.off()
    print(time1)
}else if(compute=="CPU-parallel"){
    time1<-system.time({
        mat1<-chains_PT_optimum_doPar(chain_links = 1e6, 
                                      swap_prob = .10, tune = 0.1,grid_up=1.5, grid_low=-1.5, 
                                      temps=2^c(0,4))})
    png(paste0(compute,".png"))
    hist(as.numeric(mat1[,ncol(mat1)]),xlim=c(-2,2),xlab="",main=compute, breaks=50)
    dev.off()
    print(time1)
}else if(compute=="GPU"){
    p = oclPlatforms()       #gets computing platform 
    d = oclDevices(p[[1]],type="gpu")   #sets GPU device we want to use
    gpuPT_tempSwap <- oclSimpleKernel(d[[1]], "gpuPT", code, "best")
    f <- function(x,temps=rev(2^(seq(0,4,length.out = 16))), 
                  chain_links=as.integer(10000),swap_prob=0.1,tune=0.2)
        oclRun(gpuPT_tempSwap,chain_links,temps,x,chain_links,swap_prob,tune)
    
    mat1<-rep(0,10000*10)
    time1<-system.time({
        mat1[1:10000]<-f(c(0:15)/8-1)
        for(i in 1:10){
            mat1[(i*10000+1):((i+1)*10000)]<-f(mat1[9985:10000])
        }
    })
    png(paste0(compute,".png"))
        hist(as.numeric(mat1),xlim=c(-2,2),xlab="",main=compute, breaks=50)
    dev.off()
    print(time1)
}

detach("package:doParallel", unload=TRUE)
#detach("package:parallel", unload=TRUE)
detach("package:doRNG", unload=TRUE)
detach("package:OpenCL", unload=TRUE)


##References

# 1. Darren Wilkinson 
# https://darrenjw.wordpress.com/2013/09/29/parallel-tempering-and-metropolis-coupled-mcmc/
# 2. Michael Lindon 
# https://michaellindon.github.io/lindonslog/programming/openmp/parallel-tempering-algorithm-c/
# 3. Terenin A., Dong S., and D. Draper, GPU-accelerated Gibbs Sampling, 2016, arXiv:1608.04329.
# 4. Mingas G., and C.S. Bouganis, A Custom Precision Based Architecture for Accelerating Parallel 
# Tempering MCMC on FPGAs without Introducing Sampling Error, FCCM, 2012, DOI: 10.1109/FCCM.2012.34.  
# 5. Earl D.J., and M.W. Deem, Parallel tempering: Theory, applications and new perspectives, 
# Phisical Chemistry Chimical Physics, 2005, 7(23) 3910. DOI: 10.1039/b509983h
# 6. Kofke, D.A. On the acceptance probability of replica-exchange Monte Carlo trials, The 
# Journal of Chemical Physics, 2002, 117(15) 6911. DOI: 10.1063/1.1507776

