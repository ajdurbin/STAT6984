###########################
private_code <- c(
    "//pragma OPENCL EXTENSION cl_khr_fp64 : enable 
    __kernel void gpuPrivate(__global uint *output, 
    const int chainLength)
    { 
    private uint acc = 0;
    private int total = chainLength;

    for(private int k=0; k<total; k++) {
    acc ++;}
    // Store the result
    output[0] = acc;};")

global_code <- c(
    "//pragma OPENCL EXTENSION cl_khr_fp64 : enable 
    __kernel void gpuGlobal(__global uint *output, 
    const int chainLength)
    { 
    output[0] = 0;
    private int total = chainLength;
    
    for(private int k=0; k<total; k++) {
    output[0] ++;}
    };")

#compile the code we want to use on the GPU device
library(OpenCL)
library(microbenchmark)
p = oclPlatforms()
d = oclDevices(p[[1]])
gpuPrivateR <- oclSimpleKernel(d[[1]], "gpuPrivate", private_code, "best")
time1<-microbenchmark(result <- oclRun(gpuPrivateR, as.integer(1e5)),times = 100)
result[1]
time1
    
gpuGlobalR <- oclSimpleKernel(d[[1]], "gpuGlobal", global_code, "best")
time2<-microbenchmark(result <- oclRun(gpuGlobalR, as.integer(1e5)),times=100)
result[1]
time2
    
    

