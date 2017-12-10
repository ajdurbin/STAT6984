#include <R.h>
#include <stdio.h>
#include <cstdlib>
#include <curand.h>
#include <cublas_v2.h>
/* This function is written for R cuda matrix multiply.
going to use the cublasDgemm
remember, cublasDgemm is really prepping for a*(op)A %*% (op)B + b*C
NOT USED macro
#define IDX2C(i,j,ld) (((j)*(ld))+(i))
*/

extern "C"
void cuMM(int *nr_A, int *nc_A, int *nc_B, double *A, double *B, double *C, double *a, double *b)
{
    // Set up variables
    const double alpha = (double) *a;
    const double beta = (double) *b;
   
    // Create a handle for CUBLAS
    cublasHandle_t handle;
    cublasCreate(&handle);
     
    // Allocate 3 arrays on GPU
    double *d_A, *d_B, *d_C;
    cudaMalloc(&d_A,*nr_A * *nc_A * sizeof(double));
    cudaMalloc(&d_B,*nc_A * *nc_B * sizeof(double));
    cudaMalloc(&d_C,*nr_A * *nc_B * sizeof(double));

    // Copy CPU data to GPU (could also use Unified Memory, beyond todays scope)
    cudaMemcpy(d_A, A, *nr_A * *nc_A * sizeof(double), cudaMemcpyHostToDevice);
    cudaMemcpy(d_B, B, *nc_A * *nc_B * sizeof(double), cudaMemcpyHostToDevice);
    cudaMemcpy(d_C, C, *nr_A * *nc_B * sizeof(double), cudaMemcpyHostToDevice);

    // Multiply A and B on GPU
    cublasDgemm(handle, CUBLAS_OP_N, CUBLAS_OP_N, *nr_A, *nc_B, *nc_A, &alpha, d_A, *nr_A, d_B, *nc_A, &beta, d_C, *nr_A);
    // Copy the data back to CPU
    cudaMemcpy(C,d_C,*nr_A * *nc_B * sizeof(double),cudaMemcpyDeviceToHost);
    
    //Free GPU memory
    cudaFree(d_A);
    cudaFree(d_B);
    cudaFree(d_C);  

    //return 0;
}
