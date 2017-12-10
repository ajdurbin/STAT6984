#include <R.h>
#include <stdio.h>
#include <cstdlib>
#include <curand.h>
#include <cublas_v2.h>
#include <cusolverDn.h>
#include <cuda_runtime.h>
/* This function is written for R cuda matrix multiply followed by solve.
going to use the cublasDgemm
remember, cublasDgemm is really prepping for a*(op)A %*% (op)B + b*C
NOT USED macro
#define IDX2C(i,j,ld) (((j)*(ld))+(i))
*/

extern "C"
void cuMM(int *nr_A, int *nc_A, int *nc_B, double *A, double *B, double *C, double *X, double *a, double *b)
{
    // Set up variables
    const double alpha = (double) *a;
    const double beta = (double) *b;
    int *d_Ipiv = NULL; /* pivoting sequence */ 
    int lwork = 0; /* size of workspace  */
    double *d_work = NULL; /* device workspace for getrf */
    int *d_info;

    // Create a handle for CUBLAS
    cublasHandle_t cublasH;
    cublasCreate(&cublasH);
    cusolverDnHandle_t cusolverH;
    cusolverDnCreate(&cusolverH);

    //printf("%d\n",*nc_B);
    // Allocate 4 arrays on GPU
    double *d_A, *d_B, *d_C;
    cudaMalloc(&d_A,*nr_A * *nc_A * sizeof(double));
    cudaMalloc(&d_B,*nc_A * *nc_B * sizeof(double));
    cudaMalloc(&d_C,*nr_A * *nc_A * sizeof(double));
    cudaMalloc((void**)&d_Ipiv, sizeof(int) * *nr_A);
    cudaMalloc((void**)&d_info, sizeof(int));

    // for solver, need to set aside some memory space and put it on device
    cusolverDnDgetrf_bufferSize(cusolverH, *nr_A, *nc_A, d_A, *nr_A, &lwork);
    //cusolverDnDpotrf_bufferSize(cusolverH, CUBLAS_FILL_MODE_LOWER, *nr_A, d_A, *nr_A, &lwork);
    cudaMalloc((void**)&d_work, sizeof(double)*lwork);

    // Copy CPU data to GPU (could also use Unified Memory, beyond todays scope)
    cudaMemcpy(d_A, A, *nr_A * *nc_A * sizeof(double), cudaMemcpyHostToDevice);
    cudaMemcpy(d_B, B, *nr_A * *nc_B * sizeof(double), cudaMemcpyHostToDevice);
    cudaMemcpy(d_C, C, *nr_A * *nc_A * sizeof(double), cudaMemcpyHostToDevice);

    // Compute (A'A)=C
    cublasDgemm(cublasH, CUBLAS_OP_T, CUBLAS_OP_N, *nr_A, *nc_A, *nc_A, &alpha, d_A, *nr_A, d_A, *nc_A, &beta, d_C, *nr_A);
    // get C back, it gets over written when solving    
    cudaMemcpy(C,d_C,*nr_A * *nc_A * sizeof(double),cudaMemcpyDeviceToHost);

    // solve for X in CX=B where B is identity matrix
    cusolverDnDgetrf(cusolverH, *nr_A, *nc_A, d_C, *nr_A, d_work, d_Ipiv, d_info);
    cusolverDnDgetrs(cusolverH, CUBLAS_OP_N, *nr_A, *nc_B, d_C, *nr_A, d_Ipiv, d_B, *nc_B, d_info);
    //cusolverDnDpotrf(cusolverH, CUBLAS_FILL_MODE_LOWER, *nr_A, d_C, *nr_A, d_work, lwork, d_info);

    // Copy the X back to CPU (note that it is in d_B because solve overwrites it
    cudaMemcpy(X,d_B,*nr_A * *nc_B * sizeof(double),cudaMemcpyDeviceToHost);
    
    //Free GPU memory
    cudaFree(d_A);
    cudaFree(d_B);
    cudaFree(d_C);  
    if (cublasH) cublasDestroy(cublasH);
    if (cusolverH) cusolverDnDestroy(cusolverH);
    cudaDeviceReset();
    //return 0;
}
