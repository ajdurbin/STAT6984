/* This file contains an example showing matrix multiplication using cuBLAS, 
   i.e., C(m, n) = alpha * A(m, k) %*% B(k, n) + beta * C(m, n); instead of passing 
   existing values, for A and B, we use cuRAND to generate random numbers to fill 
   the array */

/* The example is adapted from 
   https://solarianprogrammer.com/2012/05/31/matrix-multiplication-cuda-cublas-curand-thrust/ */

/* necessary header files */
#include <R.h>
#include <ctime>
#include <stdio.h>
#include <cstdlib>
#include <curand.h> 
#include <cublas_v2.h>

/* fill the array A (nr_rows_A, nr_cols_A) with random numbers on GPU */
void GPU_fill_rand(float *A, int nr_rows_A, int nr_cols_A)
{
  /* create a pseudo-random number generator */
  curandGenerator_t prng;
  curandCreateGenerator(&prng, CURAND_RNG_PSEUDO_DEFAULT);
  
  /* set the seed for the random number generator using the system clock */
  curandSetPseudoRandomGeneratorSeed(prng, (unsigned long long) clock());
  
  /* fill the array with random numbers on the device */
  curandGenerateUniform(prng, A, nr_rows_A * nr_cols_A);
}

/* Multiply the arrays A and B on GPU and save the result in C */
/* C(m, n) = alpha * A(m, k) %*% B(k, n) + beta * C(m, n) */
void gpu_blas_mmul(float *A, float *B, float *C, int m, int k, int n)
{
  int lda = m, ldb = k, ldc = m;
  const float alf = 1;
  const float bet = 0;
  const float *alpha = &alf;
  const float *beta = &bet;
  
  /* create a handle for cuBLAS */
  cublasHandle_t handle;
  cublasCreate(&handle);
  
  /* do the actual multiplication */
  cublasSgemm(handle, CUBLAS_OP_N, CUBLAS_OP_N, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc);
  
  /* destroy the handle */
  cublasDestroy(handle);
}

extern "C"
/* cublas: this function performs matrix multiplication on GPU and send the result back to CPU */
/* C(m, n) = alpha * A(m, k) %*% B(k, n) + beta * C(m, n) */
void cublas(int *m, int *k, int *n, double *C)
{

  /* allocate 3 arrays on GPU(device) */
  float *d_A, *d_B, *d_C;
  cudaMalloc(&d_A, *m * *k * sizeof(float));
  cudaMalloc(&d_B, *k * *n * sizeof(float));
  cudaMalloc(&d_C, *m * *n * sizeof(float));
  
  /* Copy CPU data (i.e., 'C' in this example) to GPU */
  cudaMemcpy(d_C, C, *m * *n * sizeof(float), cudaMemcpyHostToDevice);
  
  /* fill the arrays A and B on GPU with random numbers */
  GPU_fill_rand(d_A, *m, *k);
  GPU_fill_rand(d_B, *k, *n);
  
  /* matrix multiplication on GPU */
  gpu_blas_mmul(d_A, d_B, d_C, *m, *k, *n);
  
  /* copy the result from device to host memory */
  cudaMemcpy(C, d_C, *m * *n * sizeof(float), cudaMemcpyDeviceToHost);
  
  /* free GPU memory */
  cudaFree(d_A);
  cudaFree(d_B);
  cudaFree(d_C);
  
}
