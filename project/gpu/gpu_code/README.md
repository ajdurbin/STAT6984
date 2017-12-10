########################
########################
##### GPUs and R #######
########################
########################

This directory contains example code.  Each subdirectory contains a readme with run instructions.

* MetroH_PT.R
	+ This code is for the homework.  Run instructions are provided in the homework text.
* cuBLAS_vs_gpuR
  + This example shows how to use cuBLAS to perform matrix multiplication by filling arrays using cuRAND.  
* profile1_nv_dropin
	+ This example shows how to use the Nvidia nvblas interceptor.
* profile2_solve
	+ Nvidia example code using the cuSOLVE library.
* profile4_batch_CUBLAS
	+ Nvidia example code using the cuBLAS library.
* profile5_memory
	+ R code highlighting the difference in memory access.
* testR_BLAS
	+ R plus cuBLAS code examples highlighting gpuR and cuBLAS..
* testR_SOLVER
	+ R plus cuBLAS plus cuSOLVER example that may come in handy on the homework.  
	+NOTE: one one item to pay attention to, if passing in a vector, ncol(vector) doesn't work...
