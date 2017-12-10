module load cuda/8.0.61 gcc/5.2.0 openblas R/3.4.1

nvcc -O3 -G -I$CUDA_INC -I$R_INC -L$R_LIB -L$CUDA_LIB -lcudart -lcublas -lcurand --shared -Xcompiler -fPIC -o cuBLAS_MM.so cuBLAS_MM_d.cu
