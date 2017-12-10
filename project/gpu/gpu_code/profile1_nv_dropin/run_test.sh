module purge
module load cuda/8.0.61 gcc/5.2.0 openblas R/3.4.1 R-gpu/3.4.1
export NVBLAS_CONFIG_FILE=`pwd`/nvblas.conf

echo "first CPU only"
Rscript mm.R

echo "second, nvblas interceptor method"
env LD_PRELOAD=$CUDA_LIB/libnvblas.so Rscript mm.R

echo "third, full cuBLAS, from R"
