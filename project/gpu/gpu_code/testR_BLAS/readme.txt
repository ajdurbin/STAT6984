to run:
load modules:
module load cuda/8.0.61 gcc/5.2.0 openblas R/3.4.1

bash cpu.sh
bash gpu.sh
etc

to run all:

module load cuda/8.0.61 gcc/5.2.0 openblas R/3.4.1
bash cpu.sh && bash gpuR.sh && bash gpu_cuBLAS.sh && bash gpu_cuBLAS_unwrapped.sh

OR annotated a little:

echo “CPU” && bash cpu.sh && echo “gpuR 1. gpuMatrix 2. vclMatrix - no mem transfer 3. vclMatrix with mem transfer” && bash gpuR.sh && echo “bash R-cuBLAS passing matricies” && bash gpu_cuBLAS.sh && echo “unrolling R memory transfers” && bash gpu_cuBLAS_unwrapped.sh
