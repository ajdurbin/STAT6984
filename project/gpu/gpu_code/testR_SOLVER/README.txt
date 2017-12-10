This directory contains code to compute solve(X'X) in R via CPU or GPU.  On the GPU, the code is setup to use LU decomposition.

Commented code is included to switch to Cholesky decomposition, some modificiation will be needed.

To run:

module load cuda/8.0.61 gcc/5.2.0 openblas R/3.4.1
bash cpu.sh
bash gpu.sh


