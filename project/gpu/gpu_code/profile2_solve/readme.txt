Nvidia Sample: cuSolverDn_LinearSolver
Minimum spec: SM 2.0

A CUDA Sample that demonstrates cuSolverDN's LU, QR and Cholesky factorization.

Key concepts:
Linear Algebra
CUSOLVER Library

Load cuda/8.0.61
How to use
 *      ./cuSolverDn_LinearSolver                     // Default: cholesky
 *     ./cuSolverDn_LinearSolver -R=chol -filefile>   // cholesky factorization
 *     ./cuSolverDn_LinearSolver -R=lu -file<file>     // LU with partial pivoting
 *     ./cuSolverDn_LinearSolver -R=qr -file<file>     // QR factorization


