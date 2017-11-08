#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <R.h>
# ifdef _OPENMP
#include <omp.h>
# endif

#define dposv dposv_
extern void dposv(char *, int*, int *, double*, int*, 
                  double*, int*, int*);
#define dgemm dgemm_
extern void dgemm(char*, char*,  int*, int*, int*, double*,
                  double*, int*, double*, int*, double*, double*, 
                  int*);
#define dgemv dgemv_
extern void dgemv(char*, int*, int*, double*, double*, int*,
                  double*, int*, double*, double*, int*);
#define dsymv dsymv_
extern void dsymv(char*, int*, double*, double*, int*,
                  double*, int*, double*, double*, int*);

int i_one = 1;
double d_one = 1.0, d_zero = 0.0;
char trans = 'T', notrans = 'N', upper = 'U', nodiag = 'N';


/*
 * new_matrix:
 *
 * create a new n1 x n2 matrix which is allocated like
 * and n1*n2 array, but can be referenced as a 2-d array
 */

double ** new_matrix(unsigned int n1, unsigned int n2)
{
	int i;
	double **m;

	if(n1 == 0 || n2 == 0) return NULL;
	m = (double**) malloc(sizeof(double*) * n1);
	m[0] = (double*) malloc(sizeof(double) * (n1*n2));
	for(i=1; i<n1; i++) m[i] = m[i-1] + n2;

	return m;
}


/*
 * delete_matrix:
 *
 * delete a matrix allocated as above
 */

void delete_matrix(double** m)
{
	if(m == NULL) return;
	free(*m);
	free(m);
}


/*
 * zero:
 *
 * replace matrix with zeros
 */

void zero(double **M, unsigned int n1, unsigned int n2)
{
	unsigned int i, j;
	for(i=0; i<n1; i++) for(j=0; j<n2; j++) M[i][j] = 0;
}

/*
 * id:
 *
 * replace square matrix with identitiy
 */

void id(double **M, unsigned int n)
{
	unsigned int i;
	zero(M, n, n);
	for(i=0; i<n; i++) M[i][i] = 1.0;
}


/*
 * ols:
 *
 * calculate ordinary least squares (OLS) estimate of beta_bat
 * via solve(t(X) %*% X) %*% t(X) %*% Y; uses-preallocated temp
 * memory
 */

void ols(double **X, double *Y, int n, int m, 
		 double *XtY, double **XtX, double **XtXi, 
		 double *beta_hat) 
{
	int info;

	/* XtX = t(X) %*% X */
	dgemm(&notrans,&trans,&m,&m,&n,&d_one,*X,&m,*X,&m,&d_zero,*XtX,&m);

	/* calculate full inverse via Cholesky */
	if(XtXi) {

		/* XtY = t(X) %*% Y */
		dgemv(&notrans,&m,&n,&d_one,*X,&m,Y,&i_one,&d_zero,XtY,&i_one);

		/* XtXi = solve(XtX) */
		id(XtXi, m);	
		dposv(&upper,&m,&m,*XtX,&m,*XtXi,&m,&info);
		/* modifies XtX, replacing with chol(XtX) */

		/* beta_hat = XtXi %*% XtY */
		dsymv(&upper,&m,&d_one,*XtXi,&m,XtY,&i_one,&d_zero,beta_hat,&i_one);

	} else { /* only solve system */

		/* beta_hat = XtY = t(X) %*% Y */
		dgemv(&notrans,&m,&n,&d_one,*X,&m,Y,&i_one,&d_zero,beta_hat,&i_one);

		/* beta_hat = solve(t(X) %*% X, t(X) %*% y) */
		dposv(&upper,&m,&i_one,*XtX,&m,beta_hat,&m,&info);
	}
}


/*
 * ols_R:
 *
 * R interface to ols() -- allocates temporary memory required
 */

void ols_R(double *X_in, double *Y_in, int *n_in, int *m_in, 
	       int *inv_in, double *beta_hat_out)
{
	int i;
	double **X, **XtX, **XtXi;
	double *XtY;

   	/* change a vector representation of X into a array one */
   	X = (double **) malloc(sizeof(double*) * (*n_in));
   	X[0] = X_in;
   	for(i=1; i<*n_in; i++) X[i] = X[i-1] + *m_in;

   	/* temporary space for ols */
	XtY = (double*) malloc(sizeof(double) * (*m_in));
	XtX = new_matrix(*m_in, *m_in); 
    if(*inv_in) {
    	XtY = (double*) malloc(sizeof(double) * (*m_in));
    	XtXi = new_matrix(*m_in, *m_in);
    } else { XtXi = NULL; XtY = NULL; }

   	/* call the C-side subroutine */
   	ols(X, Y_in, *n_in, *m_in, XtY, XtX, XtXi, beta_hat_out);

   	/* clean up */
   	free(X);
	delete_matrix(XtX);
	if(XtXi) delete_matrix(XtXi);
	if(XtY) free(XtY);
}

/*
 * bootols:
 *
 * Bootstrap OLS subroutine, for B bootstrap samples of beta_hat
 */

void bootols(double **X, double *Y, int n, int m, int B, int inv, double **beta_hat) 
{
  
#pramga omp parallel
  {
  
    int b, i, j, bindex;
    double **Xb, **XtX, **XtXi;
    double *XtY, *Yb;
  
    /* temporary space for ols */
    XtX = new_matrix(m, m); 
    if(inv) {
      XtY = (double*) malloc(sizeof(double) * m);
      XtXi = new_matrix(m, m);
    } else { XtY = NULL; XtXi = NULL; }
    //Xb = new_matrix(n, m);
    //Yb = (double*) malloc(sizeof(double) * n);	
    
    /* loop over bootstrap rounds */
    for(b=0; b<B; b++) {/* fill Xb and Yb */
    for(i=0; i<n; i++) {
      bindex = floor(n * unif_rand()); /* R's RNG */
    Yb[i] = Y[bindex];
    for(j=0; j<m; j++) Xb[i][j] = X[bindex][j];
    }
    
    /* call ols on Xb Yb */
    ols(Xb, Yb, n, m, XtY, XtX, XtXi, beta_hat[b]);
    }
    
    /* clean up */
    delete_matrix(Xb);
    free(Yb);
    delete_matrix(XtX);
    if(XtXi) delete_matrix(XtXi);
    if(XtY) free(XtY);
  
  }
	
}



/*
 * bootols_R:
 *
 * bootstrap R interface to ols()
 */

void bootols_R(double *X_in, double *Y_in, int *n_in, int *m_in, 
	           int *B_in, int *inv_in, double *beta_hat_out)
{
	int i;
	double **X, **beta_hat;

   	/* change a vector representation of X into a array one */
   	X = (double **) malloc(sizeof(double*) * (*n_in));
   	X[0] = X_in;
   	for(i=1; i<*n_in; i++) X[i] = X[i-1] + *m_in;

   	/* change a vector representation of beta_hat_out into a array one */
   	beta_hat = (double **) malloc(sizeof(double*) * (*B_in));
   	beta_hat[0] = beta_hat_out;
   	for(i=1; i<*B_in; i++) beta_hat[i] = beta_hat[i-1] + *m_in;

   	/* for R's RNG */
   	GetRNGstate();

   	/* call the C-side subroutine */
   	bootols(X, Y_in, *n_in, *m_in, *B_in, *inv_in, beta_hat);

   	/* for R's RNG */
   	PutRNGstate();

   	/* clean up */
   	free(X);
   	free(beta_hat);
}
