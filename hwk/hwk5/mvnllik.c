#include <R_ext/Utils.h>
#include <R.h>
#include <Rmath.h>

#define SDEPS sqrt(DOUBLE_EPS)

int i_one = 1;
double d_one = 1.0, d_zero = 0.0;
char trans = 'T', notrans = 'N', upper = 'U', nodiag = 'N';

#define dposv dposv_
extern void dposv(char *, int*, int *, double*, int*,
                  double*, int*, int*);
#define dsymv dsymv_
extern void dsymv(char*, int*, double*, double*, int*,
                  double*, int*, double*, double*, int*);

#define ddot ddot_
extern double ddot(int*, double*, int*, double*, int*);


/*
 * zero:
 *
 * replace matrix with zeros
 */

void zero(double **M, int n1, int n2)
{
	int i, j;
	for(i=0; i<n1; i++) for(j=0; j<n2; j++) M[i][j] = 0;
}


/*
 * id:
 *
 * replace square matrix with identitiy
 */

void id(double **M, int n)
{
	int i;
	zero(M, n, n);
	for(i=0; i<n; i++) M[i][i] = 1.0;
}


/* 
 * invdet:
 *
 * economical calculation of inverse and determinant via 
 * Cholesky decomposition; M is replaced by chol(M)
 */

double invdet(int m, double **M, double **Mi)
{
  	int info;
    int i;
    double ldet;

	id(Mi, m);

    dposv(&upper,&m,&m,*M,&m,*Mi,&m,&info);

    ldet = 0;
    for(i=0; i<m; i++) ldet += log(M[i][i]);
    ldet = 2 * ldet;

	return(ldet);
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
 * new_matrix:
 *
 * create a new n1 x n2 matrix which is allocated like
 * and n1*n2 array, but can be referenced as a 2-d array
 */

double ** new_matrix(int n1, int n2)
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
 * loglik:
 *
 * calculates log likelihood for a multivariate normal distribution over 
 * a vector of theta values of length tlen used to define the covariance
 * structure; if D is m x m, then Y should be n x m.
 */

void logliks(int n, int m, double **Y, double **D, double *theta, 
	int tlen, int verb, double *llik)
{
	double **K, **Ki;
	double *KiY;
	int i, j, t;
	double ldet, qf;

	/* create space */
	K = new_matrix(m, m);
	Ki = new_matrix(m, m);
	KiY = (double*) malloc(sizeof(double) *m);

	/* loop over thetas */
	for(t=0; t<tlen; t++) {

		/* build covariance matrix */
		for(i=0; i<m; i++) {
			K[i][i] = 1.0 + SDEPS;
			for(j=i+1; j<m; j++)
				K[i][j] = K[j][i] = exp(0.0-D[i][j]/theta[t]);
		}

		/* calculate inverse and determinant*/
		ldet = invdet(m, K, Ki);

		/* initialize log likelihood calculation */
		llik[t] =  0.0 - n*(m*M_LN_SQRT_2PI + 0.5*ldet);

		/* calculate quadratic form */
		qf = 0.0;
		for(i=0; i<n; i++) {
			dsymv(&upper,&m,&d_one,*Ki,&m,Y[i],&i_one,&d_zero,KiY,&i_one);
			qf += ddot(&m,KiY,&i_one,Y[i],&i_one);
		}

		/* finish log likelihood calculation */
		llik[t] -= 0.5*qf;

		/* progress meter */
		if(verb > 0 && (t+1) % verb == 0) 
			printf("t=%d, ll=%g\n", t+1, llik[t]);
	}

	/* clean up */
	delete_matrix(K);
	delete_matrix(Ki);
	free(KiY);
}

/* C interface for logliks */
void logliks_R(int *n_in, int *m_in, double *Y_in, double *D_in,
    double *theta_in, int *tlen_in, int *verb_in, double *as_out){

    unsigned int i;
    unsigned int j;

    /* convert Y,D back to matrices */
    double **Y;
    double **D;
    D = (int **) malloc(sizeof(int*) * (*m_in));
    D[0] = D_in;
    for(i = 1; i < *m_in; i++) D[i] = D[i - 1] + *m_in;
    Y = (int **) malloc(sizeof(int*) * (*n_in));
    Y[0] = Y_in;
    for(j = 1; j < *n_in; j++) Y[i] = Y[i - 1] + *n_in;

    /* call logliks */
    logliks(*n_in, m_in, Y, D, theta_in, *tlen_in, *verb_in, as_out);

    /* free memory back */
    free(D);
    free(Y);

}
