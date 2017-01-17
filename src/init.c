#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

int assign(int *xrows, int *xcols, double *x, int *ncenters,
           double *centers, int *cluster, int *clustersize,
           int *dist);
int count(int *xrows, int *xcols, int *x, int *d);
int hardcl(int *xrows, int *xcols, double *x, int *ncenters,
	   double *centers, int *cluster, int *itermax, int *iter, 
	   int *clustersize, int *verbose, int *dist,int *methrate,
	   double *par);
int kmeans(int *xrows, int *xcols, double *x, int *ncenters,
	   double *centers, int *cluster,
           int *itermax, int *iter, int *changes,
           int *clustersize, int *verbose, int *dist);
int neuralgas(int *xrows, int *xcols, double *x, int *ncenters,
	      double *centers, int *cluster,int *itermax, int *iter,
	      int *clustersize, int *verbose, int *dist,double *par);

static const R_CMethodDef CEntries[] = {
    {"assign", (DL_FUNC) &assign, 8},
    {"count", (DL_FUNC) &count, 4},
    {"hardcl", (DL_FUNC) &hardcl, 13},
    {"kmeans", (DL_FUNC) &kmeans, 12},
    {"neuralgas", (DL_FUNC) &neuralgas, 12},
    {NULL, NULL, 0}
};

void R_init_cclust(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
