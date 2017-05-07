#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .Call calls */
extern SEXP trustOptim_quasiTR(SEXP, SEXP, SEXP, SEXP);
extern SEXP trustOptim_sparseTR(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"trustOptim_quasiTR",  (DL_FUNC) &trustOptim_quasiTR,  4},
    {"trustOptim_sparseTR", (DL_FUNC) &trustOptim_sparseTR, 5},
    {NULL, NULL, 0}
};

void R_init_trustOptim(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
