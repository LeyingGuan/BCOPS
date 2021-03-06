// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// conformalscore
NumericVector conformalscore(NumericVector ste, NumericVector s);
RcppExport SEXP _bcops_conformalscore(SEXP steSEXP, SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type ste(steSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(conformalscore(ste, s));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_bcops_conformalscore", (DL_FUNC) &_bcops_conformalscore, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_bcops(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
