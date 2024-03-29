// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cppARorder
NumericVector cppARorder(NumericMatrix mat);
RcppExport SEXP _OrderedHeatMapAnalysis_cppARorder(SEXP matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    rcpp_result_gen = Rcpp::wrap(cppARorder(mat));
    return rcpp_result_gen;
END_RCPP
}
// cppcorrNeighbor
NumericVector cppcorrNeighbor(NumericMatrix mat);
RcppExport SEXP _OrderedHeatMapAnalysis_cppcorrNeighbor(SEXP matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    rcpp_result_gen = Rcpp::wrap(cppcorrNeighbor(mat));
    return rcpp_result_gen;
END_RCPP
}
// cth_numgroup
NumericVector cth_numgroup(NumericVector cvo);
RcppExport SEXP _OrderedHeatMapAnalysis_cth_numgroup(SEXP cvoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type cvo(cvoSEXP);
    rcpp_result_gen = Rcpp::wrap(cth_numgroup(cvo));
    return rcpp_result_gen;
END_RCPP
}
// cth_numgroupZeroAlone
NumericVector cth_numgroupZeroAlone(NumericVector cvo);
RcppExport SEXP _OrderedHeatMapAnalysis_cth_numgroupZeroAlone(SEXP cvoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type cvo(cvoSEXP);
    rcpp_result_gen = Rcpp::wrap(cth_numgroupZeroAlone(cvo));
    return rcpp_result_gen;
END_RCPP
}
// cpprunstest
NumericVector cpprunstest(NumericMatrix mat, std::string alternative);
RcppExport SEXP _OrderedHeatMapAnalysis_cpprunstest(SEXP matSEXP, SEXP alternativeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< std::string >::type alternative(alternativeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpprunstest(mat, alternative));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_OrderedHeatMapAnalysis_cppARorder", (DL_FUNC) &_OrderedHeatMapAnalysis_cppARorder, 1},
    {"_OrderedHeatMapAnalysis_cppcorrNeighbor", (DL_FUNC) &_OrderedHeatMapAnalysis_cppcorrNeighbor, 1},
    {"_OrderedHeatMapAnalysis_cth_numgroup", (DL_FUNC) &_OrderedHeatMapAnalysis_cth_numgroup, 1},
    {"_OrderedHeatMapAnalysis_cth_numgroupZeroAlone", (DL_FUNC) &_OrderedHeatMapAnalysis_cth_numgroupZeroAlone, 1},
    {"_OrderedHeatMapAnalysis_cpprunstest", (DL_FUNC) &_OrderedHeatMapAnalysis_cpprunstest, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_OrderedHeatMapAnalysis(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
