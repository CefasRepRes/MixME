// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// attach_attribute
S4 attach_attribute(S4 fisheries, NumericVector attribute, CharacterVector fl, CharacterVector st, CharacterVector attribute_name);
RcppExport SEXP _MixME_attach_attribute(SEXP fisheriesSEXP, SEXP attributeSEXP, SEXP flSEXP, SEXP stSEXP, SEXP attribute_nameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< S4 >::type fisheries(fisheriesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type attribute(attributeSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type fl(flSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type st(stSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type attribute_name(attribute_nameSEXP);
    rcpp_result_gen = Rcpp::wrap(attach_attribute(fisheries, attribute, fl, st, attribute_name));
    return rcpp_result_gen;
END_RCPP
}
// flr_to_list
List flr_to_list(List om, List advice, int year, int nstock, int nfleet, int niter);
RcppExport SEXP _MixME_flr_to_list(SEXP omSEXP, SEXP adviceSEXP, SEXP yearSEXP, SEXP nstockSEXP, SEXP nfleetSEXP, SEXP niterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type om(omSEXP);
    Rcpp::traits::input_parameter< List >::type advice(adviceSEXP);
    Rcpp::traits::input_parameter< int >::type year(yearSEXP);
    Rcpp::traits::input_parameter< int >::type nstock(nstockSEXP);
    Rcpp::traits::input_parameter< int >::type nfleet(nfleetSEXP);
    Rcpp::traits::input_parameter< int >::type niter(niterSEXP);
    rcpp_result_gen = Rcpp::wrap(flr_to_list(om, advice, year, nstock, nfleet, niter));
    return rcpp_result_gen;
END_RCPP
}
// fwd_update_fleets
List fwd_update_fleets(List om, List om_fwd, List tracking, int year, CharacterVector adviceType);
RcppExport SEXP _MixME_fwd_update_fleets(SEXP omSEXP, SEXP om_fwdSEXP, SEXP trackingSEXP, SEXP yearSEXP, SEXP adviceTypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type om(omSEXP);
    Rcpp::traits::input_parameter< List >::type om_fwd(om_fwdSEXP);
    Rcpp::traits::input_parameter< List >::type tracking(trackingSEXP);
    Rcpp::traits::input_parameter< int >::type year(yearSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type adviceType(adviceTypeSEXP);
    rcpp_result_gen = Rcpp::wrap(fwd_update_fleets(om, om_fwd, tracking, year, adviceType));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_MixME_attach_attribute", (DL_FUNC) &_MixME_attach_attribute, 5},
    {"_MixME_flr_to_list", (DL_FUNC) &_MixME_flr_to_list, 6},
    {"_MixME_fwd_update_fleets", (DL_FUNC) &_MixME_fwd_update_fleets, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_MixME(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
