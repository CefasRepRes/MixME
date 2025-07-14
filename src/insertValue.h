#ifndef INSERT_VAL
#define INSERT_VAL

#include <Rcpp.h>

Rcpp::NumericVector insertValue(Rcpp::NumericVector flq,
                                Rcpp::IntegerVector years,
                                double val);

Rcpp::NumericVector insertNA(Rcpp::NumericVector flq,
                             Rcpp::IntegerVector years);

Rcpp::NumericVector insertTarget(Rcpp::NumericVector targ,
                                 Rcpp::NumericVector refs,
                                 Rcpp::IntegerVector years);

Rcpp::NumericVector insertTargetFLPar(Rcpp::NumericVector targ,
                                      Rcpp::NumericVector refs,
                                      Rcpp::IntegerVector years);

#endif