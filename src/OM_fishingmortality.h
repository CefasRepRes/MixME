#ifndef OM_fishingmortality_H
#define OM_fishingmortality_H

#include <Rcpp.h>
Rcpp::NumericVector fa_cpp(Rcpp::NumericVector arr,
                           Rcpp::S4 flts,
                           Rcpp::CharacterVector stockname);

#endif