#ifndef GETREC_H
#define GETREC_H

#include <Rcpp.h>
double getRec(Rcpp::NumericVector params,
              double ssb,
              int st,
              Rcpp::CharacterVector recType);

#endif