#ifndef GETIDX_H
#define GETIDX_H

#include <Rcpp.h>
int getIdx_flq(Rcpp::NumericVector flq,
           int a,
           int yr,
           int unit,
           int season,
           int area,
           int it);

int getIdx_3D(Rcpp::NumericVector arr,
              int a,
              int yr,
              int it);

int getIdx_4D(Rcpp::NumericVector arr,
              int stk,
              int flt,
              int yr,
              int it);

#endif