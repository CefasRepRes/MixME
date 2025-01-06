#ifndef GETIDX_H
#define GETIDX_H

#include <Rcpp.h>
int getIdx_flq(NumericVector flq,
           int quant,
           int yr,
           int unit,
           int season,
           int area,
           int it);

#endif