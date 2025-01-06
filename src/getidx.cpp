#include <Rcpp.h>

int getIdx_flq(NumericVector flq,
           int a,
           int yr,
           int unit,
           int season,
           int mt,
           int it) {
  
  // Generate index for element of interest
  int idx =
    (flq[4] * flq[3] * flq[2] * flq[1] * flq[0] * (it)) + // points to iteration
    (flq[3] * flq[2] * flq[1] * flq[0] * (mt)) +          // points to metier
    (flq[2] * flq[1] * flq[0] * (season)) +               // points to season
    (flq[1] * flq[0] * (unit)) +                          // points to unit
    (flq[0] * (yr)) +                                     // points to year
    (a);                                                  // points to quant
  
  return(idx);
}