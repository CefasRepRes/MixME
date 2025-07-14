#include <Rcpp.h>

/* This file contains helper functions to index the various multidimensional
 * arrays used by MixME. Specifically, the goal is to reduce redundancy in the
 * C++ code where these indexing operations occur frequently.
 */

// Get index for a 6D array (FLQuant)
int getIdx_flq(Rcpp::NumericVector flq,
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

// Get index for a 3D array (a, yr, it)
int getIdx_3D(Rcpp::NumericVector arr,
              int a,
              int yr,
              int it) {
  
  int idx = 
    (arr[1] * arr[0] * (it)) +
    (arr[0] * (yr)) +
    (a);
  return(idx);
}

// Get index for a 4D array (stk, flt, yr, it)
int getIdx_4D(Rcpp::NumericVector arr,
              int stk,
              int flt,
              int yr,
              int it) {
  
  int idx = 
    (arr[2] * arr[1] * arr[0] * (it)) +
    (arr[1] * arr[0] * (yr)) +
    (arr[0] * (flt)) +
    (stk);
  
  return(idx);
}