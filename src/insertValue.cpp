#include <Rcpp.h>
#include "getidx.h"

/* This file contains helper functions to insert a double or NA for a defined
 * number of years in an FLQuant.
 */

Rcpp::NumericVector insertValue(Rcpp::NumericVector flq,
                                Rcpp::IntegerVector years,
                                double val) {
  
  Rcpp::NumericVector flq0 = clone(flq);
  
  // Extract dimensions sizes
  Rcpp::NumericVector flq_Dims        = flq.attr("dim");
  Rcpp::List flq_Dimnames             = flq.attr("dimnames");
  Rcpp::CharacterVector dimnameYear   = flq_Dimnames["year"];   // year
  
  // get length of years vector
  int nyrs = years.size();
  
  // find min and max year in FLQuant
  int minyear  = atoi(dimnameYear[0]);
  int maxyear  = atoi(dimnameYear[dimnameYear.size()-1]);
  
  // if year index is outside range, stop
  if (years[0] < minyear) Rcpp::stop("years < min FLQuant year");
  if (years[nyrs-1] > maxyear) Rcpp::stop("years > max FLQUant year");
  
  int startyr  = years[0] - minyear;
  int endyr    = years[nyrs-1] - minyear;
  
  // Loop over each age, year, iteration element
  for(int a = 0; a < flq_Dims[0]; a++){
    for(int yr = startyr; yr <= endyr; yr++){
      for (int mt = 0; mt < flq_Dims[4]; mt++) {
        for(int it = 0; it < flq_Dims[5]; it++){
          
          // Generate index for element of interest
          int idx = getIdx_flq(flq_Dims, a, yr, 0, 0, mt, it);
          
          flq[idx] = val;
          
        }
      }
    }
  }
  
  return(flq);
}

Rcpp::NumericVector insertNA(Rcpp::NumericVector flq,
                             Rcpp::IntegerVector years) {
  
  Rcpp::NumericVector flq0 = clone(flq);
  
  // Extract dimensions sizes
  Rcpp::NumericVector flq_Dims        = flq.attr("dim");
  Rcpp::List flq_Dimnames             = flq.attr("dimnames");
  Rcpp::CharacterVector dimnameYear   = flq_Dimnames["year"];   // year
  
  // get length of years vector
  int nyrs = years.size();
  
  // find min and max year in FLQuant
  int minyear  = atoi(dimnameYear[0]);
  int maxyear  = atoi(dimnameYear[dimnameYear.size()-1]);
  
  // if year index is outside range, stop
  if (years[0] < minyear) Rcpp::stop("years < min FLQuant year");
  if (years[nyrs-1] > maxyear) Rcpp::stop("years > max FLQUant year");
  
  int startyr  = years[0] - minyear;
  int endyr    = years[nyrs-1] - minyear;
  
  // Loop over each age, year, iteration element
  for(int a = 0; a < flq_Dims[0]; a++){
    for(int yr = startyr; yr <= endyr; yr++){
      for (int mt = 0; mt < flq_Dims[4]; mt++) {
        for(int it = 0; it < flq_Dims[5]; it++){
          
          // Generate index for element of interest
          int idx = getIdx_flq(flq_Dims, a, yr, 0, 0, mt, it);
          
          flq[idx] = NA_REAL;
          
        }
      }
    }
  }
  
  return(flq);
}

/* This function inserts a values from a reference FLQuant for a defined
 * number of years into a target FLQuant.
 */

Rcpp::NumericVector insertTarget(Rcpp::NumericVector targ,
                                 Rcpp::NumericVector refs,
                                 Rcpp::IntegerVector years) {
  
  Rcpp::NumericVector targ0 = clone(targ);
  
  // Extract dimensions sizes
  Rcpp::NumericVector targ_Dims  = targ.attr("dim");
  Rcpp::NumericVector refs_Dims  = refs.attr("dim");
  
  Rcpp::List targ_Dimnames       = targ.attr("dimnames");
  Rcpp::List refs_Dimnames       = refs.attr("dimnames");
  
  Rcpp::CharacterVector targYear = targ_Dimnames["year"];   // year
  Rcpp::CharacterVector refsYear = refs_Dimnames["year"];   // year
  
  // get length of years vector
  int nyrs = years.size();
  
  // find min and max year in FLQuant
  int mintargyear  = atoi(targYear[0]);
  int maxtargyear  = atoi(targYear[targYear.size()-1]);
  
  int minrefsyear  = atoi(refsYear[0]);
  int maxrefsyear  = atoi(refsYear[refsYear.size()-1]);
  
  // if age, metier and iteration dimensions do not match, stop
  if (targ_Dims[0] != refs_Dims[0]) Rcpp::stop("target and reference quant dims do not match");
  if (targ_Dims[4] != refs_Dims[4]) Rcpp::stop("target and reference area dims do not match");
  if (targ_Dims[5] != refs_Dims[5]) Rcpp::stop("target and reference iter dims do not match");
  
  // if year index is outside range, stop
  if (years[0] < mintargyear) Rcpp::stop("years < min target FLQuant year");
  if (years[0] < minrefsyear) Rcpp::stop("years < min reference FLQuant year");
  
  if (years[nyrs-1] > maxtargyear) Rcpp::stop("years > max target FLQUant year");
  if (years[nyrs-1] > maxrefsyear) Rcpp::stop("years > max reference FLQUant year");
  
  int startyr  = years[0] - mintargyear;
  int endyr    = years[nyrs-1] - mintargyear;
  int offsetyr = startyr - (years[0] - minrefsyear);
  
  // Loop over each age, year, iteration element
  for(int a = 0; a < targ_Dims[0]; a++){
    for(int yr = startyr; yr <= endyr; yr++){
      for (int mt = 0; mt < targ_Dims[4]; mt++) {
        for(int it = 0; it < targ_Dims[5]; it++){
          
          // Generate index for element of interest
          int refs_idx = getIdx_flq(refs_Dims, a, yr+offsetyr, 0, 0, mt, it);
          int targ_idx = getIdx_flq(targ_Dims, a, yr, 0, 0, mt, it);
          
          targ0[targ_idx] = refs[refs_idx];
          
        }
      }
    }
  }
  
  return(targ0);
}

/* This function inserts a values from a reference FLPar for a defined
 * number of years into a target FLPar.
 */

Rcpp::NumericVector insertTargetFLPar(Rcpp::NumericVector targ,
                                      Rcpp::NumericVector refs,
                                      Rcpp::IntegerVector years) {
  
  Rcpp::NumericVector targ0 = clone(targ);
  
  // Extract dimensions sizes
  Rcpp::NumericVector targ_Dims  = targ.attr("dim");
  Rcpp::NumericVector refs_Dims  = refs.attr("dim");
  
  bool useMet = false;
  if (targ_Dims.size() > 3) useMet = true;
  
  Rcpp::List targ_Dimnames       = targ.attr("dimnames");
  Rcpp::List refs_Dimnames       = refs.attr("dimnames");
  
  Rcpp::CharacterVector targYear = targ_Dimnames["year"];   // year
  Rcpp::CharacterVector refsYear = refs_Dimnames["year"];   // year
  
  // get length of years vector
  int nyrs = years.size();
  
  // find min and max year in FLQuant
  int mintargyear  = atoi(targYear[0]);
  int maxtargyear  = atoi(targYear[targYear.size()-1]);
  
  int minrefsyear  = atoi(refsYear[0]);
  int maxrefsyear  = atoi(refsYear[refsYear.size()-1]);
  
  // if age, metier and iteration dimensions do not match, stop
  if (targ_Dims[0] != refs_Dims[0]) Rcpp::stop("target and reference quant dims do not match");
  if (useMet == true) {
    if (targ_Dims[2] != refs_Dims[2]) Rcpp::stop("target and reference area dims do not match");
    if (targ_Dims[3] != refs_Dims[3]) Rcpp::stop("target and reference iter dims do not match");
  } else {
    if (targ_Dims[2] != refs_Dims[2]) Rcpp::stop("target and reference iter dims do not match");
  }
  
  // if year index is outside range, stop
  if (years[0] < mintargyear) Rcpp::stop("years < min target FLQuant year");
  if (years[0] < minrefsyear) Rcpp::stop("years < min reference FLQuant year");
  
  if (years[nyrs-1] > maxtargyear) Rcpp::stop("years > max target FLQUant year");
  if (years[nyrs-1] > maxrefsyear) Rcpp::stop("years > max reference FLQUant year");
  
  int startyr  = years[0] - mintargyear;
  int endyr    = years[nyrs-1] - mintargyear;
  int offsetyr = startyr - (years[0] - minrefsyear);
  
  // Loop over each age, year, iteration element
  if (useMet == true) {
    for(int a = 0; a < targ_Dims[0]; a++){
      for(int yr = startyr; yr <= endyr; yr++){
        for (int mt = 0; mt < targ_Dims[2]; mt++) {
          for(int it = 0; it < targ_Dims[3]; it++){
            
            // Generate index for element of interest
            int refs_idx = getIdx_4D(refs_Dims, a, yr+offsetyr, mt, it);
            int targ_idx = getIdx_4D(targ_Dims, a, yr, mt, it);
            
            targ0[targ_idx] = refs[refs_idx];
            
          }
        }
      }
    }
  } else {
    for(int a = 0; a < targ_Dims[0]; a++){
      for(int yr = startyr; yr <= endyr; yr++){
        for(int it = 0; it < targ_Dims[2]; it++){
          
          // Generate index for element of interest
          int refs_idx = getIdx_3D(refs_Dims, a, yr+offsetyr, it);
          int targ_idx = getIdx_3D(targ_Dims, a, yr, it);
          
          targ0[targ_idx] = refs[refs_idx];
          
        }
      }
    }
  }
  
  return(targ0);
}