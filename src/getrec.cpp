#include <Rcpp.h>

/* This file contains helper functions to implement variuous stock recruitment
 * functions.
 */

// Implement recruitment
double getRec(Rcpp::NumericVector params,
              double ssb,
              int st,
              Rcpp::CharacterVector recType){
  
  // Initialise recruitment
  double rec = 0;
  
  // Check that recruitment model exists
  if ((recType[st] != "ricker") &
      (recType[st] != "bevholt") &
      (recType[st] != "constant") &
      (recType[st] != "bevholtSS3") &
      (recType[st] != "cushing") &
      (recType[st] != "segreg") &
      (recType[st] != "survsrr") &
      (recType[st] != "bevholtsig") &
      (recType[st] != "mean")) {
    Rcpp::stop("SR model not found\n");
  }
  
  // Ricker SR model
  if (recType[st] == "ricker") {
    
    // check size of parameter vector
    if (params.size() < 2) Rcpp::stop("ricker: 2 stock-recruit parameters needed");
    
    rec = params[0] * ssb * exp(-params[1] * ssb);
  }
  
  // Beverton-Holt SR model
  if (recType[st] == "bevholt") {
    
    // check size of parameter vector
    if (params.size() < 2) Rcpp::stop("bevholt: 2 stock-recruit parameters needed");
    
    rec = params[0] * ssb / (params[1] + ssb);
    
    if (params.size() > 2) {
      rec = params[0] / (1 + pow(params[1]/ssb, params[2]));
    }
  }
  
  // Constant recruitment
  if (recType[st] == "constant" | recType[st] == "mean") {
    rec = params[0]; 
  }
  
  // SS3 implementation of Beverton-Holt
  if (recType[st] == "bevholtSS3") {
    // check size of parameter vector
    if (params.size() < 3) Rcpp::stop("bevholtSS3: 3 stock-recruit parameters needed");
    
    rec = (4.0 * params[0] * params[1] * ssb) / (params[2] * (1.0 - params[0]) + ssb * (5 * params[0] - 1.0));
    
  }
  
  // Cushing SR model
  if (recType[st] == "cushing") {
    // check size of parameter vector
    if (params.size() < 2) Rcpp::stop("cushing: 2 stock-recruit parameters needed");
    rec = params[0] * exp(log(ssb)*params[1]);
  }
  
  // Segmented regression SR model
  if (recType[st] == "segreg") {
    
    // check size of parameter vector
    if (params.size() < 2) Rcpp::stop("segreg: 2 stock-recruit parameters needed");
    
    if(ssb <= params[1]) {
      rec = params[0] * ssb;
    }
    else {
      rec = params[0] * params[1];
    }
  }
  
  // Surv SR model
  if (recType[st] == "survsrr") {
    
    // check size of parameter vector
    if (params.size() < 4) Rcpp::stop("survsrr: 4 stock-recruit parameters needed");
    
    // sratio is the recruits sex ratio, default 0.5 assumes 2 sex model
    double sratio = 0.5;
    if (params.size() > 4) {
      sratio = params[4];
    }
    
    double z0 = log(1.0 / (params[3] / params[0]));
    double zmax = z0 + params[1] * (0.0 - z0);
    double zsurv = exp((1.0 - pow((ssb / params[3]), params[2])) * (zmax - z0) + z0);
    
    // Sex ratio at recruitment set at 1:1
    rec = ssb * zsurv * sratio;
  }
  
  if (recType[st] == "bevholtsig") {
    
    // check size of parameter vector
    if (params.size() < 3) Rcpp::stop("bevholtsig: 3 stock-recruit parameters needed");
    
    // 1 Bevholt, rec = a * srp / (b + srp)
    if (params[2] == 1) {
      rec = params[0] * ssb / (params[1] + ssb);
    } else if (params[2] == 2) {
      
      // 2 Ricker, rec = a * srp * exp (-b * srp)
      rec = params[0] * ssb * exp(-params[1] * ssb);
      
      // 3 Segreg, rec = if(ssb < b) a * ssb else a * b
    } else if (params[2] == 3) {
      if(ssb <= params[1]) {
        rec = params[0] * ssb;
      } else {
        rec = params[0] * params[1];
      }
    }
  }
  
  return(rec);
}

