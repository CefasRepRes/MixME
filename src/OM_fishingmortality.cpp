#include <Rcpp.h>
#include "getidx.h"
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector fa_cpp(NumericVector arr,
                     S4 flts,
                     CharacterVector stockname){
  
  // Clone input array to avoid overwriting input object
  NumericVector arrc = clone(arr);
  
  // Extract list of fleets
  List fltsList = flts.slot(".Data");
  
  // find fleet size
  int nflts = fltsList.size();
  
  // Rcout << "nflts " << nflts << "\n";
  
  // Make sure results array has correct dimensions
  NumericVector arr_Dims = arrc.attr("dim");
  int arr_Dimsize = arr_Dims.size();
  
  // Extract dimnames from the array
  List arr_Dimnames = arrc.attr("dimnames");
  CharacterVector arrAges   = arr_Dimnames["age"];    // age
  CharacterVector arrYear   = arr_Dimnames["year"];   // year
  CharacterVector arrUnit   = arr_Dimnames["unit"];   // unit
  CharacterVector arrSeason = arr_Dimnames["season"]; // season
  
  // loop over fleet data
  for(int fl = 0; fl < nflts; fl++) {
    
    // Extract fleet data
    S4 fltS4 = fltsList[fl];
    
    // Extract vector of catches names
    CharacterVector catsNames = fltS4.slot("names");
    
    // How many catches?
    int catsSize = catsNames.size();
    
    // Rcout << "names " << catsNames << "\n";
    
    // Create object to check if fleet catches stock
    int catsCheck = -99;
    
    // Check which names (if any) match current stock being looped over
    for(int ca = 0; ca < catsSize; ca++){
      if(stockname[0] == catsNames[ca]){
        catsCheck = ca;
      }
    }
    
    // Only process if fleet catches stock
    if(catsCheck != -99){
      
      // In this section, we take the inputted fleet effort, calculate the
      // proportional share of effort for each metier and calculate 
      // metier-specific effort.
      // 
      // Then I take the metier specific catchability and selectivity and
      // calculate the fleet partial fishing mortality-at-age.
      
      // Extract data for stock catches
      List catsList = fltS4.slot(".Data");
      
      // Extract stock of interest
      S4 catS4 = catsList[catsCheck];
      
      // Extract catchability
      NumericVector cat_cq = catS4.slot("catch.q");
      
      // Extract effort
      NumericVector flt_effort = fltS4.slot("effort");
      
      // Extract selectivitty
      NumericVector cat_sl = catS4.slot("catch.sel");
      
      // find dimensions
      // ---------------
      
      // Extract dimensions
      List stk_Dimnames = cat_sl.attr("dimnames");
      CharacterVector dimnameAges   = stk_Dimnames["age"];    // age
      CharacterVector dimnameYear   = stk_Dimnames["year"];   // year
      CharacterVector dimnameUnit   = stk_Dimnames["unit"];   // unit
      CharacterVector dimnameSeason = stk_Dimnames["season"]; // season
      
      // Extract dimensions sizes
      NumericVector stk_Dims = cat_sl.attr("dim");
      NumericVector eff_Dims = flt_effort.attr("dim");
      NumericVector q_Dims   = cat_cq.attr("dim");
      
      // Rcout << "Dims " << stk_Dims << "\n";
      if(arr_Dims[0] != stk_Dims[0]) {
        stop("array iterations do not match stock dimensions: age");
      }
      if(arr_Dims[1] != stk_Dims[1]) {
        stop("array iterations do not match stock dimensions: year");
      }
      if(arr_Dims[5] != stk_Dims[5]) {
        stop("array iterations do not match stock dimensions: iter");
      }
      
      // carry out crazy loop over each element of array
      // -----------------------------------------------
      
      // Loop over each age, year, iteration element
      for(int it = 0; it < arr_Dims[5]; it++){
        for(int a = 0; a < arr_Dims[0]; a++){
          for(int yr = 0; yr < arr_Dims[1]; yr++){
            for (int mt = 0; mt < stk_Dims[4]; mt++) {
              
              // Find age, year, iteration index to match source data structures
              int aidx = -99;
              int yidx = -99;
              for(int ai = 0; ai < stk_Dims[0]; ai++) {
                if(dimnameAges[ai] == arrAges[a]){
                  aidx = ai;
                }
              }
              for(int yi = 0; yi < stk_Dims[1]; yi++) {
                if(dimnameYear[yi] == arrYear[yr]) {
                  yidx = yi;
                }
              }
              
              if(aidx == -99){
                stop("array ages do not match stock dimensions");
              }
              if(yidx == -99){
                stop("array years do not match stock dimensions");
              }
              
              // Generate index for element of interest
              int idx_sl = getIdx_flq(stk_Dims, aidx, yidx, 0, 0, mt, it);
              int idx_ef = getIdx_flq(eff_Dims, 0, yidx, 0, 0, mt, it);
              
              // Extract catchability
              int idx_cq;
              if (q_Dims.size() > 3) {
                idx_cq = getIdx_4D(q_Dims, 0, yidx, mt, it);
              } else {
                idx_cq = getIdx_3D(q_Dims, 0, yidx, it);
              }
              
              // Generate index for element in results array
              int idx_ans;
              if (arr_Dimsize == 7) {
                idx_ans =
                  (arr_Dims[5] * arr_Dims[4] * arr_Dims[3] * arr_Dims[2] * arr_Dims[1] * arr_Dims[0] * (fl)) +
                  (arr_Dims[4] * arr_Dims[3] * arr_Dims[2] * arr_Dims[1] * arr_Dims[0] * (it)) +
                  (arr_Dims[3] * arr_Dims[2] * arr_Dims[1] * arr_Dims[0] * (mt)) + 
                  (arr_Dims[2] * arr_Dims[1] * arr_Dims[0] * (1 - 1)) + // points to season (assumed to be 1)
                  (arr_Dims[1] * arr_Dims[0] * (1 - 1)) + // points to unit (assumed to be 1)
                  (arr_Dims[0] * (yr)) +
                  (a);
              } else if (arr_Dimsize == 6) {
                idx_ans =
                  (arr_Dims[4] * arr_Dims[3] * arr_Dims[2] * arr_Dims[1] * arr_Dims[0] * (it)) +
                  (arr_Dims[3] * arr_Dims[2] * arr_Dims[1] * arr_Dims[0] * (mt)) + 
                  (arr_Dims[2] * arr_Dims[1] * arr_Dims[0] * (1 - 1)) + // points to season (assumed to be 1)
                  (arr_Dims[1] * arr_Dims[0] * (1 - 1)) + // points to unit (assumed to be 1)
                  (arr_Dims[0] * (yr)) +
                  (a);
              } else {
                stop("Error: insufficient results array dimensions");
              }

              // Rcout << "effort " << flt_effort[idx_ef] << " q " << cat_cq[idx_cq] << " sel " << cat_sl[idx_sl] << "ans" << flt_effort[idx_ef] * cat_cq[idx_cq] * cat_sl[idx_sl] << "\n";
              // Rcout << "idx " << idx_ans << "\n";
              
              arrc[idx_ans] += flt_effort[idx_ef] * cat_cq[idx_cq] * cat_sl[idx_sl];
            } // END loop mt
          } // END loop yr
        } // END loop a
      } // END loop it
    } // END if fleet catches stock
  } // END loop fl
  
  return(arrc);
}
