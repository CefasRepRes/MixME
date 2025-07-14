#include <Rcpp.h>
#include "getidx.h"
#include "insertValue.h"
using namespace Rcpp;

// [[Rcpp::export]]
S4 insertNA_FLFisheries(S4 flts,                    // FLFisheries
                        CharacterVector fltsNames,   // character vector of fleet names
                        CharacterVector stksNames,   // character vector of stock names
                        IntegerVector   years) {
  
  // Clone FLFisheries to avoid overwriting original object
  S4 fltsS4 = clone(flts);
  
  // Extract vector of fleets names
  CharacterVector flts0Names = fltsS4.slot("names");
  
  // Calculate number of fleets
  int nfleet = flts0Names.size(); 
  
  // Extract list of fleets
  List fltsList = fltsS4.slot(".Data");
  
  // =========================================================================//
  // Loop over fleets that match fltsNames
  // =========================================================================//
  
  // update fleets stock-by-stock
  for (int st = 0; st < stksNames.size(); st++) {
    for (int fl = 0; fl < fltsNames.size(); fl++) {
      
      // First, check whether fleet is in vector of fleet names supplied as input
      int fltsCheck = -99;
      for (int fl0 = 0; fl0 < nfleet; fl0++) {
        if (fltsNames[fl] == flts0Names[fl0]) {
          fltsCheck = fl0;
        }
      } 
      
      // If match...
      if (fltsCheck != -99) {
        
        // Second, we again want to see if this fleet catches the stock that we are iterating over.
        
        // Extract data from fleet of interest
        S4 fltS4 = fltsList[fltsCheck];
        
        // Extract catch names
        CharacterVector catsNames = fltS4.slot("names");
        
        // How many catches?
        int catsSize = catsNames.size();
        
        // Create object to check if fleet catches stock
        int catsCheck = -99;
        
        // Check which names (if any) match current stock being looped over
        for(int ca = 0; ca < catsSize; ca++){
          if(stksNames[st] == catsNames[ca]){
            catsCheck = ca;
          }
        }
        
        // If fleet catches stock...
        if(catsCheck != -99){
          
          // In this section, I take the metier specific catchability and selectivity 
          // and calculate the fleet partial fishing mortality-at-age.
          
          // Extract data for stock catches
          List catsList = fltS4.slot(".Data");
          
          // Extract stock catch of interest
          S4 catS4 = catsList[catsCheck];
          
          // Extract landings.n, landings.wt, discards.n, discards.wt
          NumericVector cat_ln = catS4.slot("landings.n");
          NumericVector cat_lw = catS4.slot("landings.wt");
          NumericVector cat_dn = catS4.slot("discards.n");
          NumericVector cat_dw = catS4.slot("discards.wt");
          
          cat_ln = insertNA(cat_ln, years);
          cat_lw = insertNA(cat_lw, years);
          cat_dn = insertNA(cat_dn, years);
          cat_dw = insertNA(cat_dw, years);
          
          // Reinsert data
          catS4.slot("landings.n") = cat_ln;
          catS4.slot("discards.n") = cat_dn;
          catS4.slot("landings.wt") = cat_lw;
          catS4.slot("discards.wt") = cat_dw;
          
          catsList[catsCheck] = catS4;
          fltS4.slot(".Data") = catsList;
          
        } // END if fleet catches stock
        
        fltsList[fltsCheck] = fltS4;
        
      } // END if fleet in fleet names
    } // END loop over fleet
  } // END loop over stocks
    
    // Reinsert stocks and fleets
    fltsS4.slot(".Data") = fltsList;
    
    return(fltsS4);
  }