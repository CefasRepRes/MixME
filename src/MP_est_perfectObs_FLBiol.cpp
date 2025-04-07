#include <Rcpp.h>
#include "getidx.h"
#include "findIdxVector.h"
#include "insertValue.h"
using namespace Rcpp;

// [[Rcpp::export]]
List MP_est_perfectObs_FLBiol(S4 stks,                     // FLBiols OM
                              S4 flts,                     // FLFisheries OM
                              S4   stk0,                   // FLBiols observed
                              List flts0,                  // FLFisheries observed
                              CharacterVector stk_obs_name,// character vector of stock names
                              IntegerVector   years_oem) {
  
  // Clone FLFisheries and FLBiols to avoid overwriting original object
  S4 stks_om  = clone(stks);
  S4 flts_om  = clone(flts);
  S4 stk_obs = clone(stk0);
  List flts_list_obs = clone(flts0);
  
  // find names associated with fleets
  CharacterVector flts_list_names = flts_list_obs.names();
  int flt_Check = findIdxVector(stk_obs_name, flts_list_names);
  
  if (flt_Check == -99) stop("Stock name does not match observed FLFisheries!");
  
  // extract FLFisheries associated with stock
  S4 flts_obs = flts_list_obs[flt_Check];
  
  // extract list of stocks and fleets
  List stks_om_List = stks_om.slot(".Data");
  List flts_om_List = flts_om.slot(".Data");
  List flts_obs_List = flts_obs.slot(".Data");
  
  // Extract vector of stocks and fleets names
  CharacterVector stks_om_names = stks_om.slot("names");
  CharacterVector flts_om_names = flts_om.slot("names");
  CharacterVector flts_obs_names = flts_obs.slot("names");  
  
  // Calculate number of stocks and fleets
  int nfleet = flts_obs_names.size();
  // int nstock = stks_om_names.size();
  
  // =========================================================================//
  // Handle stock of interest
  // =========================================================================//
  
  // check whether stock is in vector of stock names.
  int stks_Check = findIdxVector(stk_obs_name, stks_om_names);
  if (stks_Check == -99) stop("Stock name does not match FLBiols names!");
  
  // extract OM stock
  S4 stk_om = stks_om_List[stks_Check];
  
  // extract numbers slot
  NumericVector stk_om_n  = stk_om.slot("n");
  NumericVector stk_obs_n = stk_obs.slot("n");
  
  // insert stock numbers 
  stk_obs_n = insertTarget(stk_obs_n, stk_om_n, years_oem);
  stk_obs.slot("n") = stk_obs_n;
  
  // extract recruitment slots
  S4 stk_om_rec = stk_om.slot("rec");
  S4 stk_obs_rec = stk_obs.slot("rec");
  
  // insert recruitment information
  stk_obs_rec.slot("params") = stk_om_rec.slot("params");
  stk_obs_rec.slot("model")  = stk_om_rec.slot("model");
  stk_obs.slot("rec") = stk_obs_rec;
  
  // =========================================================================//
  // Loop over fleets
  // =========================================================================//
  
  for (int fl = 0; fl < nfleet; fl++) {
    
    // Extract data from fleet of interest
    S4 flt_obs_S4 = flts_obs_List[fl];
    
    // find corresponding reference fleet
    CharacterVector flt_obs_name(1);
    flt_obs_name = flts_obs_names[fl];
    int fltsCheck = findIdxVector(flt_obs_name, flts_om_names);
    
    if (fltsCheck != -99) {
      
      // extract om fleet
      S4 flt_om_S4 = flts_om_List[fltsCheck];
      
      // extract effort slot
      NumericVector flt_obs_effort = flt_obs_S4.slot("effort");
      NumericVector flt_om_effort  = flt_om_S4.slot("effort");
      
      // handle effort
      flt_obs_effort = insertTarget(flt_obs_effort, flt_om_effort, years_oem);
      
      // Extract data for stock catches
      List cats_obs_List = flt_obs_S4.slot(".Data");
      List cats_om_List = flt_om_S4.slot(".Data");
      
      // Extract catch names
      CharacterVector cats_obs_names = flt_obs_S4.slot("names");
      CharacterVector cats_om_names  = flt_om_S4.slot("names");
      
      // Check if this fleet catches the stock that we are processing
      int cats_obs_Check = findIdxVector(stk_obs_name, cats_obs_names);
      int cats_om_Check = findIdxVector(stk_obs_name, cats_om_names);  
      
      // If fleet catches stock...
      if((cats_obs_Check != -99) & (cats_om_Check != -99)){
        
        // Extract stock catch of interest
        S4 cat_obs_S4 = cats_obs_List[cats_obs_Check];
        S4 cat_om_S4  = cats_om_List[cats_om_Check];
        
        // Handle catch selectivity
        NumericVector catchSel_obs  = cat_obs_S4.slot("catch.sel");
        NumericVector catchSel_om   = cat_om_S4.slot("catch.sel");
        catchSel_obs = insertTarget(catchSel_obs, catchSel_om, years_oem);
        
        // Handle catchability
        NumericVector catchq_obs = cat_obs_S4.slot("catch.q");
        NumericVector catchq_om  = cat_om_S4.slot("catch.q");
        catchq_obs = insertTargetFLPar(catchq_obs, catchq_om, years_oem);
        
        cat_obs_S4.slot("catch.sel") = catchSel_obs;
        cat_obs_S4.slot("catch.q")   = catchq_obs;
        cats_obs_List[cats_obs_Check] = cat_obs_S4;
        
      } // END if fleet catches stock
      
      flt_obs_S4.slot(".Data")  = cats_obs_List;
      flt_obs_S4.slot("effort") = flt_obs_effort;
      
    } // END if reference fleet available
    
    flts_obs_List[fl] = flt_obs_S4;
    
  } // END loop over observed fleets
  
  flts_obs.slot(".Data") = flts_obs_List;
  
  return(List::create(_["stk0"] = stk_obs,
                      _["flt0"] = flts_obs));
}