#include <Rcpp.h>
#include "getidx.h"
using namespace Rcpp;

// [[Rcpp::export]]
List fwd_update_fleets(List om, List om_fwd, List tracking, int year, CharacterVector adviceType) {
  
  // Clone operating model to avoid overwriting original object
  List omc = clone(om);
  
  // Clone projected operating model to avoid overwriting original object
  List omc_fwd = clone(om_fwd);
  
  // clone tracking object
  List trackingc = clone(tracking);

  // ====================================
  // Prepare quota and over-quota arrays
  // ====================================
    
  // extract quota array
  NumericVector q_array = trackingc["quota"];
  
  // extract over-quota array
  NumericVector oq_array = trackingc["overquota"];
  
  // extract quota array dimensions
  NumericVector quota_Dims     = q_array.attr("dim");
  List          quota_Dimnames = q_array.attr("dimnames");
  
  // extract quota array dimensions names
  CharacterVector q_dimnameStock = quota_Dimnames["stk"]; // stock
  CharacterVector q_dimnameFleet = quota_Dimnames["flt"]; // fleet
  CharacterVector q_dimnameYear = quota_Dimnames["year"]; // year
  CharacterVector q_dimnameIter = quota_Dimnames["iter"]; // iteration
  
  // extract quota array dimensions
  NumericVector overquota_Dims     = oq_array.attr("dim");
  List          overquota_Dimnames = oq_array.attr("dimnames");
  
  // extract overquota array dimension names
  CharacterVector oq_dimnameStock = overquota_Dimnames["stk"]; // stock
  CharacterVector oq_dimnameFleet = overquota_Dimnames["flt"]; // fleet
  CharacterVector oq_dimnameYear = overquota_Dimnames["year"]; // year
  CharacterVector oq_dimnameIter = overquota_Dimnames["iter"]; // iteration
  
  // =================================
  // Loop over fleets
  // =================================
  
  // Extract all fleets 
  S4 fltsS4     = omc[1];
  S4 fltsS4_fwd = omc_fwd[1];
  
  // Extract vector of fleet names
  CharacterVector fltsNames     = fltsS4.slot("names");
  CharacterVector fltsNames_fwd = fltsS4_fwd.slot("names");
  
  // calculate number of fleets
  int nfleet     = fltsNames.size();
  int nfleet_fwd = fltsNames_fwd.size();
  
  // Check that numbers of fleets are identical
  if(nfleet != nfleet_fwd) {
    stop("Number of fleets in operating model and projection must be identical");
  }
  
  // Check that vectors of fleet names are identical
  for(int ti = 0; ti < nfleet; ti++) {
    if(fltsNames[ti] != fltsNames_fwd[ti]) {
      stop("Names of fleets in operating model and projection must be identical");
    }
  }
  
  // Extract list of fleets
  List fltsList     = fltsS4.slot(".Data");
  List fltsList_fwd = fltsS4_fwd.slot(".Data");
  
  // Loop over each fleet
  for(int fl = 0; fl < nfleet; fl++){
    
    // Extract fleet of interest
    S4 fltS4     = fltsList[fl];
    S4 fltS4_fwd = fltsList_fwd[fl];
    
    // ----------------------------------
    // Process effort
    // ----------------------------------
    
    // Extract effort 
    NumericVector effort     = fltS4.slot("effort");
    NumericVector effort_fwd = fltS4_fwd.slot("effort");
    
    // Create index for year and iteration of interest
    // -----------------------------------------------
    // Here, we need to loop over each year and iteration, and extract the
    // elements of interest
    
    // extract dimensions
    NumericVector effort_Dims     = effort_fwd.attr("dim");
    List          effort_Dimnames = effort_fwd.attr("dimnames");
    
    // extract year dimension
    CharacterVector dimnameYear = effort_Dimnames["year"]; // year
    
    // find location of year of interest
    int yr;
    for(int y = 0; y < effort_Dims[1]; y++){
      int yi = atoi(dimnameYear[y]);
      if(year == yi){
        yr = y;
      }
    }
    
    // Print year
    // Rcout << "... calculated effort year index " << yr << " \n";
    
    // loop over each iteration
    for(int it = 0; it < effort_Dims[5]; it++){
      
      // Generate index for element of interest
      int idx = getIdx_flq(effort_Dims, 0, yr, 0, 0, 0, it);

      // extract elements
      effort[idx] = effort_fwd[idx];
    }
    
    // Reinsert effort into slot
    fltS4.slot("effort") = effort;
    
    // ----------------------------------
    // Loop over catches
    // ----------------------------------
    
    // Extract catches 
    List catsList     = fltS4.slot(".Data");
    List catsList_fwd = fltS4_fwd.slot(".Data");
    
    // Extract vector of catches
    CharacterVector catsNames     = fltS4.slot("names");
    CharacterVector catsNames_fwd = fltS4_fwd.slot("names");
    
    // calculate number of catches
    int ncatch     = catsNames.size();
    int ncatch_fwd = catsNames_fwd.size();
    
    // Check same number of catches
    if(ncatch != ncatch_fwd){
      stop("In fleet %fl : Number of catches in operating model and projection must be identical");
    }
    
    // Check that vectors of fleet names are identical
    for(int ti = 0; ti < ncatch; ti++) {
      if(catsNames[ti] != catsNames_fwd[ti]) {
        stop("In fleet %fl : Names of catches in operating model and projection must be identical");
      }
    }
    
    // Loop over each catch
    for(int ca = 0; ca < ncatch; ca++){
      
      // Extract catches
      S4 catS4     = catsList[ca];
      S4 catS4_fwd = catsList_fwd[ca];
      
      // Extract landings and discards numbers-at-age
      NumericVector ln     = catS4.slot("landings.n");
      NumericVector ln_fwd = catS4_fwd.slot("landings.n");
      
      NumericVector dn     = catS4.slot("discards.n");
      NumericVector dn_fwd = catS4_fwd.slot("discards.n");
      
      // Extract landings and discards individual mean weights-at-age
      NumericVector lw     = catS4.slot("landings.wt");
      NumericVector lw_fwd = catS4_fwd.slot("landings.wt");
      
      NumericVector dw     = catS4.slot("discards.wt");
      NumericVector dw_fwd = catS4_fwd.slot("discards.wt");
      
      // Calculate overquota discards
      // -----------------------------------------------
      // Here, we need to loop over each year and iteration, and extract the
      // elements of interest
      
      // extract dimensions
      NumericVector ln_Dims     = ln_fwd.attr("dim");
      List          ln_Dimnames = ln_fwd.attr("dimnames");
      
      // extract year dimension
      dimnameYear = ln_Dimnames["year"];   // year
      
      // find location of year of interest
      for(int y = 0; y < ln_Dims[1]; y++){
        int yi = atoi(dimnameYear[y]);
        if(year == yi){
          yr = y;
        }
      }
      
      // Print stock name
      // Rcout << "fl: " << fltsNames[fl] << ", ca: " << catsNames[ca] << "\n";
      
      // Create vectors to store landings and discards vectors
      NumericVector ln_vector(ln_Dims[0]);
      NumericVector lw_vector(ln_Dims[0]);
      
      NumericVector dn_vector(ln_Dims[0]);
      NumericVector dw_vector(ln_Dims[0]);
      
      
      // loop over each iteration
      for(int it = 0; it < ln_Dims[5]; it++){
        
        // First extract vector of numbers and weights at age
        // --------------------------------------------------
        
        // loop over each age
        for(int a = 0; a < ln_Dims[0]; a++){
          
          // Generate index for element of interest
          int idx = getIdx_flq(ln_Dims, a, yr, 0, 0, 0, it);

          // check if any landings or discards are missing weight information.
          // If yes: throw error.
          
          if((ln_fwd[idx] > 0) & (lw_fwd[idx] == 0)) {
            stop("In 'fwdMixME': for fleet %lf and stock %ca, 'landings.n' > 0 but 'landings.wt' is 0");
          }
          
          if((dn_fwd[idx] > 0) & (dw_fwd[idx] == 0)) {
            stop("In 'fwdMixME': for fleet %lf and stock %ca, 'discards.n' > 0 but 'discards.wt' is 0");
          }
          
          // extract elements
          ln_vector[a] = ln_fwd[idx];
          dn_vector[a] = dn_fwd[idx];
          
          lw_vector[a] = lw_fwd[idx];
          dw_vector[a] = dw_fwd[idx];
          
          // Print index
          // Rcout << " ... " << idx;
          
        } // END loop age
        
        // Extract associated data on quotas by stock and fleet
        // ----------------------------------------------------
        
        // match index locations for stocks, fleets and year
        int q_yr;
        int q_flt;
        int q_stk;
        
        // stock
        for(int qi = 0; qi < quota_Dims[0]; qi ++) {
          if(q_dimnameStock[qi] == catsNames[ca]) {
            q_stk = qi;
          }
        }
        
        // fleet
        for(int qi = 0; qi < quota_Dims[1]; qi ++) {
          if(q_dimnameFleet[qi] == fltsNames[fl]) {
            q_flt = qi;
          }
        }

        // year
        for(int qi = 0; qi < quota_Dims[2]; qi ++) {
          if(q_dimnameYear[qi] == year) {
            q_yr = qi;
          }
        }
        
        // Generate index for quota array
        int q_idx = getIdx_4D(quota_Dims, q_stk, q_flt, q_yr, it);
        
        // extract available quota
        double quota = q_array[q_idx];
        
        // Define over-quota index for tracking array
        // ------------------------------------------
        
        // match index locations for stocks, fleets and year
        int oq_yr;
        int oq_flt;
        int oq_stk;
        
        // stock
        for(int qi = 0; qi < overquota_Dims[0]; qi ++) {
          if(oq_dimnameStock[qi] == catsNames[ca]) {
            oq_stk = qi;
          }
        }
        
        // fleet
        for(int qi = 0; qi < overquota_Dims[1]; qi ++) {
          if(oq_dimnameFleet[qi] == fltsNames[fl]) {
            oq_flt = qi;
          }
        }
        
        // year
        for(int qi = 0; qi < overquota_Dims[2]; qi ++) {
          if(oq_dimnameYear[qi] == year) {
            oq_yr = qi;
          }
        }
        
        // Generate index for overquota array
        int oq_idx = getIdx_4D(overquota_Dims, oq_stk, oq_flt, oq_yr, it);
        
        // Calculations if advice is landings-based
        // ----------------------------------------
        
        // If advice is landings based
        if(adviceType[0] == "landings") {
          
          // Calculate overall landings biomass
          double landings = sum(ln_vector * lw_vector);
          
          // check if landings exceeds quota
          if(landings > quota) {
            
            // If landings exceed quota, add excess to discards
            // - calculate proportion of excess landings numbers to be removed
            // ---- overquota * (lbiomass-at-age/sum(lbiomass)) / lmeanweight-at-age
            // - calculate proportion of discards numbers that are overquota 
            //   (used to update discard weights)
            
            // calculate over-quota biomass
            double overquota = landings - quota;
            
            // Update tracking object
            oq_array[oq_idx] = overquota;
            
            // NOTE:
            // In reality I would expect the selection pattern to be more heavily
            // skewed towards larger fish than the within-quota landings fraction
            // but we don't have data to parameterise this.
            
            // Calculate landings biomass distribution for relevant iterations
            NumericVector biomassdistribution = (ln_vector * lw_vector)/landings;
            
            // Calculate equivalent overquota numbers
            NumericVector overquota_n = (overquota * biomassdistribution) / lw_vector;
              
            // Update landings and discards numbers
            NumericVector ln_new = ln_vector - overquota_n;
            NumericVector dn_new = dn_vector + overquota_n;
            
            // Calculate proportion of overquota landings and discards numbers
            NumericVector overquotaproportion = overquota_n / (dn_vector + overquota_n);
            
            // Discards mean weight will now be higher because of discarding of
            // larger fish - calculate weighted mean for each age
            NumericVector dw_new = 
              (dw_vector * (1 - overquotaproportion)) + 
              (lw_vector * overquotaproportion);
            
            // Insert into FLCatch object
            // --------------------------
            
            // loop over each age
            for(int a = 0; a < ln_Dims[0]; a++){
              
              // Generate index for element of interest
              int idx = getIdx_flq(ln_Dims, a, yr, 0, 0, 0, it);
              
              // insert elements
              ln[idx] = ln_new[a];
              dn[idx] = dn_new[a];
              dw[idx] = dw_new[a];
              
            } // END loop age
          } else {
            
            // update tracking object
            oq_array[oq_idx] = 0;
            
            // Insert into FLCatch object
            // --------------------------
            
            // loop over each age
            for(int a = 0; a < ln_Dims[0]; a++){
              
              // Generate index for element of interest
              int idx = getIdx_flq(ln_Dims, a, yr, 0, 0, 0, it);
              
              // insert elements
              ln[idx] = ln_vector[a];
              dn[idx] = dn_vector[a];
              dw[idx] = dw_vector[a];
              
            } // END loop age
          } // END if landings > quota
        } // END landings-based advice
        
        // Calculations if advice is catch-based
        // -------------------------------------
        
        // if advice is catch-based
        if(adviceType[0] == "catch") {
          
          // Calculate overall catch biomass
          double catches = sum(ln_vector * lw_vector) + sum(dn_vector * dw_vector);
          
          // check if catch exceeds quota
          if(catches > quota) {
            
            // calculate over-quota biomass
            double overquota = catches - quota;
            
            // print progress
            // Rcout << "fl: " << fltsNames[fl] << ", ca: " << catsNames[ca] << ", catch: " << catches << ", quota: " << quota << "\n";
            // Rcout << "\n dn: " << dn_vector << "\n dw: " << dw_vector << "\n ln: " << ln_vector << "\n lw: " << lw_vector << "\n";
            
            // Update tracking object
            oq_array[oq_idx] = overquota;
            
            // calculate landings biomass fraction
            NumericVector landingsfraction = (ln_vector * lw_vector) / ((ln_vector * lw_vector) + (dn_vector * dw_vector));
            
            // Calculate landings biomass distribution
            NumericVector landingsbiomassdistribution = (ln_vector * lw_vector)/ sum(ln_vector * lw_vector);
            
            // Calculate discards biomass distribution
            NumericVector discardsbiomassdistribution = (dn_vector * dw_vector)/ sum(dn_vector * dw_vector);
            
            // Calculate equivalent overquota numbers
            NumericVector landingsoverquota_n = ((overquota * landingsbiomassdistribution) * landingsfraction) / lw_vector; 
            NumericVector discardsoverquota_n = ((overquota * discardsbiomassdistribution) * (1-landingsfraction)) / dw_vector; 
            
            // If landings/discards weight is zero for any age, this will result in NaN.
            // Replace these cases with zero numbers
            for(int ii = 0; ii < lw_vector.size(); ii++) {
              if(lw_vector[ii] == 0) {
                landingsoverquota_n[ii] = 0;
              }
              if(dw_vector[ii] == 0) {
                discardsoverquota_n[ii] = 0;
              }
            }
            
            // Update landings and discards numbers
            NumericVector ln_new = ln_vector - landingsoverquota_n;
            NumericVector dn_new = dn_vector + landingsoverquota_n;
            
            // Calculate proportion of overquota landings and discards numbers
            NumericVector overquotaproportion = landingsoverquota_n / dn_new;
            
            // Discards mean weight will now be higher because of discarding of
            // larger fish - calculate weighted mean for each age
            NumericVector dw_new = 
              (dw_vector * (1 - overquotaproportion)) + 
              (lw_vector * overquotaproportion);
            
            // Insert into FLCatch object
            // --------------------------
            
            // loop over each age
            for(int a = 0; a < ln_Dims[0]; a++){
              
              // Generate index for element of interest
              int idx = getIdx_flq(ln_Dims, a, yr, 0, 0, 0, it);
              
              // insert elements
              ln[idx] = ln_new[a];
              dn[idx] = dn_new[a];
              dw[idx] = dw_new[a];
              
            } // END loop age
          } else {
            
            // update tracking object
            oq_array[oq_idx] = 0;
            
            // Insert into FLCatch object
            // --------------------------
            
            // loop over each age
            for(int a = 0; a < ln_Dims[0]; a++){
              
              // Generate index for element of interest
              int idx = getIdx_flq(ln_Dims, a, yr, 0, 0, 0, it);
              
              // insert elements
              ln[idx] = ln_vector[a];
              dn[idx] = dn_vector[a];
              dw[idx] = dw_vector[a];
              
            } // END loop age 
          } // END if catch > quota
        } // END catch-based advice
      } // END iteration loop
      
      // Insert update numbers and weights into slots
      catS4.slot("landings.n")  = ln;
      catS4.slot("discards.n")  = dn;
      catS4.slot("discards.wt") = dw;
      
      // Insert catches into catch list
      catsList[ca] = catS4;
      
    } // END catch loop
    
    fltS4.slot(".Data") = catsList;
    fltsList[fl]        = fltS4;
    
  } // END fleet loop
  
  fltsS4.slot(".Data") = fltsList;
  omc[1]               = fltsS4;
  
  trackingc["overquota"] = oq_array;
  
  // Create list for output
  List out = List::create(_["om"]       = omc,
                          _["tracking"] = trackingc);
  
  return out;
}