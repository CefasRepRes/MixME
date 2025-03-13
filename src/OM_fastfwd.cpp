#include <Rcpp.h>
#include "getidx.h"
#include "getrec.h"
#include "OM_fishingmortality.h"
using namespace Rcpp;

// [[Rcpp::export]]
List fast_fwd(List om,                    // [0] = FLBiols, [1] = FLFisheries
              int year,                   // 4-digit year
              NumericMatrix effort,       // effort for each fleet [nf,ni]
              CharacterVector recType,    // stock recruitment code for each stock
              IntegerVector   popType,    // population dynamics code for each stock
              List sr_residuals) {
  
  // Population Dynamics Code
  // - 0: age-structured
  // - 1: biomass-dynamics
  // - 2: fixed population
  
  // Clone operating model to avoid overwriting original object
  List omc = clone(om);
  
  // -------------------//
  // Extract stock data
  // -------------------//
  
  // Extract all stocks
  S4 stksS4 = omc[0];
  
  // Extract vector of stock names
  CharacterVector stksNames = stksS4.slot("names");
  
  // Calculate number of stocks
  int nstock = stksNames.size(); 
  
  // Extract list of stocks
  List stksList = stksS4.slot(".Data");
  
  // -------------------//
  // Extract fleet data
  // -------------------//
  
  // Extract all fleets
  S4 fltsS4 = omc[1];
  
  // Extract vector of fleets names
  CharacterVector fltsNames = fltsS4.slot("names");
  
  // Calculate number of fleets
  int nfleet = fltsNames.size(); 
  
  // Extract list of fleets
  List fltsList = fltsS4.slot(".Data");
  
  // =========================================================================//
  // Project Effort
  // =========================================================================//
  
  // loop over fleet
  for (int fl = 0; fl < nfleet; fl++) {
    
    // Extract data from fleet of interest
    S4 fltS4 = fltsList[fl];
    
    // Extract effort
    NumericVector flt_effort = fltS4.slot("effort");
    
    // Extract dimensions sizes
    NumericVector eff_Dims = flt_effort.attr("dim");
    
    // Extract year dimension
    List eff_Dimnames = flt_effort.attr("dimnames");
    CharacterVector dimnameYear = eff_Dimnames["year"];
    
    // create index for year of interest
    int minyr = atoi(dimnameYear[0]);
    int yr    = year-minyr;
    
    // Loop over iterations first because these are independent
    for (int it = 0; it < eff_Dims[5]; it++) {
      
      // initialise variable to hold total effort in fleet
      double toteff = 0;
      
      // Calculate total effort across metiers for future effort-share calculation
      for (int mt = 0; mt < eff_Dims[4]; mt++) {
        
        // Get effort index
        int idx_ef = getIdx_flq(eff_Dims, 0, yr, 0, 0, mt, it);
        
        // Extract effort
        toteff += flt_effort[idx_ef];
        
      } // END loop over metier
      
      // Calculate partial effort for each metier
      for (int mt = 0; mt < eff_Dims[4]; mt++) {
        
        // Get effort index
        int idx_ef = getIdx_flq(eff_Dims, 0, yr, 0, 0, mt, it);
        
        // Extract effort
        double parteff = flt_effort[idx_ef];
        flt_effort[idx_ef] = (parteff/toteff) * effort(fl,it);
        
        // Check calculation
        // Rcout << "effort allocation for mt " << mt << ": "<< flt_effort[idx_ef] << "\n";
      } // END loop over metier
    } // END loop over iteration
    
    // reinsert updated effort array
    fltS4.slot("effort") = flt_effort;
    fltsList[fl]         = fltS4;
    
  } // END loop over fleets
  
  // =========================================================================//
  // Project Catches (stock-by-stock)
  // =========================================================================//
  
  // loop over stock
  for (int st = 0; st < nstock; st++) {
    
    // In this section, I need to extract the numbers and natural mortality
    // for the stock.
    
    // Extract stock data
    S4 stkS4 = stksList[st];
    
    // Extract slots of interest
    NumericVector stk_n = stkS4.slot("n");
    NumericVector stk_m = stkS4.slot("m");
    
    List stk_mat_obj      = stkS4.slot("mat");
    NumericVector stk_mat = stk_mat_obj[0];
    NumericVector stk_wt  = stkS4.slot("wt");
    S4 stk_rec          = stkS4.slot("rec");
    
    // Extract dimension names using numbers-at-age as a template
    List stk_n_Dimnames = stk_n.attr("dimnames");
    CharacterVector dimnameAges   = stk_n_Dimnames["age"];    // age
    CharacterVector dimnameYear   = stk_n_Dimnames["year"];   // year
    CharacterVector dimnameUnit   = stk_n_Dimnames["unit"];   // unit
    CharacterVector dimnameSeason = stk_n_Dimnames["season"]; // season
    
    // Extract dimension sizes using numbers-at-age as a template
    NumericVector stk_n_Dims = stk_n.attr("dim");
    
    // Create object to store fishing mortality using numbers-at-age as a template
    NumericVector stk_F_init = clone(stk_n);
    stk_F_init.fill(0.0);
    
    // create index for year of interest
    int minyr = atoi(dimnameYear[0]);
    int yr    = year-minyr;
    
    // -------------------------------//
    // Total Fishing Mortality
    // -------------------------------//
    
    // The first step in the process is to find the new fishing mortality-at-age
    // for the stock given the inputted vector of efforts. This total fishing
    // mortality will then be used to calculate the catches given a partial
    // fishing mortality.
    
    CharacterVector stName(1);
    stName[0] = stksNames[st];
    
    NumericVector stk_F = fa_cpp(stk_F_init, fltsS4, stName);
    
    // Rcout << "F: " << stk_F << "\n";
    
    // -----------------------------------------------------------------------//
    // Removals
    // -----------------------------------------------------------------------//
    
    // In this section, we loop over each fleet again, but this time we want to
    // calculate the numbers of landed and discarded fish within each metier.
    
    for (int fl = 0; fl < nfleet; fl++) {
      
      // In this first section, we again want to see if this fleet catches the
      // stock that we are iterating over.
      
      // Extract data from fleet of interest
      S4 fltS4 = fltsList[fl];
      
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
        
        // Extract catchability
        NumericVector cat_cq = catS4.slot("catch.q");
        
        // Extract effort
        NumericVector flt_effort = fltS4.slot("effort");
        
        // Extract selectivity
        NumericVector cat_sl = catS4.slot("catch.sel");
        
        // Extract landings
        NumericVector cat_land = catS4.slot("landings.n");
        
        // Extract discards
        NumericVector cat_disc = catS4.slot("discards.n");
        
        // find dimensions
        // ---------------
        
        // Extract dimensions sizes
        NumericVector cat_Dims = cat_sl.attr("dim");
        NumericVector eff_Dims = flt_effort.attr("dim");
        NumericVector q_Dims   = cat_cq.attr("dim");
        
        // carry out crazy loop over each element of array
        // -----------------------------------------------
        
        // Loop over iterations first because these are independent
        for (int it = 0; it < cat_Dims[5]; it++) {
          for (int a = 0; a < cat_Dims[0]; a++) {
            for (int mt = 0; mt < cat_Dims[4]; mt++) {
              
              // Calculate partial fishing mortality
              // ------------------------------------
              
              // Extract selectivity at age
              int idx_sel = getIdx_flq(cat_Dims, a, yr, 0, 0, mt, it);
              double sel = cat_sl[idx_sel];
              
              // Extract effort
              int idx_ef = getIdx_flq(eff_Dims, 0, yr, 0, 0, mt, it);
              double eff = flt_effort[idx_ef];
              
              // Extract catchability
              int idx_cq;
              if (q_Dims.size() > 3) {
                idx_cq = getIdx_4D(q_Dims, 0, yr, mt, it);
              } else {
                idx_cq = getIdx_3D(q_Dims, 0, yr, it);
              }
              double cq = cat_cq[idx_cq];
              
              // Calculate partial fishing mortality indexed by metier, age, iteration
              double partF = cq * eff * sel;
              
              // Calculate catches 
              // -----------------------------
              
              // Calculate index to extract total fishing mortality, natural mortality, numbers
              int idx = getIdx_flq(stk_n_Dims, a, yr, 0, 0, 0, it);
              
              // Calculate index to extract total fishing mortality, natural mortality, numbers
              int idx_catch = getIdx_flq(stk_n_Dims, a, yr, 0, 0, mt, it);
              
              // Calculate catch using Baranov Equation
              double catches = (partF/(stk_F[idx] + stk_m[idx])) * (1 - exp(-(stk_F[idx] + stk_m[idx]))) * stk_n[idx];
              
              // Split into landings and discards
              double landingsfraction = cat_land[idx_catch] / (cat_land[idx_catch] + cat_disc[idx_catch]);
              cat_land[idx_catch] = catches * landingsfraction;
              cat_disc[idx_catch] = catches * (1 - landingsfraction);
              
              // Report fleet, stock, effort
              // Rcout << "fleet: " << fl << " stock: " << st << " age: " << a << " ";
              // Rcout << "catchq: " << cq << " eff: "<< eff << " sel: "<< sel << "\n";
              // Rcout << "partF: " << partF << " Fage: "<< stk_F[idx] << " n: "<< stk_n[idx] << " m: " << stk_m[idx] << "\n";
              // Rcout << "catches: " << catches << " landing: "<< cat_land[idx_catch] << " discards: "<< cat_disc[idx_catch] << "\n";
              
            } // END loop over metier
          } // END loop over age
        } // END loop over iteration
        
        // Reinsert data
        catS4.slot("landings.n") = cat_land;
        catS4.slot("discards.n") = cat_disc;
        catsList[catsCheck] = catS4;
        fltS4.slot(".Data") = catsList;
        
      } // END if fleet catches stock
      
      fltsList[fl] = fltS4;
      
    } // END loop over fleet
    
    // -----------------------------------------------------------------------//
    // Survival and Recruitment
    // -----------------------------------------------------------------------//
    //
    // Let's leverage the fact that we have already calculated total fishing 
    // mortality for this stock. We don't actually care about area divisions
    // here - we're assuming a single stock unit.
    //
    // However, we need to be careful about treating age-structured, 
    // biomass-based and fixed populations separately.
    
    // TO DO: we also need to add process deviances to this...
    
    if (yr < stk_n_Dims[1]-1) { // Only run if < final year
      
      // Age structured populations
      if (popType[st] == 0) {
        
        // Calculate SSB
        double ssb = 0;
        
        // Calculate survivors for each age
        for (int it = 0; it < stk_n_Dims[5]; it++) {
          
          // -------------------------------------------------------------------//
          // Survival
          // -------------------------------------------------------------------//
          
          // loop over post-recruitment ages
          for (int a = 1; a < stk_n_Dims[0]; a++) {
            
            // Calculate index to extract total fishing mortality, natural mortality, numbers
            int idx_get = getIdx_flq(stk_n_Dims, a-1, yr, 0, 0, 0, it);
            
            // Calculate index to insert total fishing mortality, natural mortality, numbers
            int idx_insert = getIdx_flq(stk_n_Dims, a, yr+1, 0, 0, 0, it);
            
            stk_n[idx_insert] = stk_n[idx_get] * exp(-(stk_m[idx_get] + stk_F[idx_get]));
            
            // Additional calculation of plus group survivors
            if(a == stk_n_Dims[0]-1) {
              
              // Calculate index to extract total fishing mortality, natural mortality, numbers
              int idx_plus = getIdx_flq(stk_n_Dims, a, yr, 0, 0, 0, it);
              
              stk_n[idx_insert] += stk_n[idx_plus] * exp(-(stk_m[idx_plus] + stk_F[idx_plus]));
            } // END if plus group
          } // END loop over ages
          
          // -------------------------------------------------------------------//
          // Recruitment
          // -------------------------------------------------------------------//
          // 
          // We want to support the full range of stock-recruit functions that are
          // available in FLasher. These are:
          //
          // - Ricker (ricker)
          // - Beverton-Holt (bevholt)
          // - constant (or mean)
          // - bevholtSS3
          // - cushing
          // - segreg
          // - survsrr
          // - bevholtsig
          // - mixedsrr

          // We want to pick up the SSB consistent with the age of the recruitment
          // age group. So next year SSB if age=0, current year SSB if age=1,
          // last year SSB if age=2.
          
          // Calculate recruitment lag
          int minage = atoi(dimnameAges[0]);
          
          // first zero-out recruitment-age numbers in yr+1 - this is important
          // for handling age-0 recruitment SSB calculations
          
          // Index to insert results
          int idx_insert = getIdx_flq(stk_n_Dims, 0, yr+1, 0, 0, 0, it);
          stk_n[idx_insert] = 0;
          
          // Calculate spawning stock biomass
          ssb = 0;
          for (int a = 0; a < stk_n_Dims[0]; a++) {
            int idx_get = getIdx_flq(stk_n_Dims, a, yr+1-minage, 0, 0, 0, it);
            ssb += stk_n[idx_get] * stk_mat[idx_get] * stk_wt[idx_get];
          }
          
          // print SSB
          // Rcout << "SSB " << ssb << "\n";
          
          // Extract parameters
          NumericVector params_rec  = stk_rec.slot("params");
          NumericVector params_Dims = params_rec.attr("dim");
          List params_rec_Dimnames  = params_rec.attr("dimnames");
          CharacterVector params_rec_dimnameyear = params_rec_Dimnames["year"];
          
          // Extract parameters for this iteration
          NumericVector params(params_Dims[0]);
          
          // handle optional year dimension in recruitment parameters 
          for (int pi=0; pi < params_Dims[0]; pi++) {
            if (params_Dims.size() > 2) {
              
              // We need to handle cases where we have a truncated timeseries
              // so we need to find the correct year index
              int yr_rec = -99;
              for(int yr_id = 0; yr_id < params_Dims[1]; yr_id++) {
                if(dimnameYear[yr+1] == params_rec_dimnameyear[yr_id]) {
                  yr_rec = yr_id;
                }
              }
              
              int pi_idx = getIdx_3D(params_Dims, pi, yr_rec, it);
              params[pi] = params_rec[pi_idx];
            } else {
              int pi_idx = (params_Dims[0] * (it)) + (pi);
              params[pi] = params_rec[pi_idx];
            }
          }
          
          // print SR params
          Rcout << "SR params " << params << "\n";
          
          double rec = getRec(params, ssb, st, recType);
          
          // Recruitment Noise
          // -------------------------------------------------------------------//
          //
          // We might have cases where recruitment error has a different year
          // dimension compared to the Operating model. Hence, we need to match
          // year indices to extract the correct values.
          
          // Extract stock-recruitment noise
          NumericVector SRres = sr_residuals[st];
          
          // Extract stock-recruitment noise dimensions
          NumericVector SRres_Dims = SRres.attr("dim");
          List SRres_Dimnames      = SRres.attr("dimnames");
          CharacterVector SRres_dimnameyear = SRres_Dimnames["year"];
          
          // Find year index
          int yr_sr = -99;
          for(int yr_id = 0; yr_id < SRres_Dims[1]; yr_id++) {
            if(dimnameYear[yr] == SRres_dimnameyear[yr_id]) {
              yr_sr = yr_id;
            }
          }
          
          if (yr_sr == -99) {stop("SR noise year dimensions do not match operating model dimensions!");}

          int idx_SRres =
            (SRres_Dims[4] * SRres_Dims[3] * SRres_Dims[2] * SRres_Dims[1] * SRres_Dims[0] * (it)) +
            (SRres_Dims[3] * SRres_Dims[2] * SRres_Dims[1] * SRres_Dims[0] * (1-1)) + // points to area (assumed to be 1)
            (SRres_Dims[2] * SRres_Dims[1] * SRres_Dims[0] * (1 - 1)) + // points to season (assumed to be 1)
            (SRres_Dims[1] * SRres_Dims[0] * (1 - 1)) + // points to unit (assumed to be 1)
            (SRres_Dims[0] * (yr_sr+1)) +
            (0);
          double SRresi = SRres[idx_SRres];
          
          // print SR residual
          // Rcout << "SR residual " << SRresi << "\n";
          
          stk_n[idx_insert] = rec * SRresi;
          
          // print recruitment
          // Rcout << "rec " << rec << "\n";
          
        } // END loop over iteration
      } // END age-structured population
    } // END if yr < maxyear
  } // END loop over stock
  
  // Reinsert stocks and fleets
  stksS4.slot(".Data") = stksList;
  fltsS4.slot(".Data") = fltsList;
  
  omc[0] = stksS4;
  omc[1] = fltsS4;
  
  return(omc);
}