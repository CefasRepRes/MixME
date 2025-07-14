#include <Rcpp.h>
#include "getidx.h"
using namespace Rcpp;

// [[Rcpp::export]]
List flr_to_list(List om, List advice, int year, int nstock, int nfleet, int niter, int avgE_nyear){

  // Clone operating model to avoid overwriting original object
  List omc = clone(om);

  // Create container for result
  List omList(niter);

  // Create container for testing
  List test;

  // =================================
  // Loop over iterations
  // =================================

  // Loop over each iteration
  for(int it = 0; it < niter; it++){

    // Create lists for: numbers-at-age
    //                   natural mortality-at-age
    //                   landings individual mean weight-at-age
    //                   discards individual mean weight-at-age
    //                   proportion catch retained-at-age
    //                   selection-at-age
    List n_age(nstock);
    List m_age(nstock);
    List lw_age(nstock);
    List dw_age(nstock);
    List lf_age(nstock);
    List sl_age(nstock);

    // Extract all stocks
    S4 stksS4 = omc[0];

    // Extract vector of stock names
    CharacterVector stksNames = stksS4.slot("names");

    // Rprintf("Processing n-at-age, m-at-age \n");

    // Extract list of stocks
    List stksList = stksS4.slot(".Data");

    // ----------------------------------------------
    // Process numbers-at-age           (vector: age)
    // Process natural mortality-at-age (vector: age)
    // ----------------------------------------------

    // Process each stock
    for(int st = 0; st < nstock; st++){

      // Extract stock of interest
      S4 stkS4 = stksList[st];

      // Extract slots of interest
      NumericVector stk_n = stkS4.slot("n");
      NumericVector stk_m = stkS4.slot("m");

      // Length of numeric vector
      // int stk_n_Size =  stk_n.size();

      // Extract dimensions for number-at-age
      List stk_n_Dimnames = stk_n.attr("dimnames");
      CharacterVector dimnameAges   = stk_n_Dimnames["age"];    // age
      CharacterVector dimnameYear   = stk_n_Dimnames["year"];   // year
      // CharacterVector dimnameUnit   = stk_n_Dimnames["unit"];   // unit
      // CharacterVector dimnameSeason = stk_n_Dimnames["season"]; // season

      // Extract dimensions for natural mortality-at-age
      // List stk_m_Dimnames = stk_m.attr("dimnames");
      // CharacterVector dimnameAges_m   = stk_m_Dimnames["age"];    // age
      // CharacterVector dimnameYear_m   = stk_m_Dimnames["year"];   // year
      // CharacterVector dimnameUnit_m   = stk_m_Dimnames["unit"];   // unit
      // CharacterVector dimnameSeason_m = stk_m_Dimnames["season"]; // season

      // Extract dimensions sizes
      NumericVector stk_n_Dims = stk_n.attr("dim");
      NumericVector stk_m_Dims = stk_m.attr("dim");

      // Create empty vector for result
      NumericVector n_stAge(stk_n_Dims[0]);
      NumericVector m_stAge(stk_m_Dims[0]);

      // create index for year of interest
      int minyr = atoi(dimnameYear[0]); // convert from element of CharacterVector to integer
      int yr = (year-minyr);

      // Rprintf("... calculated year index \n");

      // Loop over each age of interest
      for(int a = 0; a < stk_n_Dims[0]; a++){

        // extract age
        // int ai = atoi(dimnameAges[a]);

        // Generate index for element of interest
        int idx = getIdx_flq(stk_n_Dims, a, yr, 0, 0, 0, it);

        // Rcout << "extract age " << a << " element " << element << " value " << stk_n[element] << "\n";

        // Insert value into vector
        n_stAge[a] = stk_n[idx];
        m_stAge[a] = stk_m[idx];

      }
      // Rcout << "Num at age " << n_stAge << "\n";
      // Rprintf("... extracted value at age \n");

      // add names to age vector
      n_stAge.names() = dimnameAges;
      m_stAge.names() = dimnameAges;

      // Insert result into n_age object
      n_age[st] = n_stAge;
      m_age[st] = m_stAge;

    }

    // -----------------------------------------------------------------------
    // Process: landings individual mean weight-at-age (matrix: age x fleet)
    //          discards individual mean weight-at-age (matrix: age x fleet)
    //          proportional catch retained-at-age     (matrix: age x fleet)
    //          catch selection-at-age                 (matrix: age x fleet)
    // -----------------------------------------------------------------------

    // Extract all fleets
    S4 fltsS4 = omc[1];

    // Extract vector of fleets names
    CharacterVector fltsNames = fltsS4.slot("names");

    // Extract list of fleets
    List fltsList = fltsS4.slot(".Data");

    // Rprintf("processing landings, discards weights etc... \n");

    // Loop over stocks  - extract dimensions and generate results object
    // Loop over fleets  - check if stock is captured by fleet
    //                   - if yes, extract catches and process
    for(int st = 0; st < nstock; st++){

      // Extract stock of interest
      S4 stkS4 = stksList[st];

      // Extract a slot containing the age dimension
      NumericVector stk_n = stkS4.slot("n");

      // Extract dimensions for number-at-age
      List stk_n_Dimnames = stk_n.attr("dimnames");
      CharacterVector dimnameAges   = stk_n_Dimnames["age"];    // age
      CharacterVector dimnameYear   = stk_n_Dimnames["year"];   // year
      // CharacterVector dimnameUnit   = stk_n_Dimnames["unit"];   // unit
      // CharacterVector dimnameSeason = stk_n_Dimnames["season"]; // season

      // Extract dimensions sizes
      NumericVector stk_n_Dims = stk_n.attr("dim");

      // Generate matrix of age x fleet for each variable
      NumericMatrix lw_stAgeFlt(stk_n_Dims[0], nfleet);
      NumericMatrix dw_stAgeFlt(stk_n_Dims[0], nfleet);
      NumericMatrix lf_stAgeFlt(stk_n_Dims[0], nfleet);
      NumericMatrix sl_stAgeFlt(stk_n_Dims[0], nfleet);
      
      // create index for year of interest
      int minyr = atoi(dimnameYear[0]); // convert from element of CharacterVector to integer
      int yr = (year-minyr);

      // Loop over fleets to process each variable
      for(int fl = 0; fl < nfleet; fl++){

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

        if(catsCheck != -99){
          
          // If the fleet catches the stock, we need to extract fleet effort and
          // calculate the proportional effort-share across metiers to weight
          // averages
          
          // Extract fleet effort slot
          NumericVector flt_effort = fltS4.slot("effort");
          
          // Extract fleet effort dimensions
          NumericVector flt_effort_Dims = flt_effort.attr("dim");
          
          // Generate object to store proportional effort-share
          NumericVector effshare_flMet(flt_effort_Dims[4]);
          
          // Loop over each metier and extract the effort
          for (int mt = 0; mt < flt_effort_Dims[4]; mt++) {
            int idx = getIdx_flq(flt_effort_Dims, 0, yr, 0, 0, mt, it);
            effshare_flMet[mt] = flt_effort[idx];
          }
          
          // Calculate proportional effortshare
          effshare_flMet = effshare_flMet/sum(effshare_flMet);
          
          // Next, I need to extract the catches themselves. I need the landings
          // and discards numbers and weights, and the selectivity.

          // Extract data for stock catches
          List catsList = fltS4.slot(".Data");

          // Extract stock of interest
          S4 catS4 = catsList[catsCheck];

          // Extract slots of interest
          NumericVector cat_lw = catS4.slot("landings.wt");
          NumericVector cat_dw = catS4.slot("discards.wt");
          NumericVector cat_lf = catS4.slot("landings.n");
          NumericVector cat_df = catS4.slot("discards.n");
          NumericVector cat_sl = catS4.slot("catch.sel");
          NumericVector cat_cq = catS4.slot("catch.q");
          
          // Extract catch dimensions
          NumericVector cat_Dims    = cat_lf.attr("dim");
          NumericVector cat_cq_Dims = cat_cq.attr("dim");

          // --------------------------
          // HERE I'M ASSUMING THAT THE AGE DIMENSION IS THE SAME IN THE CATCH
          // AND THE STOCK OBJECTS --- THIS SHOULD BE TRUE 99% OF THE TIME
          // --------------------------

          // Rprintf("... calculated year index \n");

          // Loop over each age of interest
          for(int a = 0; a < stk_n_Dims[0]; a++){
            
            // generate temporary objects to store metier-resolved values for subsequent averaging
            NumericVector cat_lw_stMet(cat_Dims[4]);
            NumericVector cat_dw_stMet(cat_Dims[4]);
            NumericVector cat_lf_stMet(cat_Dims[4]);
            NumericVector cat_df_stMet(cat_Dims[4]);
            NumericVector cat_sl_stMet(cat_Dims[4]);
            NumericVector cat_cn_stMet(cat_Dims[4]);
            NumericVector cat_cq_stMet(cat_Dims[4]);
            
            // generate temporary object to store weighting for subsequent averaging
            NumericVector cat_wt_lw(cat_Dims[4]);
            NumericVector cat_wt_dw(cat_Dims[4]);
            NumericVector cat_wt_lf(cat_Dims[4]);
            NumericVector cat_wt_sl(cat_Dims[4]);

            // Loop over each area (metier)
            for ( int mt = 0; mt < cat_Dims[4]; mt++) {
            
            // Generate index for element of interest
            int idx = getIdx_flq(stk_n_Dims, a, yr, 0, 0, mt, it);
              
              int idx_cq;
              if (cat_cq_Dims.size() > 3) {
                idx_cq = getIdx_4D(cat_cq_Dims, 0, yr, mt, it);
              } else {
                idx_cq = getIdx_3D(cat_cq_Dims, 0, yr, it);
              }
              
              // Insert value into vector
              cat_lw_stMet(mt) = cat_lw[idx];
              cat_dw_stMet(mt) = cat_dw[idx];
              cat_lf_stMet(mt) = cat_lf[idx];
              cat_df_stMet(mt) = cat_df[idx];
              cat_sl_stMet(mt) = cat_sl[idx];
              cat_cn_stMet(mt) = cat_lf[idx] + cat_df[idx];
              cat_cq_stMet(mt) = cat_cq[idx_cq];
              
              // if weights are NA, use zero
              if(NumericVector::is_na(cat_lw[idx])) {
                cat_lw_stMet(mt) = 0;
              }
              if(NumericVector::is_na(cat_dw[idx])) {
                cat_dw_stMet(mt) = 0;
              }
              
              // if landings fraction are NA, use zero
              if(NumericVector::is_na(cat_lf[idx])) {
                cat_lf_stMet(mt) = 0;
              }
              if(NumericVector::is_na(cat_df[idx])) {
                cat_df_stMet(mt) = 0;
              }
              
              // if catchability is NA, use zero
              if(NumericVector::is_na(cat_cq[idx_cq])) {
                cat_cq_stMet(mt) = 0;
              }
              
              // If total catch fraction is zero, use 1 
              if(cat_cn_stMet[mt] == 0) {
                cat_cn_stMet(mt) = 1;
              }
              
              // Landings and discards weights are a weighted-average over metiers
              // where the weighting is:
              //
              // (lf(i)*efs(i)*q(i)*sel(i))/sum(lf(i)*efs(i)*q(i)*sel(i))
              //
              // where:
              // lf(i)  = landings fraction in i'th metier
              // efs(i) = effortshare in i'th metier
              // q(i)   = catchability in i'th metier
              // sel(i) = selectivity in i'th metier
              
              cat_wt_lw(mt) = cat_lf_stMet(mt) * effshare_flMet(mt) * cat_cq_stMet(mt) * cat_sl_stMet(mt);
              cat_wt_dw(mt) = cat_df_stMet(mt) * effshare_flMet(mt) * cat_cq_stMet(mt) * cat_sl_stMet(mt);
              
              // Selectivity at age is weighted by:
              //
              // efs(i)*q(i)/sum(efs(i)*q(i))
              
              cat_wt_sl(mt) = effshare_flMet(mt) * cat_cq_stMet(mt);
              
              // Landings fraction at age is weighted by:
              // 
              // efs(i)*q(i)*sel(i))/sum(efs(i)*q(i)*sel(i))
              
              cat_wt_lf(mt) = effshare_flMet(mt) * cat_cq_stMet(mt) * cat_sl_stMet(mt);
              
            } // END loop over area (metier)
            
            // Sum weightings over metier
            double cat_wt_lw_tot = sum(cat_wt_lw);
            double cat_wt_dw_tot = sum(cat_wt_dw);
            double cat_wt_sl_tot = sum(cat_wt_sl);
            double cat_wt_lf_tot = sum(cat_wt_lf);
            
            // if summed weightings are 0, use 1
            if(cat_wt_lw_tot == 0) {cat_wt_lw_tot = 1;}
            if(cat_wt_dw_tot == 0) {cat_wt_dw_tot = 1;}
            if(cat_wt_sl_tot == 0) {cat_wt_sl_tot = 1;}
            if(cat_wt_lf_tot == 0) {cat_wt_lf_tot = 1;}
            
            // Calculate Weightings
            NumericVector cat_lw_weight = cat_wt_lw/cat_wt_lw_tot;
            NumericVector cat_dw_weight = cat_wt_dw/cat_wt_dw_tot;
            NumericVector cat_sl_weight = cat_wt_sl/cat_wt_sl_tot;
            NumericVector cat_lf_weight = cat_wt_lf/cat_wt_lf_tot;
            
            // Insert value into matrix
            lw_stAgeFlt(a,fl) = sum(cat_lw_stMet * cat_lw_weight);
            dw_stAgeFlt(a,fl) = sum(cat_dw_stMet * cat_dw_weight);
            
            // So the next step is to get the mean landings fraction across 
            // metiers.
            
            // renormalise to catch instances of != 1 total catch fraction
            cat_lf_stMet = cat_lf_stMet/cat_cn_stMet;
            
            // weighted averages
            lf_stAgeFlt(a,fl) = sum(cat_lf_stMet * cat_lf_weight);
            sl_stAgeFlt(a,fl) = sum(cat_sl_stMet * cat_sl_weight);
            
          } // END loop over ages
        } // END if stock is caught
      } // END loop over fleets

      // Rprintf("... extracted value at age-fleet \n");

      // add names to matrix
      rownames(lw_stAgeFlt) = dimnameAges;
      rownames(dw_stAgeFlt) = dimnameAges;
      rownames(lf_stAgeFlt) = dimnameAges;
      rownames(sl_stAgeFlt) = dimnameAges;

      colnames(lw_stAgeFlt) = fltsNames;
      colnames(dw_stAgeFlt) = fltsNames;
      colnames(lf_stAgeFlt) = fltsNames;
      colnames(sl_stAgeFlt) = fltsNames;

      // Insert result into n_age object
      lw_age[st] = lw_stAgeFlt;
      dw_age[st] = dw_stAgeFlt;
      lf_age[st] = lf_stAgeFlt;
      sl_age[st] = sl_stAgeFlt;

    }

    // -----------------------------------------------------------------------
    // Process: catchability by fleet                  (matrix: stock x fleet)
    //          quota-share by fleet                   (matrix: stock x fleet)
    // -----------------------------------------------------------------------

    // Generate matrix of age x fleet for each variable
    NumericMatrix cq_stFlt(nstock, nfleet);
    NumericMatrix qs_stFlt(nstock, nfleet);

    // Rprintf("processing catchability and quota \n");

    // Loop over stocks  - extract dimensions and generate results object
    // Loop over fleets  - check if stock is captured by fleet
    //                   - if yes, extract catches and process
    for(int st = 0; st < nstock; st++){

      // ------------------------
      // I'M ASSUMING THAT ADVICE WITH MULTIPLE ITERATIONS WILL BE A VECTOR
      // ------------------------

      // extract advice for stock of interest
      NumericVector adv_st = advice[st];

      for(int fl = 0; fl < nfleet; fl++){

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

        if(catsCheck != -99){

          // Extract data for stock catches
          List catsList = fltS4.slot(".Data");

          // Extract stock of interest
          S4 catS4 = catsList[catsCheck];

          // Extract slots of interest
          NumericVector cat_cq = catS4.slot("catch.q");
          NumericVector cat_qs = catS4.attr("quotashare");
          
          // Extract effort slot for weighted averaging and 
          // generate object to store proportional effort-share
          NumericVector flt_effort = fltS4.slot("effort");
          NumericVector flt_effort_Dims = flt_effort.attr("dim");
          NumericVector effshare_flMet(flt_effort_Dims[4]);

          // Extract dimensions for quotashare
          List cat_qs_Dimnames = cat_qs.attr("dimnames");
          CharacterVector dimnameYear   = cat_qs_Dimnames["year"];   // year

          // Extract dimensions sizes
          NumericVector cat_cq_Dims = cat_cq.attr("dim");
          NumericVector cat_qs_Dims = cat_qs.attr("dim");
          
          // Prepare object to store metier-resolved catchability
          NumericVector cq_flMet(flt_effort_Dims[4]);

          // create index for year of interest
          int minyr = atoi(dimnameYear[0]); // convert from element of CharacterVector to integer
          int yr = (year-minyr);
          
          // --------------------------
          // Process catchability
          // --------------------------
          
          // Loop over each metier
          for (int mt = 0; mt < flt_effort_Dims[4]; mt++) {
            
            // Generate index for effort
            int idx = getIdx_flq(flt_effort_Dims, 0, yr, 0, 0, mt, it);
            
            // Generate index for catch.q
            int idx_cq;
            if (cat_cq_Dims.size() > 3) {
              idx_cq = getIdx_4D(cat_cq_Dims, 0, yr, mt, it);
            } else {
              idx_cq = getIdx_3D(cat_cq_Dims, 0, yr, it);
            }
            
            // Throw an error if catch-q is NA
            if(R_IsNA(cat_cq[idx_cq])) {
              Rcout << "Fleet: " << fltsNames[fl] << "; Catch: " << catsNames[catsCheck] << "\n";
              stop("catchq is NA");
            }
            
            // Insert value into vector
            effshare_flMet[mt] = flt_effort[idx];
            cq_flMet[mt] = cat_cq[idx_cq];
          }
          
          // Calculate proportional effortshare
          effshare_flMet = effshare_flMet/sum(effshare_flMet);
          
          // Insert value into matrix
          cq_stFlt(st,fl) = sum(cq_flMet * effshare_flMet);
          
          // --------------------------
          // Process quotashare
          // --------------------------

          // Generate index for quotashare
          int idx_qs = getIdx_flq(cat_qs_Dims, 0, yr, 0, 0, 0, it);
          
          // Throw an error if quota-share is NA (I need better error handling!)
          if(R_IsNA(cat_qs[idx_qs])) {
            Rcout << "Fleet: " << fltsNames[fl] << "; Catch: " << catsNames[catsCheck] << "\n";
            stop("quotashare is NA");
          }

          // Insert value into matrix
          qs_stFlt(st,fl) = cat_qs[idx_qs] * adv_st[it];

        } // END if fleet catches stock
      } // END loop over fleets
    } // END loop over stocks

    // add names to matrix
    rownames(cq_stFlt) = stksNames;
    rownames(qs_stFlt) = stksNames;

    colnames(cq_stFlt) = fltsNames;
    colnames(qs_stFlt) = fltsNames;
    
    // -----------------------------------------------------------------------
    // Process: effort by fleet                  (vector: fleet)
    // -----------------------------------------------------------------------
    
    // Generate vector to store fleet efforts 
    NumericVector ef_flt(nfleet);
    
    // Loop over each fleet
    for(int fl = 0; fl < nfleet; fl++){
      
      // Extract data from fleet of interest
      S4 fltS4 = fltsList[fl];
      
      // Extract effort slot
      NumericVector flt_effort = fltS4.slot("effort");
      
      // Extract dimensions for effort
      List flt_eff_Dimnames = flt_effort.attr("dimnames");
      CharacterVector dimnameYear   = flt_eff_Dimnames["year"];   // year
      NumericVector flt_eff_Dims = flt_effort.attr("dim");
      
      // create numeric vector to hold yearly efforts
      NumericVector flt_eff_yrs(avgE_nyear);
      
      // create index for year of interest (previous year)
      int minyr = atoi(dimnameYear[0]); // convert from element of CharacterVector to integer
      int yr = (year-minyr);
      
      // Loop over each reference year for average effort
      for (int refyr = 0; refyr < avgE_nyear; refyr++) {
        
        // Temporary object to store metier-resolved effort
        NumericVector eff_flMet(flt_eff_Dims[4]);
        
        // Loop over each metier
        for (int mt = 0; mt < flt_eff_Dims[4]; mt++) {
          
          // Generate index for fleet effort
          int idx_eff = getIdx_flq(flt_eff_Dims, 0, (yr-1-refyr), 0, 0, mt, it);
          eff_flMet[mt] = flt_effort[idx_eff];
        } // END loop over metier
        
        // Insert efforts
        flt_eff_yrs[refyr] = sum(eff_flMet);
        
      } // END loop over reference year
      
      // Calculate mean effort
      ef_flt[fl] = mean(flt_eff_yrs);
      
    } // END loop over fleets
    
    // -----------------------------------------------------------------------
    // Combine results into a single list
    // -----------------------------------------------------------------------

    // add names to list objects
    n_age.names() = stksNames;
    m_age.names() = stksNames;
    lw_age.names() = stksNames;
    dw_age.names() = stksNames;
    lf_age.names() = stksNames;
    sl_age.names() = stksNames;
    ef_flt.names() = fltsNames;

    // insert
    omList[it] = List::create(_["n"] = n_age,
                              _["m"] = m_age,
                              _["landwt"] = lw_age,
                              _["discwt"] = dw_age,
                              _["landfrac"] = lf_age,
                              _["catchsel"] = sl_age,
                              _["catchq"]   = cq_stFlt,
                              _["quota"]    = qs_stFlt,
                              _["effort"]   = ef_flt);
  }
  return(omList);
}
