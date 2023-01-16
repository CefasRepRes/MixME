#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List flr_to_list(List om, List advice, int year, int nstock, int nfleet, int niter){

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
      CharacterVector dimnameUnit   = stk_n_Dimnames["unit"];   // unit
      CharacterVector dimnameSeason = stk_n_Dimnames["season"]; // season

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
        int idx =
          (stk_n_Dims[4] * stk_n_Dims[3] * stk_n_Dims[2] * stk_n_Dims[1] * stk_n_Dims[0] * (it)) +
          (stk_n_Dims[3] * stk_n_Dims[2] * stk_n_Dims[1] * stk_n_Dims[0] * (1 - 1)) + // points to area (assumed to be 1)
          (stk_n_Dims[2] * stk_n_Dims[1] * stk_n_Dims[0] * (1 - 1)) + // points to season (assumed to be 1)
          (stk_n_Dims[1] * stk_n_Dims[0] * (1 - 1)) + // points to unit (assumed to be 1)
          (stk_n_Dims[0] * (yr)) +
          (a);

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
      CharacterVector dimnameUnit   = stk_n_Dimnames["unit"];   // unit
      CharacterVector dimnameSeason = stk_n_Dimnames["season"]; // season

      // Extract dimensions sizes
      NumericVector stk_n_Dims = stk_n.attr("dim");

      // Generate matrix of age x fleet for each variable
      NumericMatrix lw_stAgeFlt(stk_n_Dims[0], nfleet);
      NumericMatrix dw_stAgeFlt(stk_n_Dims[0], nfleet);
      NumericMatrix lf_stAgeFlt(stk_n_Dims[0], nfleet);
      NumericMatrix sl_stAgeFlt(stk_n_Dims[0], nfleet);

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

          // Extract data for stock catches
          List catsList = fltS4.slot(".Data");

          // Extract stock of interest
          S4 catS4 = catsList[catsCheck];

          // Extract slots of interest
          NumericVector cat_lw = catS4.slot("landings.wt");
          NumericVector cat_dw = catS4.slot("discards.wt");
          NumericVector cat_lf = catS4.slot("landings.n");
          NumericVector cat_sl = catS4.slot("catch.sel");

          // --------------------------
          // HERE I'M ASSUMING THAT THE AGE DIMENSION IS THE SAME IN THE CATCH
          // AND THE STOCK OBJECTS --- THIS SHOULD BE TRUE 99% OF THE TIME
          // --------------------------

          // create index for year of interest
          int minyr = atoi(dimnameYear[0]); // convert from element of CharacterVector to integer
          int yr = (year-minyr);

          // Rprintf("... calculated year index \n");

          // Loop over each age of interest
          for(int a = 0; a < stk_n_Dims[0]; a++){

            // Generate index for element of interest
            int idx =
              (stk_n_Dims[4] * stk_n_Dims[3] * stk_n_Dims[2] * stk_n_Dims[1] * stk_n_Dims[0] * (it)) +
              (stk_n_Dims[3] * stk_n_Dims[2] * stk_n_Dims[1] * stk_n_Dims[0] * (1 - 1)) + // points to area (assumed to be 1)
              (stk_n_Dims[2] * stk_n_Dims[1] * stk_n_Dims[0] * (1 - 1)) + // points to season (assumed to be 1)
              (stk_n_Dims[1] * stk_n_Dims[0] * (1 - 1)) + // points to unit (assumed to be 1)
              (stk_n_Dims[0] * (yr)) +
              (a);

            // Insert value into matrix
            lw_stAgeFlt(a,fl) = cat_lw[idx];
            dw_stAgeFlt(a,fl) = cat_dw[idx];
            lf_stAgeFlt(a,fl) = cat_lf[idx];
            sl_stAgeFlt(a,fl) = cat_sl[idx];

          }
        }
      }

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

          // Extract dimensions for quotashare
          List cat_qs_Dimnames = cat_qs.attr("dimnames");
          CharacterVector dimnameYear   = cat_qs_Dimnames["year"];   // year

          // Extract dimensions sizes
          NumericVector cat_cq_Dims = cat_cq.attr("dim");
          NumericVector cat_qs_Dims = cat_qs.attr("dim");

          // create index for year of interest
          int minyr = atoi(dimnameYear[0]); // convert from element of CharacterVector to integer
          int yr = (year-minyr);

          // --------------------------
          // HERE I'M ASSUMING THAT THE CATCH.Q IS AN FLPAR WITHOUT YEAR DIMENSION
          // --------------------------

          // Generate index for catch.q
          int idx_cq =
            (cat_cq_Dims[0] * (it)) +
            (0);

          // Generate index for quotashare
          int idx_qs =
            (cat_qs_Dims[4] * cat_qs_Dims[3] * cat_qs_Dims[2] * cat_qs_Dims[1] * cat_qs_Dims[0] * (it)) +
            (cat_qs_Dims[3] * cat_qs_Dims[2] * cat_qs_Dims[1] * cat_qs_Dims[0] * (1 - 1)) + // points to area (assumed to be 1)
            (cat_qs_Dims[2] * cat_qs_Dims[1] * cat_qs_Dims[0] * (1 - 1)) + // points to season (assumed to be 1)
            (cat_qs_Dims[1] * cat_qs_Dims[0] * (1 - 1)) + // points to unit (assumed to be 1)
            (cat_qs_Dims[0] * (yr)) +
            (0);
          
          // Throw an error if quota-share is NA (I need better error handling!)
          if(R_IsNA(cat_qs[idx_qs])) {
            Rcout << "Fleet: " << fltsNames[fl] << "; Catch: " << catsNames[catsCheck] << "\n";
            stop("quotashare is NA");
          }
          
          // Throw an error if catch-q is NA
          if(R_IsNA(cat_cq[idx_cq])) {
            Rcout << "Fleet: " << fltsNames[fl] << "; Catch: " << catsNames[catsCheck] << "\n";
            stop("catchq is NA");
          }

          // Insert value into matrix
          cq_stFlt(st,fl) = cat_cq[idx_cq];
          qs_stFlt(st,fl) = cat_qs[idx_qs] * adv_st[it];

        }
      }
    }

    // add names to matrix
    rownames(cq_stFlt) = stksNames;
    rownames(qs_stFlt) = stksNames;

    colnames(cq_stFlt) = fltsNames;
    colnames(qs_stFlt) = fltsNames;

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

    // insert
    omList[it] = List::create(_["n"] = n_age,
                              _["m"] = m_age,
                              _["landwt"] = lw_age,
                              _["discwt"] = dw_age,
                              _["landfrac"] = lf_age,
                              _["catchsel"] = sl_age,
                              _["catchq"]   = cq_stFlt,
                              _["quota"]    = qs_stFlt);
  }
  return(omList);
}
