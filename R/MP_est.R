# ---
# title: 'Management Procedure - Stock estimation methods'
# author: 'Matthew Pace'
# date: 'August 2022'
# ---
#
#' Implementation of stock estimation methods
#'
#' Function takes a time-series of observed catch or index data
#' (\code{FLStock} and \code{FLIndex} respectively) and estimates current stock
#' status using user-supplied methods. Optionally, if a lag exists between the
#' assessment and management years, a short-term forecast may be carried out to
#' estimate stock status during the management year.
#' 
#' The *fitList* argument is a list that could contain the following elements:
#' \itemize{
#' \item *parallel*: 
#' \item *conf*: 
#' \item *par_ini*: 
#' \item *track_ini*: Logical. Should fitted parameters be used as initial parameter
#'                    values the following year? 
#' }
#'
#' The *fwdList* argument is a list that should contain the following elements:
#' \itemize{
#' \item *forecast*: Logical. Should a forecast be carried out?
#' \item *fwd_trgt*: Character. The short-term forecast target type. Either
#'                   'fsq' or 'TAC'.
#' \item *fwd_yrs*: Integer. The number of years to forecast
#' \item *fwd_yrs_average*: Integer vector. Index of historic years over which to
#'                          average biological data
#' \item *fwd_yrs_rec_start*: Integer vector. Index of historic years from which
#'                            sample recruitment.
#' \item *fwd_yrs_sel*: Integer vector. Index of historic years over which to average
#'                      selectivity-at-age
#' \item *fwd_yrs_lf_remove*: Integer vector. Index of historic years for which
#'                            landings fraction data are overwritten by the landings
#'                            fraction data in the most recent year.
#' \item *fwd_splitLD*: Separate landings and discards numbers in forecast outputs.
#' }
#'
#' @param stk a list of stocks, each of class \code{FLBiol} or \code{FLStock},
#'            or a single stock to be assessed
#' @param idx a list of indices ...
#' @param ctrl a forward control object of class \code{...}
#'             for each stock.
#' @param args list of additional arguments
#' @param estMethod named list of estimation methods for each stock.
#'                  List elements may be the name of an in-built estimation
#'                  method or a user-supplied function that is applied to the
#'                  stock. If \code{NULL}, defaults to 'perfectObs'.
#' @param tracking a named list of tracking objects to monitor emergent dynamic
#'                 properties. Must contain an \code{FLStock} object named 'stk'
#'                 containing landings, discards, catch and harvest information.
#' @param fitList (optional) a named list of length *n* stocks. Each list element is a sub-list
#'                containing the arguments needed to estimate stock status.
#'                Defaults to \code{NULL}.
#' @param fwdList (optional) a named list of length *n* stocks. Each list element is a sub-list
#'                containing the arguments needed to carry out a short-term
#'                forecast for the stock. See 'Details' for required list elements.
#'                Defaults to \code{NULL}.
#'
#' @return Nested named list containing a list of estimated stock objects,
#'         a list of stock-recruitment relationships and the tracking object.
#'
#' @export

estMixME <- function(x,
                     stk,
                     flt,
                     idx,
                     ctrl = NULL, ## DO I REALLY NEED THIS OR CAN I GENERATE IN FUNCTION?
                     om   = NULL,
                     args,
                     estmethod = NULL,
                     estgroup,
                     tracking,
                     fitList = NULL,
                     fwdList = NULL) {
  
  # ------------------------#
  # (Option A) multi-stock estimation
  #
  # ... Apply user-supplied multi-stock estimation method
  # ...... If FLBiol
  # ...... If FLStock
  #
  # (Option B) single stock estimation
  #
  # ... (Option B1) Apply user-supplied single-stock estimation method
  # ...... If FLBiol
  # ...... If FLStock
  #
  # ... (Option B2) Apply perfect stock observation
  # ...... If FLBiol
  # ...... If FLStock
  # ------------------------#
  
  ## extract timings
  iy   <- args$iy             # initial projection year
  ay   <- args$ay             # current (assessment) year
  mlag <- args$management_lag # managment lag
  
  # ---------------------------------------------------------#
  # (Option A) Multi-stock estimation
  # ---------------------------------------------------------#
  
  if (length(estgroup[[x]]) > 1) {
    
    if (is.function(estmethod[[x]])) {
      
      ## if initial parameters are supplied, store in tracking in first year
      if (!is.null(fitList[[x]]$par_ini) & ay == iy){
        tracking[[x]]$par_ini <- fitList[[x]]$par_ini
      }
      
      ## If FLBiols
      if (class(stk[[estgroup[[x]][1]]]) == "FLBiol") {
        stk_est <- do.call(estmethod[[x]],
                           c(list(stk      = stk[estgroup[[x]]],
                                  flt      = flt[estgroup[[x]]],
                                  idx      = idx[estgroup[[x]]],
                                  tracking = tracking[estgroup[[x]]],
                                  args     = args),
                             fitList[[x]],
                             fwdList[[x]]))
      }
      
      ## If FLStock
      if (class(stk[[estgroup[[x]][1]]]) == "FLStock") {
        stk_est <- do.call(estmethod[[x]],
                           c(list(stk      = stk[estgroup[[x]]],
                                  idx      = idx[estgroup[[x]]],
                                  tracking = tracking[estgroup[[x]]],
                                  args     = args),
                             fitList[[x]],
                             fwdList[[x]]))
      }
      
    } else {
      stop("A multi-stock estimation method must be supplied to 'estmethod' as a function.")
    }
  }
  
  # ---------------------------------------------------------#
  # (Option B) Single-stock estimation
  # ---------------------------------------------------------#
  
  if (length(estgroup[[x]]) == 1) {
    
    # ---------------------------------------------------------#
    # (Option B1) Apply user-supplied single-stock estimation method
    # ---------------------------------------------------------#
    
    ## If available, apply user-supplied estimation method
    if (is.function(estmethod[[x]])) {
      
      ## if initial parameters are supplied, store in tracking in first year
      if (!is.null(fitList[[x]]$par_ini) & ay == iy){
        tracking[[x]]$par_ini <- fitList[[x]]$par_ini
      }
      
      ## If FLBiols
      if (class(stk[[x]]) == "FLBiol") {
        stk_est <- do.call(estmethod[[x]],
                           c(list(stk      = stk[[x]],
                                  flt      = flt[[x]],
                                  idx      = idx[[x]],
                                  tracking = tracking[[x]],
                                  args     = args),
                             fitList[[x]],
                             fwdList[[x]]))
      }
      
      ## If FLStock
      if (class(stk[[x]]) == "FLStock") {
        stk_est <- do.call(estmethod[[x]],
                           c(list(stk      = stk[[x]],
                                  idx      = idx[[x]],
                                  tracking = tracking[[x]],
                                  args     = args),
                             fitList[[x]],
                             fwdList[[x]]))
      }
      
    } else if (estmethod[[x]] == "perfectObs") {
      
      # ---------------------------------------------------------#
      # (Option B2) Apply perfect stock observation
      # ---------------------------------------------------------#
      #
      # Alternatively, simply populate OEM stock numbers and recruitment from OM
      
      ## Copy observed stock object
      stk0 <- stk[[x]]
      
      ## Extract data year vector
      yrs_oem <- (range(stk0)["minyear"]):(range(stk0)["maxyear"])
      
      ## If FLBiol
      if (class(stk[[x]]) == "FLBiol") {
        
        ## generate null stock recruitment object
        sr0 <- NULL
        
        out0 <- MP_est_perfectObs_FLBiol(om$stks, om$flts, stk0, flt, x, yrs_oem)
        stk0 <- out0$stk0
        flt0 <- out0$flt0
        
        # ## insert stock numbers
        # FLCore::n(stk0)[,ac(yrs_oem)] <- FLCore::n(om$stks[[x]])[,ac(yrs_oem)]
        # 
        # ## insert stock recruitment data (currently redundant - but good to keep)
        # stk0@rec@params <- om$stks[[x]]@rec@params
        # stk0@rec@model  <- om$stks[[x]]@rec@model
        # 
        # ## extract vector of fleets catching stocks
        # flt0 <- flt[[x]]
        # fltnames <- sapply(flt0, function(xx) x %in% names(xx))
        # 
        # ## insert parameters/variables to calculate fishing mortality
        # for (i in names(flt0)[fltnames]) {
        #   effort(flt0[[i]])[, ac(yrs_oem)]              <- om$flts[[i]]@effort[, ac(yrs_oem)]
        #   catch.q(flt0[[i]][[x]])["alpha", ac(yrs_oem)] <- catch.q(om$flts[[i]][[x]])["alpha", ac(yrs_oem)]
        #   catch.sel(flt0[[i]][[x]])[, ac(yrs_oem)]      <- catch.sel(om$flts[[i]][[x]])[, ac(yrs_oem)]
        # }
        
        ## Calculate fishing mortality at age
        totFage <- getFage(stks = stk0, flts = flt0, stkname = x, yr = ay, use_fastF = args$use_fastF)
        totFbar <- apply(totFage[ac(args$frange[[x]][1]:args$frange[[x]][2]),,,,,,drop = FALSE], c(2:6), mean)
        
        ## Update tracking object
        tracking[[x]]$stk["F.est", ac(ay)]  <- totFbar
        tracking[[x]]$stk["B.est", ac(ay)]  <- FLCore::tsb(stk0)[,ac(ay)]
        tracking[[x]]$stk["SB.est", ac(ay)] <- FLCore::ssb(stk0)[,ac(ay)]
        
        tracking[[x]]$stk["L.est", ac(ay)] <- getCW(flt0,x, "landings", summarise = TRUE)[,ac(ay)]
        tracking[[x]]$stk["D.est", ac(ay)] <- getCW(flt0,x, "landings", summarise = TRUE)[,ac(ay)]
        tracking[[x]]$stk["C.est", ac(ay)] <- tracking[[x]]$stk["L.est", ac(ay)] + tracking[[x]]$stk["D.est", ac(ay)]
        
        tracking[[x]]$sel_est[,ac(ay)] <- sweep(totFage, c(2:6), totFbar, "/")
        
      } # END if FLBiol
      
      ## If FLStock
      if (class(stk[[x]]) == "FLStock") {
        
        ## insert stock numbers and calculate stock biomass
        FLCore::stock.n(stk0)[,ac(yrs_oem)] <- FLCore::n(om$stks[[x]])[,ac(yrs_oem)]
        FLCore::stock(stk0)                 <- FLCore::computeStock(stk0)
        
        ## extract vector of fleets catching stocks
        fltnames <- names(om$flts)[sapply(om$flts, function(ii) any(names(ii) %in% x))]
        
        ## insert fishing mortality
        fltFage <- getFage(stks = om$stks, flts = om$flts, stkname = x, yr = yrs_oem, use_fastF = args$use_fastF)

        FLCore::harvest(stk0)[,ac(yrs_oem)] <- fltFage
        
        # Under perfect stock observations and zero management lag, the final
        # (current) year is also the advice year and no fishing has occurred yet. The
        # harvest data in the stock object for this year is the basis for selectivity
        # understood by the OM.
        
        # If projected values are means of the historical period, there will be
        # slight differences in the resulting values compared to the FLStock
        # generated when conditioning the OM. This is because we are calculated
        # using projected values at the fleet level, whereas the FLStock is simply
        # a mean of historic F.
        
        # This whole process is not needed if we intend to extend the stock
        # for a short-term forecast.
        
        ## Update intermediate year harvest selectivity
        if (mlag == 0) {
          if(ay == iy) {
            FLCore::harvest(stk0)[,ac(ay)] <- 
              FLCore::yearMeans(harvest(stk0)[,ac((ay-3):(ay-1))])
          }
          if (ay > iy) {
            
            FLCore::harvest(stk0)[,ac(ay)] <- FLCore::harvest(stk0)[,ac(ay-1)]
            # harvest(stk0)[,ac(ay)] <- sweep(harvest(stk0)[,ac(ay-1)], 
            #                                 c(2:6), fbar(stk0)[,ac(ay-1)], "/")
            
          }
        }
        
        ## extract stock recruitment information
        # sr0        <- FLCore::as.FLSR(stk0, model = om$stks[[x]]@rec@model) 
        ## throws error if stk0 recruitment time-series is too short
        
        sr0        <- FLCore::FLSR(model = om$stks[[x]]@rec@model)
        sr0        <- FLCore::propagate(sr0, iter = dim(stk0)[6])
        sr0        <- window(sr0, start = dims(stk0)$minyear, end = dims(stk0)$maxyear)
        sr0@rec    <- FLCore::rec(stk0)
        sr0@ssb    <- FLCore::ssb(stk0)
        sr0@params <- om$stks[[x]]@rec@params
        
        ## create a null object for fleets
        flt0 <- NULL
        
        ## estimate final data year
        dy <- dims(stk0)$maxyear
        
        ## Update tracking object
        if (dy %in% dimnames(tracking[[x]]$stk)$year) {
          tracking[[x]]$stk["F.est", ac(dy)]  <- FLCore::fbar(stk0)[,ac(dy)]
          tracking[[x]]$stk["B.est", ac(dy)]  <- FLCore::stock(stk0)[,ac(dy)]
          tracking[[x]]$stk["SB.est", ac(dy)] <- FLCore::ssb(stk0)[,ac(dy)]
          
          tracking[[x]]$stk["C.est", ac(dy)] <- FLCore::catch(stk0)[,ac(dy)]
          tracking[[x]]$stk["L.est", ac(dy)] <- FLCore::landings(stk0)[,ac(dy)]
          tracking[[x]]$stk["D.est", ac(dy)] <- FLCore::discards(stk0)[,ac(dy)]
          
          tracking[[x]]$sel_est[,ac(dy)] <- sweep(FLCore::harvest(stk0)[,ac(dy)], 
                                                  c(2:6), fbar(stk0)[,ac(dy)], "/")
        }
        
      } # END if FLStock
      
      ## Combine outputs into list
      stk_est <- list(stk0 = stk0,
                      flt0 = flt0,
                      sr0  = sr0,
                      tracking = tracking[[x]])
      
    } # END if "perfectObs"
  } # END single-stock estimation 
  
  return(stk_est)
}




