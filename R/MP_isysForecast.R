#' Short-term forecast to translate F-based advice to TAC
#'
#' Function runs a short-term forecast for a single stock to translate a target
#' fishing mortality into a catch target.
#'
#' If input stock object is of class \code{FLBiol}, function first coerces to
#' \code{FLStock}. Catch and harvesting properties are taken from either the
#' operating model or observation error model tracking object. A forecast is
#' carried out using \code{fwd} and the catch target for the advice year is
#' extracted.
#'
#' @param stk an object of class \code{FLBiol} or \code{FLStock}
#' @param ctrl a forward control object of class \code{...}
#'             for each stock.
#' @param args list of additional arguments
#' @param tracking a named list of tracking objects to monitor emergent dynamic
#'                 properties. Must contain an \code{FLStock} object named 'stk'
#'                 containing landings, discards, catch and harvest information.
#' @param forecast Logical. Should a forecast be carried out?
#' @param fwd_trgt Character. The short-term forecast target type. Either
#'                 'fsq' or 'TAC'.
#' @param fwd_yrs Integer. The number of years to forecast.
#' @param fwd_yrs_average Integer vector. Index of historic years over which to
#'                        average biological data.
#' @param fwd_yrs_rec_start Integer vector. Index of historic years from which
#'                          sample recruitment.
#' @param fwd_yrs_sel Integer vector. Index of historic years over which to average
#'                    selectivity-at-age.
#' @param fwd_yrs_lf_remove Integer vector. Index of historic years for which
#'                          landings fraction data are overwritten by the landings
#'                          fraction data in the most recent year.
#' @param fwd_splitLD Separate landings and discards numbers in forecast outputs.
#'
#' @return A named list containing a list of forward control objects and the
#'         tracking object.
#'
#' @export

isysForecast <- function(stk,
                         sr,
                         ctrl,
                         args,
                         tracking,
                         forecast = NULL,
                         fwd_trgt = NULL,
                         fwd_yrs  = NULL,
                         fwd_yrs_average   = NULL,
                         fwd_yrs_rec_start = NULL,
                         fwd_yrs_sel       = NULL,
                         fwd_yrs_lf_remove = NULL,
                         fwd_splitLD       = NULL) {
  
  ## extract timings
  ay   <- args$ay             # current (assessment) year
  mlag <- args$management_lag # lag between assessment year and advice year
  
  # convert FLBiol into FLStock - this will not be necessary if this is run
  # within the goFish loop
  
  if(is.FLStock(stk)) {
    
    stk0 <- stk
    sr0  <- sr
    
    ## truncate to min data year
    minyr <- dims(stk0@stock[!is.na(stk0@stock.n)])$minyear # min year where data exists
    stk0  <- window(stk0, start = minyr)
    
    ## If forward control projects beyond available data, extend stock object
    if(max(ctrl@target[,"year"]) > max(dimnames(stk0)$year)) {
      stk0 <- FLasher::stf(stk0, max(ctrl@target[,"year"]) > max(dimnames(stk0)$year))
    }
    
  } else if(class(stk) == "FLBiol") {
    
    # NOTE: WE NEED TO BE ABLE TO HANDLE FORECASTS USING FLBIOLS AND FLFISHERIES
    #       RATHER THAN COERCING INTO AN FLSTOCK
    
    # stop("Forecasts using FLBiols and FLFisheries not yet supported!")
    
    ## coerce to FLStock
    stk0 <- as(stk,"FLStock")
    
    ## populate missing slots with data from stock object in tracking
    units(stk0) <- units(tracking[["stk"]])
    
    ## Fill missing slots
    stk0@landings.n  <- tracking[["stk"]]@landings.n
    stk0@landings.wt <- tracking[["stk"]]@landings.wt
    stk0@discards.n  <- tracking[["stk"]]@discards.n
    stk0@discards.wt <- tracking[["stk"]]@discards.wt
    stk0@catch.n  <- tracking[["stk"]]@catch.n
    stk0@catch.wt <- tracking[["stk"]]@catch.wt
    stk0@harvest  <- tracking[["stk"]]@harvest
    
    ## update Fbar range
    range(stk0)["minfbar"] <- range(tracking[["stk"]])["minfbar"]
    range(stk0)["maxfbar"] <- range(tracking[["stk"]])["maxfbar"]
    
    ## compute properties
    stock(stk0)    <- computeStock(stk0)
    landings(stk0) <- computeLandings(stk0)
    discards(stk0) <- computeDiscards(stk0)
    catch(stk0)    <- computeCatch(stk0)
    
    ## truncate to min data year
    minyr <- dims(stk0@stock[!is.na(stk0@stock)])$minyear # min year where data exists
    stk0  <- window(stk0, start = minyr)
    
    ## Generate SR of correct dimensions
    sr0        <- as.FLSR(stk0, model = stk@rec@model)
    sr0@rec    <- rec(stk0)
    sr0@ssb    <- ssb(stk0)
    sr0@params <- stk@rec@params
    
  } else {
    
    stop("stock object class must be either FLStock or FLBiol")
  }
  
  # This applies if intermediate year assumption is TAC-based
  #
  # If current (intermediate) year is the first year of simulation
  # then use existing (real) advice for forecast.
  # Otherwise, use the previous year's TAC advice
  
  # if(ay == args$iy) {
  #
  # }
  
  ## FLAsher::fwd to get catch target
  stk_fwd <- FLasher::fwd(stk0, sr = sr0, control = ctrl)
  
  ## Extract catch target
  TAC <- c(catch(stk_fwd)[,ac(ay+mlag)])
  
  ## Construct fwd control object
  ctrl0 <- fwdControl(list(year = ay + mlag, quant = "catch", value = TAC))
  
  return(list(ctrl     = ctrl0,
              tracking = tracking))
}