# ---
# title: 'Management Procedure - Implementation system'
# author: 'Matthew Pace'
# date: 'August 2022'
# ---
#
#' Implementation of advice outputs
#'
#' Function takes harvest control rule outputs (in the form of a control object)
#' and applies the implementation system to generate the advice that will drive
#' fleet activity. The implementation system can be any set of formal methods
#' that modify advice (e.g. TAC change limits, banking and borrowing) and/or
#' transform advice targets from catch-based to effort-based (f-based) or vice
#' versa.
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
#' @param stk object of class \code{FLBiols} containing a named \code{FLBiol}
#'            for each stock.
#' @param ctrl named list containing a forward control object of class \code{...}
#'             for each stock.
#' @param args list of additional arguments
#' @param isysmethod named list of implementation system methods for each stock.
#'                   List elements may be the name of an in-built method or a
#'                   user-supplied function that is applied to the
#'                   stock. Defaults to class \code{NULL}.
#' @param tracking a named list of tracking objects to monitor emergent dynamic
#'                 properties
#' @param fwdList a named list of stocks. Each list element is a sub-list
#'                containing the arguments needed to carry out a short-term
#'                forecast for the stock. See 'Details'.
#'
#' @return a named list containing a list of forward control objects for each stock
#'         and a list of tracking objects.
#'
#' @export

isysMIME <- function(stk,
                     sr = NULL,
                     ctrl,
                     args,
                     isysmethod = NULL,
                     tracking,
                     fwdList = NULL){

  # NOTE: CURRENTLY ASSUMES THAT ADVICE IS ON A STOCK-BY-STOCK BASIS.
  #       I WANT TO RELAX THIS ASSUMPTION TO HANDLE MULTISTOCK ADVICE
  #       OUTPUTS

  # ===================================#
  # SECTION 1: Extract global arguments
  # ===================================#

  ## extract timings
  ay   <- args$ay             # current (assessment) year
  mlag <- args$management_lag # lag between assessment year and advice year

  ## if no methods are provided, generate a null list
  if(is.null(isysmethod)) {

    isysmethod <- vector(mode = "list",length = length(stk@names))
    names(isysmethod) <- stk@names

  }

  # ======================================================#
  # SECTION 2: Iterative advice implementation (by stock)
  # ======================================================#

  # At the moment, I'm implementing advice by stock, but it doesn't have to be
  # this way. Ideally advice is implemented by management plan, which can be
  # single stock or multi-stock.

  ## For each stock, implement advice
  ctrlList <- lapply(stk@names, function(x){

    # ------------------------------------------------------------#
    # (Option 1) Apply user-supplied advice implementation method
    # ------------------------------------------------------------#

    # This should be the method that is applied 99% of the time.
    # For each stock

    ## Run user-supplied method (if provided)
    if(!is.null(isysmethod[[x]])) {
      if(is.function(isysmethod[[x]])) {

        out <- do.call(isysmethod[[x]],
                       list(stk      = stk[[x]],
                            ctrl     = ctrl[[x]],
                            args     = args,
                            tracking = tracking[[x]],
                            forecast          = fwdList[[x]]$forecast,
                            fwd_trgt          = fwdList[[x]]$fwd_trgt,
                            fwd_yrs           = fwdList[[x]]$fwd_yrs,
                            fwd_yrs_average   = fwdList[[x]]$fwd_yrs_average,
                            fwd_yrs_rec_start = fwdList[[x]]$fwd_yrs_rec_start,
                            fwd_yrs_sel       = fwdList[[x]]$fwd_yrs_sel,
                            fwd_yrs_lf_remove = fwdList[[x]]$fwd_yrs_lf_remove,
                            fwd_splitLD       = fwdList[[x]]$fwd_splitLD))
        ctrl0  <- out$ctrl
        track0 <- out$tracking

      } else {
        stop("'isysmethod' must be a function")
      }

    } else {

      # ------------------------------------------------------------#
      # (Option 2) Short-term forecast to output TAC advice
      # ------------------------------------------------------------#

      # If no implementation system method is provided by the user,
      # then assume that implemented advice must be in the form of TAC.
      # Hence, if advice is currently f-based, run a short-term forecast
      # to convert advice into a catch target

      # I need to allow for different management lags, including zero lag, and
      # intermediate year assumptions

      ## If advice is catch-based, return TAC
      if(ctrl[[x]]@target[ctrl[[x]]@target[,"year"] == (ay+mlag),"quant"] == "catch") {

        ctrl0  <- ctrl[[x]]
        track0 <- tracking[[x]]

        ## If advice is F-based, perform a short-term forecast to get TAC
      } else if(ctrl[[x]]@target[ctrl[[x]]@target[,"year"] == (ay+mlag),"quant"] == "fbar") {

        ## This carries out a single-stock forecast
        out <- isysForecast(stk      = stk[[x]],
                            sr       = sr[[x]],
                            ctrl     = ctrl[[x]],
                            tracking = tracking[[x]],
                            args     = args)
        ctrl0  <- out$ctrl
        track0 <- out$tracking

      } else {
        stop("Advice must be catch or f")
      }

    }

    return(list(ctrl     = ctrl0,
                tracking = track0))
  })

  ## A bit of a shoddy way of re-organising our returned list
  ctrl <- vector(mode = "list", length = length(stk))
  for(x in 1:length(stk)) {

    ctrl[[x]]     <- ctrlList[[x]]$ctrl
    tracking[[x]] <- ctrlList[[x]]$tracking

  }
  names(ctrl) <- names(stk)

  ## return control
  return(list(ctrl     = ctrl,
              tracking = tracking))

}

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
#' @return
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
    minyr <- dims(stk0@stock[!is.na(stk0@stock)])$minyear # min year where data exists
    stk0  <- window(stk0, start = minyr)

  } else if(class(stk) == "FLBiol") {

    # NOTE: THERE MIGHT BE A MORE EFFICIENT AND ROBUST WAY OF DOING THIS...
    # PERHAPS COERCING TO FLSTOCK SHOULD HAPPEN A LOT EARLIER IN THE PROCESS...

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
