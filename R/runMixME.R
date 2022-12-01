# ---
# title: 'Main function to run MIME'
# author: 'Matthew Pace'
# date: 'August 2022'
# ---
#
#' Run mixed fishery management strategy simulation
#'
#' This function carries out a single run for a set of one or more management
#' procedures (MP) to evaluate MP performance given mixed fishery technical
#' interactions. Function uses a conditioned operating model (OM) to simulate
#' true system dynamics, whereas the observation error model (OEM), MP and
#' additional arguments drive the imperfect evaluation of stock status and the
#' generation of management advice.
#'
#' @param om operating model (OM)
#' @param oem observation error model (OEM)
#' @param mp management procedure (MP)
#' @param args named list. Additional MSE simulation arguments.
#'
#' @return A named list containing the projected operating model and a tracking
#'         object containing summary metrics for observed, estimated and true
#'         stock and fishery properties and simulation performance statistics.
#'
#' @export

runMixME <- function(om,
                     oem,
                     ctrl_obj,
                     args,
                     ...) {

  # ===========================================================================#
  # Run check on inputs
  # ===========================================================================#

  ## om must contain "stks" and "flts"

  ## stock names in "stks", "flts" and advice objects must match

  ## args must contain "adviceType"

  ## Check that there are no NAs in critical slots

  ## Define discarding options if not already specified
  overquotaDiscarding <- TRUE
  sizeselectDiscarding <- TRUE

  # ===========================================================================#
  # Set up objects
  # ===========================================================================#

  ## If FLStocks are provided, convert FLStocks into FLBiols
  if(class(om$stks) == "FLStocks") {
    om$stks <- FLCore::FLBiols(lapply(om$stks@names,
                              function(x) {
                                biol <- as(om$stks[[x]],"FLBiol")

                                biol@rec@params <- sr_list[[x]]@params
                                biol@rec@model  <- sr_list[[x]]@model
                                biol@rec$rec    <- NULL
                                return(biol)
                              }))
  }

  ## define projection years
  projyrs <- (args$iy):(args$fy-1)

  ## Generate tracker for optimisation and warnings
  tracking <- makeTracking(om = om, projyrs = projyrs)

  # ===========================================================================#
  # Run mp
  # ===========================================================================#

  ## Run simulation loop
  for(yr in projyrs) {

    ## Print current year
    cat("year: ",yr,"\n")

    ## Update current (assessment) year in args
    args$ay <- yr

    # -------------------------------------------------------------------------#
    # Observation Error Module
    # -------------------------------------------------------------------------#

    ## if not available, generate null deviances in observation error model
    if(length(deviances(oem)$stk) == 0)
      deviances(oem)$stk <- rep(list(NULL), length(observations(oem)$stk))

    ## Extract arguments
    ctrl.oem        <- mse::args(oem)
    ctrl.oem$om     <- om
    ctrl.oem$args   <- args
    ctrl.oem$observations <- mse::observations(oem)
    ctrl.oem$deviances    <- mse::deviances(oem)
    ctrl.oem$tracking     <- tracking

    ## Apply observation error model to each stock
    out <- do.call("oemMixME", ctrl.oem)

    # I'M CURRENTLY FORCING THE OBSERVED STOCKS TO BE FLSTOCKS... I PROBABLY WANT
    # TO ALLOW FOR FLBIOLS AND FLFLEETS TOO...

    ## Extract results
    stk0     <- out$stk
    flt0     <- out$flt
    idx0     <- out$idx
    tracking <- out$tracking

    ## observations(oem) <- lapply(out, "[[", "observations")
    observations(oem) <- out$observations

    # -------------------------------------------------------------------------#
    # Stock Estimation Module
    # -------------------------------------------------------------------------#

    ## Extract arguments
    ctrl.est          <- mse::args(ctrl_obj$est)
    ctrl.est$stk      <- stk0
    ctrl.est$idx      <- idx0
    ctrl.est$args     <- args
    ctrl.est$tracking <- tracking

    ## Add OM data if perfect observation required
    if(any(ctrl.est$estmethod == "perfectObs")) {
      ctrl.est$om <- om
    }

    ## Run the estimation module
    out      <- do.call("estMixME", ctrl.est)

    ## Extract results
    stk0 <- out$stk
    flt0 <- out$flt
    sr0  <- out$sr

    # ctrl     <- out$ctrl
    tracking <- out$tracking

    # -------------------------------------------------------------------------#
    # Harvest Control Rule Module
    # -------------------------------------------------------------------------#

    ## if exists...
    if(!is.null(ctrl_obj$phcr)){

      ## Set up inputs to parameterise harvest control rule
      ctrl.phcr          <- mse::args(ctrl_obj$phcr)
      ctrl.phcr$stk      <- stk0
      ctrl.phcr$args     <- args
      ctrl.phcr$tracking <- tracking

      ## Run pHCR module
      out      <- do.call("phcrMixME", ctrl.phcr)
      hcrpars  <- out$hcrpars

    }

    ## Set up inputs to HCR
    ctrl.hcr          <- mse::args(ctrl_obj$hcr)
    ctrl.hcr$stk      <- stk0
    ctrl.hcr$args     <- args
    ctrl.hcr$tracking <- tracking

    if(exists("hcrpars")){
      ctrl.hcr$hcrpars <- hcrpars
    }

    ## Run HCR module
    out      <- do.call("hcrMixME", ctrl.hcr)
    ctrl     <- out$ctrl
    tracking <- out$tracking

    # -------------------------------------------------------------------------#
    # Implementation System
    # -------------------------------------------------------------------------#

    ## Set up inputs to implementation system
    ctrl.is          <- mse::args(ctrl_obj$isys)
    ctrl.is$ctrl     <- ctrl
    ctrl.is$stk      <- stk0
    ctrl.is$sr       <- sr0
    ctrl.is$args     <- args
    ctrl.is$tracking <- tracking

    ## Run implementation system
    out      <- do.call("isysMixME", ctrl.is)
    ctrl     <- out$ctrl
    tracking <- out$tracking

    # -------------------------------------------------------------------------#
    # Forward projection
    # -------------------------------------------------------------------------#

    ## Set up inputs to forward projection module
    ctrl.fwd          <- mse::args(ctrl_obj$fwd)
    ctrl.fwd$om       <- om
    ctrl.fwd$ctrl     <- ctrl
    ctrl.fwd$args     <- args
    ctrl.fwd$tracking <- tracking

    ## Run forward projection
    out      <- do.call("fwdMixME", ctrl.fwd)
    om       <- out$om
    tracking <- out$tracking
  }

  # ===========================================================================#
  # Output results
  # ===========================================================================#

  return(list(om       = om,
              tracking = tracking))
}
