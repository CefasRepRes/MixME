# ---
# title: 'MixME simulation loop'
# author: 'Matthew Pace'
# date: 'September 2023'
# ---
#
#' Implement simulation loop
#'
#' This function carries out the MixME simulation loop, iterating over each 
#' projection year. The function print progress through each year and management
#' procedure module, and returns an updated operating model and tracking object
#'
#' @param om operating model (OM)
#' @param oem observation error model (OEM)
#' @param ctrl_obj management procedure (MP)
#' @param args named list. Additional MSE simulation arguments.
#'
#' @return A named list containing the projected operating model and a tracking
#'         object containing summary metrics for observed, estimated and true
#'         stock and fishery properties and simulation performance statistics.
#'
#' @export

simMixME <- function (om,
                      oem,
                      tracking,
                      ctrl_obj,
                      args) {
  
  ## define projection years
  projyrs <- (args$iy):(args$fy - args$management_lag)
  
  ## some arguments to handle progress messages
  filearg <- ifelse(args$parallel, paste0(paste0(dimnames(om)$iter,collapse = ""),".txt",collapse = ""), "")
  apparg  <- ifelse(args$parallel, TRUE, FALSE)
  
  ## Run simulation loop
  for (yr in projyrs) {
    
    ## Print current year
    if(args$verbose)
      cat("year: ",yr,"\n", file = filearg, append = apparg)
    
    ## Update current (assessment) year in args
    args$ay <- yr
    
    # -------------------------------------------------------------------------#
    # Forward projection (if management lag > 0)
    # -------------------------------------------------------------------------#
    # Some stock assessment models make use of catch or survey indices from the
    # assessment year (data lag = 0). To handle these cases, we need to project
    # the stock based on management advice generated in a previous time-step to
    # get fishing mortality values.
    
    if (args$management_lag > 0) {
      
      if(args$verbose)
        cat("OPERATING MODEL > ", file = filearg, append = apparg)
      
      ## Set up inputs to forward projection module
      ctrl.fwd          <- mse::args(ctrl_obj$fwd)
      ctrl.fwd$om       <- om
      ctrl.fwd$args     <- args
      ctrl.fwd$tracking <- tracking
      
      ## Run forward projection
      out      <- do.call("fwdMixME", ctrl.fwd)
      om       <- out$om
      tracking <- out$tracking
      
    }
    
    # -------------------------------------------------------------------------#
    # Observation Error Module
    # -------------------------------------------------------------------------#
    if (args$verbose)
      cat("OBSERVATION ERROR MODEL > ", file = filearg, append = apparg)
    
    ## if not available, generate null deviances in observation error model
    if (length(mse::deviances(oem)$stk) == 0)
      mse::deviances(oem)$stk <- rep(list(NULL), length(mse::observations(oem)$stk))
    
    ## Extract arguments
    ctrl.oem        <- mse::args(oem)
    ctrl.oem$om     <- om
    ctrl.oem$args   <- args
    ctrl.oem$observations <- mse::observations(oem)
    ctrl.oem$deviances    <- mse::deviances(oem)
    ctrl.oem$tracking     <- tracking
    
    ## Apply observation error model to each stock
    out <- do.call("oemRun", ctrl.oem)
    
    # I'M CURRENTLY FORCING THE OBSERVED STOCKS TO BE FLSTOCKS... I PROBABLY WANT
    # TO ALLOW FOR FLBIOLS AND FLFLEETS TOO...
    
    ## Extract results
    stk0     <- out$stk
    flt0     <- out$flt
    idx0     <- out$idx
    tracking <- out$tracking
    
    ## observations(oem) <- lapply(out, "[[", "observations")
    mse::observations(oem) <- out$observations
    
    # -------------------------------------------------------------------------#
    # Stock Estimation Module
    # -------------------------------------------------------------------------#
    if(args$verbose)
      cat("MP STOCK ESTIMATION > ", file = filearg, append = apparg)
    
    ## Extract arguments
    ctrl.est          <- mse::args(ctrl_obj$est)
    ctrl.est$stk      <- stk0
    ctrl.est$flt      <- flt0
    ctrl.est$idx      <- idx0
    ctrl.est$args     <- args
    ctrl.est$tracking <- tracking
    
    ## Add OM data if perfect observation required
    if (any(ctrl.est$estmethod == "perfectObs")) {
      ctrl.est$om <- om
    }
    
    ## Run the estimation module
    out      <- do.call("estRun", ctrl.est)
    
    ## Extract results
    stk0 <- out$stk
    flt0 <- out$flt
    sr0  <- out$sr
    
    # ctrl     <- out$ctrl
    tracking <- out$tracking
    
    # -------------------------------------------------------------------------#
    # Harvest Control Rule Module
    # -------------------------------------------------------------------------#
    if(args$verbose)
      cat("MP HCR > ", file = filearg, append = apparg)
    
    ## if exists...
    if (!is.null(ctrl_obj$phcr)) {
      
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
    out      <- do.call("hcrRun", ctrl.hcr)
    ctrl     <- out$ctrl
    tracking <- out$tracking
    
    # -------------------------------------------------------------------------#
    # Implementation System
    # -------------------------------------------------------------------------#
    if(args$verbose)
      cat("MP IMPLEMENTATION SYSTEM > ", file = filearg, append = apparg)
    
    ## Set up inputs to implementation system
    ctrl.is          <- mse::args(ctrl_obj$isys)
    ctrl.is$ctrl     <- ctrl
    ctrl.is$stk      <- stk0
    ctrl.is$sr       <- sr0
    ctrl.is$args     <- args
    ctrl.is$tracking <- tracking
    
    ## Run implementation system
    out      <- do.call("isysRun", ctrl.is)
    ctrl     <- out$ctrl
    tracking <- out$tracking
    
    # -------------------------------------------------------------------------#
    # Forward projection (if management lag == 0)
    # -------------------------------------------------------------------------#
    if (args$management_lag == 0) {
      if(args$verbose)
        cat("OPERATING MODEL > ", file = filearg, append = apparg)
      
      ## Set up inputs to forward projection module
      ctrl.fwd          <- mse::args(ctrl_obj$fwd)
      ctrl.fwd$om       <- om
      ctrl.fwd$args     <- args
      ctrl.fwd$tracking <- tracking
      
      ## Run forward projection
      out      <- do.call("fwdMixME", ctrl.fwd)
      om       <- out$om
      tracking <- out$tracking
    }
    
    if(args$verbose)
      cat("\n", file = filearg, append = apparg)
  }
  
  ## return updated operating model and tracking objects
  return(list(om = om,
              tracking = tracking))
}