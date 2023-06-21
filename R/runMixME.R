# ---
# title: 'Main function to run MixME'
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
  if(!any(names(om) == "stks") | !any(names(om) == "flts")) 
     stop("'om' must contain stock and fleet data in 'stks' and 'flts' respectively")

  ## stock names in "stks", "flts" must match
  if(!all(names(om$stks) %in% unique(unlist(lapply(om$flts, names)))))
    stop("stock names in 'stks' and catches names in 'flts' must match")

  ## args must contain critical elements
  if(!any(names(args) == c("adviceType"))) stop("'adviceType' missing in 'args'.")
  if(!any(names(args) == c("iy"))) stop("Intermediate year 'iy' missing in 'args'.")
  
  if(args$iy > args$fy) stop("Final year 'fy' must be greater than intermediate year 'iy'")
  
  if(is.null(args$management_lag)) args$management_lag <- 1 # default management lag to 1
  if(is.null(args$frq)) args$frq <- 1                       # default advice frequency to 1
  
  if(args$management_lag > 0 & is.null(args$adviceInit)) stop("'adviceInit' missing in 'args'")

  ## Check that there are no NAs in critical slots
  
  ## If management lag or data lag > 0, first projection year must be the 
  ## intermediate year and OM must contain catch.
  
  ## If banking and borrowing is used make sure forecast extends to TACyr+1 
  ## --- do I really want to hard code this procedure?? Maybe better to bundle
  ##     into implementation system?? 

  ## Define discarding options if not already specified -- PROBABLY DELETE (specify in fwd)
  # overquotaDiscarding <- TRUE
  # sizeselectDiscarding <- TRUE
  
  ## Check parallelisation
  if (is.null(args$parallel)) args$parallel <- FALSE
  if (args$parallel == TRUE) {
    if (foreach::getDoParRegistered()) { # if parallel env already set-up
      args$nworkers <- foreach::getDoParWorkers()
    } else if (is.null(args$nworkers)) {  # if parallel env not set up - no workers specified
      
      args$nworkers <- parallel::detectCores() - 2
      cl <- makeCluster(args$nworkers)
      registerDoParallel(cl)
      
    } else if (args$nworkers > 1) {       # if parallel env not set up - workers specified
      
      cl <- makeCluster(args$nworkers)
      registerDoParallel(cl)
      
    } else {
      warning("'parallel' is TRUE but only 1 worker specified")
    }
  } # END if parallel = TRUE

  # ===========================================================================#
  # Set up objects
  # ===========================================================================#

  ## If FLStocks are provided, convert FLStocks into FLBiols
  if (class(om$stks) == "FLStocks") {
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
  projyrs <- (args$iy):(args$fy - args$management_lag)
  
  ## if stock estimation methods are used, add metrics
  if (is.function(ctrl_obj$est@args$estmethod)) {
    addmetrics <- c("conv.est") # assessment model fit convergence code
  } else {
    addmetrics <- NULL
  }

  ## Generate tracker for optimisation and warnings
  tracking <- makeTracking(om = om, projyrs = (args$iy):(args$fy), addmetrics = addmetrics)
  
  if(!is.null(args$adviceInit)) {
    for(i in names(om$stks)) {
      tracking[[i]]$advice[,ac(args$iy),] <- args$adviceInit[[i]]
    }
  }

  # ===========================================================================#
  # Run mp
  # ===========================================================================#

  ## Run simulation loop
  for (yr in projyrs) {

    ## Print current year
    cat("year: ",yr,"\n")

    ## Update current (assessment) year in args
    args$ay <- yr
    
    # -------------------------------------------------------------------------#
    # Track OM in initial projection year
    # -------------------------------------------------------------------------#
    # Applies if fishery-stock dynamics are completed for this year (i.e. no
    # management this year).
    
    if (yr == args$iy & args$management_lag > 0) {
      
      tracking <- updateTrackingOM(om = om, tracking = tracking, args = args, yr = yr)
    }

    # -------------------------------------------------------------------------#
    # Observation Error Module
    # -------------------------------------------------------------------------#
    cat("OBSERVATION ERROR MODEL > ")

    ## if not available, generate null deviances in observation error model
    if (length(deviances(oem)$stk) == 0)
      deviances(oem)$stk <- rep(list(NULL), length(observations(oem)$stk))

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
    observations(oem) <- out$observations

    # -------------------------------------------------------------------------#
    # Stock Estimation Module
    # -------------------------------------------------------------------------#
    cat("MP STOCK ESTIMATION > ")

    ## Extract arguments
    ctrl.est          <- mse::args(ctrl_obj$est)
    ctrl.est$stk      <- stk0
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
    cat("MP HCR > ")

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
    cat("MP IMPLEMENTATION SYSTEM > ")

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
    # Forward projection
    # -------------------------------------------------------------------------#
    cat("OPERATING MODEL > ")

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
    
    cat("\n")
  }

  # ===========================================================================#
  # Output results
  # ===========================================================================#
  
  ## if processing in parallel, shut down workers
  # if (foreach::getDoParRegistered()) {
  #   stopCluster(cl)
  #   
  # }
  

  return(list(om       = om,
              tracking = tracking,
              ctrl_obj = ctrl_obj,
              args     = args))
}
