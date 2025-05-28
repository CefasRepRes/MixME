# ---
# title: 'Operating Model projection methods'
# author: 'Matthew Pace'
# date: 'August 2022'
# ---
#
#
#' Forward projection of stocks - including process error
#'
#' This function takes the output of the implementation error model and
#' projects the stocks and fleets forward one time step.
#'
#' @param om A list of \code{FLBiols} and \code{FLFisheries} containing the relevant
#'           stock and fleet information in the Operating Model.
#' @param args     list of additional arguments
#' @param tracking a named list of tracking objects to monitor emergent dynamic
#'                 properties. Contains catch advice for each stock.
#' @param sr_residuals (Optional) \code{FLQuants} or \code{list}. Multiplicative
#'                     stock recruitment residuals. Defaults to \code{NULL}.
#' @param proc_res (Optional) Character. Where is process error noise stored?
#'                 If \code{NULL}, no process error is applied to stock numbers.
#'                 Defaults to \code{NULL}.
#' @param adviceType Character. The basis of management advice. 
#'                   Can be 'catch' or 'landings'.'f' is not yet possible.
#' @param effortType Character. The basis of effort constraint. Can be 'max', 'min'
#'                   or 'sqE'.
#' @param exceptions Matrix (stock, fleet). Can be 0 or 1. 
#' @param multiplier Matrix (stock, fleet). Defaults to 1.
#' @param nyear      (Optional) Number of reference years for the calculation of
#'                   status quo effort. Defaults to \code{NULL}.
#' @param effort_max (Optional) Integer indicating the maximum allowed
#'                   fishing effort for any fleet. Defaults to 100 and
#'                   cannot be \code{NULL}.
#' @param effsharemethod (Optional) User-supplied function to carry out redistribution
#'                       of effort-share across metiers. Must take \code{om} and
#'                       \code{yr} as minimum inputs.
#' @param effshareargs (Optional) List of additional arguments required by effort-share
#'                     redistribution function.
#'
#' @return A list containing the \code{FLBiols} and \code{FLFisheries} objects
#'         with projected stock numbers, fleet efforts, and fleet-stock landings
#'         and discards numbers.
#'
#' @export

fwdMixME <- function(om,                  # FLBiols/FLFisheries
                     args,                # Additional arguments
                     tracking,            # Tracking object containing advice
                     sr_residuals = NULL, # list or FLQuants of recruitment residuals
                     proc_res     = NULL, # where is process error noise stored?
                     adviceType,
                     effortType,
                     exceptions,
                     multiplier,
                     nyear      = NULL,   # number of years for status quo effort
                     effort_max = 100,    # maximum allowed fishing effort
                     effsharemethod = NULL,
                     effshareargs   = NULL,
                     ...) {

  # CURRENTLY ASSUMES THAT PROJECTION IS A SINGLE YEAR... PROBABLY UNAVOIDABLE
  # BECAUSE WE NEED TO RECALCULATE EFFORT FOR EACH TIMESTEP (EVEN IF ADVICE
  # DOES NOT CHANGE...)
  # NEED TO THINK ABOUT HOW TO HANDLE MULTI-ANNUAL ADVICE...

  # FLasher DOES NOT ALLOW EFFORT TO BE ZERO. I WILL NEED TO EITHER REPLACE
  # FLasher OR BUILD A ROUTINE TO PROJECT ZERO EFFORT CASES.

  # AT THE MOMENT, ADVICETYPE CAN ONLY TAKE A SINGLE VALUE - I WILL NEED TO ADAPT
  # THIS SO THAT DIFFERENT STOCKS CAN HAVE LANDINGS, CATCH OR F-BASED ADVICE.

  # ===========================================================================#
  # Extract Arguments
  # ===========================================================================#

  ni   <- dims(om$stks[[1]])$iter
  yr   <- args$ay
  iy   <- args$iy
  mlag <- args$management_lag
  fy   <- args$fy
  
  ## How many times to retry if effort optimization does not identify choke-stocks?
  if(!is.null(args$maxRetry)) {
    maxRetry <- args$maxRetry
  } else {
    maxRetry <- 1
  }
  
  ## use R or c++ to update fleet catches - Default to c++ - DELETE THIS LATER
  if(!is.null(args$testfwd)) {
    testfwd <- args$testfwd
  } else {
    testfwd <- FALSE # use c++
  }
  
  ## use R or TMB to optimise fleet efforts - DELETE THIS LATER
  if(!is.null(args$useTMB)) {
    useTMB <- args$useTMB
  } else {
    useTMB <- TRUE
  }
  
  ## use last year effort as initial parameters? Default = FALSE
  if(!is.null(args$useEffortAsInit)) {
    useEffortAsInit <- args$useEffortAsInit
  } else {
    useEffortAsInit <- FALSE
  }
  
  ## use stock recruitment residuals
  if(!is.null(sr_residuals))
    if(is.list(sr_residuals))
      sr_residuals <- FLQuants(sr_residuals)
  
  ## handle missing reference years for status quo effort
  if (is.null(nyear)) nyear <- 1
  
  # ===========================================================================#
  # Process Advice
  # ===========================================================================#
  
  ## extract advice from tracking object
  advice <- lapply(om$stks@names, function(x) {
    adv_x <- c(tracking[[x]]$advice[1,ac(yr),])
    if (!args$use_fastfwd) {adv_x[adv_x == 0] <- 0.01} # impute small catch target to help convergence
    return(adv_x)
  })
  names(advice) <- om$stks@names
  
  # ===========================================================================#
  # Handle cases of failed advice generation
  # ===========================================================================#
  
  ## index for iterations with missing advice
  adv_missing <- which(!apply(!is.na(do.call(rbind, advice)), MARGIN = c(2), FUN = all))
  
  ## handle cases of missing advice
  if(length(adv_missing) > 0) {
    
    ## impute previous year's true realised target
    for(x in om$stks@names) {
      if(adviceType == "catch")    adv_metric <- "C.om"
      if(adviceType == "landings") adv_metric <- "L.om"
      if(adviceType == "f")        adv_metric <- "F.om"
      advice[[x]][adv_missing] <- c(tracking[[x]]$stk[adv_metric, ac(yr-1), 1, 1, 1, adv_missing])
    }
    
    ## mark iteration(s) as failed
    tracking$iterfail[ac(yr), adv_missing] <- 1
    
  }
  
  ## track previous instances of failed management procedure advice
  if(yr > iy)
    tracking$iterfail[ac(yr), which(tracking$iterfail[ac(yr-1),] > 0)] <- 1
  
  
  # ===========================================================================#
  # Advice implementation given mixed fisheries technical interactions
  # ===========================================================================#

  ## Advice is a TAC per stock
  if(adviceType %in% c("catch","landings")) {
    
    # -------------------------------------------------------------------------#
    # (Optional) Effort-share re-weighting
    # -------------------------------------------------------------------------#
    
    if (!is.null(effsharemethod)) {
      if (is.function(effsharemethod)) {
        om <- do.call(effsharemethod, c(list(om = om, yr = yr),effshareargs))
      }
    }

    # -------------------------------------------------------------------------#
    # Reorganise data for optimisation
    # -------------------------------------------------------------------------#

    ## generate simplified list to pass to optimiser
    omList <- FLBiols2List(om = om,
                           year = yr,
                           advice = advice,
                           useCpp = TRUE,
                           avgE_nyear = ifelse(yr > iy, 1, nyear),
                           process_residuals = proc_res)

    # -------------------------------------------------------------------------#
    # Optimise fleet activity
    # -------------------------------------------------------------------------#
    
    ## if any effortType is status quo effort then update exceptions 
    exceptions[,names(effortType[,as.character(yr)])[which(effortType[,as.character(yr)] == "sqE")]] <- 0
    
    ## condense effortType
    effortType <- unique(effortType[,as.character(yr)])[unique(effortType[,as.character(yr)]) != "sqE"]
    
    ## only run optimisation if at least 1 fleet has dynamic effort
    if(any(colSums(exceptions) > 0)) {
      
      ## optimise effort
      effOptimised <- effortBaranov(omList       = omList,
                                    adviceType   = adviceType,
                                    effortType   = effortType,
                                    exceptions   = exceptions,
                                    multiplier   = multiplier,
                                    maxRetry     = maxRetry,
                                    useEffortAsInit = useEffortAsInit,
                                    useTMB       = useTMB,
                                    correctResid = FALSE)
      
      ## Extract effort parameters for each fleet
      pars <- sapply(1:ni, function(x) { effOptimised[[x]]$par})
      
      ## If pars are not a matrix, coerce
      if(!is.matrix(pars)){
        pars <- matrix(pars, nrow = length(om$flts), ncol = ni)
      }
      
    } else {
      ## Extract status quo effort for all fleets
      pars <- sapply(1:ni, function(x) { log(omList[[x]]$effort)})
      
    } # END if any fleets dynamic effort

    # -------------------------------------------------------------------------#
    # TRACKING
    # -------------------------------------------------------------------------#

    ## save quota stock-fleet
    tracking$quota[,,ac(yr),] <- sapply(1:ni, function(y) {
      omList[[y]]$quota
    }, simplify = "array")
    
    if(any(colSums(exceptions) > 0)) {
      
      ## save optimisation results to tracker
      tracking$optim[,ac(yr),] <- sapply(1:ni, function(x){
        c(effOptimised[[x]]$objective,
          effOptimised[[x]]$convergence)
      })
      
      ## save optimisation messages to tracker
      tracking$message[1,ac(yr),] <- sapply(1:ni, function(x){
        effOptimised[[x]]$message[1]
      })
      
      ## save re-scaling messages to tracker
      tracking$message[2,ac(yr),] <- sapply(1:ni, function(x){
        effOptimised[[x]]$message[2]
      })
      
      ## save choke stock vector to tracker
      tracking$choke[,ac(yr),] <- sapply(1:ni, function(x){
        effOptimised[[x]]$stkLim
      })
      
      ## save quota uptake to tracker
      tracking$uptake[,,ac(yr),] <- sapply(1:ni, function(x){
          tracking$quota[,,ac(yr),x] - effOptimised[[x]]$Cfleet
      }, simplify = "array")
      
    } else {
      
      ## save quota uptake to tracker
      tracking$uptake[,,ac(yr),] <- sapply(1:ni, function(x){
        tracking$quota[,,ac(yr),x] - catchBaranov(par = omList[[x]]$effort,
                                                  dat = omList[[x]],
                                                  adviceType = adviceType,
                                                  islog = FALSE)
      }, simplify = "array")
      
      ## save choke stock vector to tracker
      tracking$choke[,ac(yr),] <- NA
      
    }
    
    # -------------------------------------------------------------------------#
    # Do not project if final projection year with zero management lag
    # -------------------------------------------------------------------------#
    #
    # If management lag is zero, the simulation will proceed to the final year, but
    # we don't actually want to project forward past the final year, so end here.
    
    # if(yr == fy & mlag == 0) {
    #   
    #   ## update tracking object
    #   tracking <- updateTrackingOM(om = om, tracking = tracking, args = args, yr = yr)
    #   
    #   ## return unprojected stock
    #   return(list(om       = om,
    #               tracking = tracking))
    #   
    # }

  } else if(adviceType == "f") {

    # Technically, f-based advice is really effort restriction. The challenge is
    # work out how effort translates to F for each stock --- and what the least
    # allowable effort will be.
    #
    # How is f-based advice translated into allowable effort in practice? How is
    # effort allocated to different fleets?
    #
    # Would we expect the proportional activity across fleets to remain
    # constant? --- if so, we simply scale all efforts to meet the limiting F.

    stop("f-based advice not currently implemented")
  } else {
    stop("Advice format (adviceType) must be 'f', 'catch' or 'landings'")
  }
  
  # ===========================================================================#
  # Project forward using fast_fwd
  # ===========================================================================#
  
  if (args$use_fastfwd == TRUE) {
    out <- projectFastfwd(om       = om,
                          tracking = tracking,
                          pars     = exp(pars),
                          yr       = yr,
                          popType  = args$popType,
                          sr_residuals = sr_residuals)
    om       <- out$om
    tracking <- out$tracking
  }
  
  # ===========================================================================#
  # Project forward using FLasher::fwd
  # ===========================================================================#
  
  if (args$use_fastfwd == FALSE) {
    out <- projectFLasher(om           = om, 
                          tracking     = tracking,
                          pars         = pars,
                          yr           = yr,
                          effort_max   = effort_max,
                          sr_residuals = sr_residuals,
                          proc_res     = proc_res,
                          testfwd      = testfwd,
                          adviceType   = adviceType)
    
    om       <- out$om
    tracking <- out$tracking
  }

  # (Optional) Add process error noise if available
  #
  # This is uncertainty around the survival process (natural mortality)
  # Only affects stock numbers

  if(!is.null(proc_res)){
  }

  # ===========================================================================#
  # Update tracking object
  # ===========================================================================#
  
  tracking <- updateTrackingOM(om = om, tracking = tracking, args = args, yr = yr)
  
  # ===========================================================================#
  # Return outputs
  # ===========================================================================#

  ## return projected stock
  return(list(om       = om,
              tracking = tracking))

}


#' Automatic generation of FCB matrix for FLasher
#'
#' This function takes an FLBiols and FLFisheries object and generates an FCB
#' matrix
#' 
#' @export

makeFCB <- function(biols, flts){

  ## number of biols and fleets
  nbiols <- length(biols)
  nflts  <- length(flts)

  ## biols names
  biolnames <- names(biols)

  # Calculate the positions of: Fishery in Fisheries object
  #                             Catches in Fishery object
  #                             Biols caught by Catches object

  nums <- lapply(1:nflts, function(x){

    ## extract catch names from fleet object
    catchnames <- names(flts[[x]])

    ## position of catch name in fleet
    catchpos <- 1:length(catchnames)

    ## position of biol name that matches catch name
    biolspos <- sapply(1:length(catchnames), function(y){
      which(catchnames[y] == biolnames)
    })
    
    ## Warning if any catches don't have equivalent biols positions
    if(is.list(biolspos)) {
      missingbiols <- catchnames[unlist(lapply(biolspos, function(x) length(x) == 0))]
      warning(paste0("in fleet ",
                     names(flts)[x],
                     ", catches do not have biols: ",
                     missingbiols,"\n"))
      
      for(i in which(unlist(lapply(biolspos, function(x) length(x) == 0)))) {
        biolspos[[i]] <- 0
      }
      biolspos <- unlist(biolspos)
    }

    ## A hacky solution to get the numbers in the right order...
    matrix(c(rep(x, length(catchnames)),
               catchpos,
               biolspos),
             ncol = 3, byrow = FALSE)
  })
  
  ## combine into single matrix
  fcb <- do.call(rbind, nums)
  
  ## remove rows with missing biols 
  fcb <- fcb[fcb[,3] > 0,,drop = FALSE]
  
  ## Define row and column names
  dimnames(fcb) <- list(1:nrow(fcb), c("F", "C", "B"))

  return(fcb)
}

