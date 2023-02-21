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

isysMixME <- function(x,
                      stk,
                      sr = NULL,
                      ctrl,
                      args,
                      isysmethod = NULL,
                      tracking,
                      isysList = NULL){
  
  # ---------------------------------------------------------------------------#
  # 2. Iterative advice implementation (by stock)
  # ... (Option 1) Apply user-supplied advice implementation method
  # ... (Option 2) If catch-based, return TAC advice
  # ... (Option 3) If F-based, built-in short-term forecast to output TAC advice
  # ---------------------------------------------------------------------------#
  
  ## extract timings
  ay   <- args$ay             # current (assessment) year
  mlag <- args$management_lag # lag between assessment year and advice year
  
  ## CHECK THAT ADVICE IS NOT NA!!
  
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
                          fwd_trgt          = isysList[[x]]$fwd_trgt,
                          fwd_yrs           = isysList[[x]]$fwd_yrs,
                          fwd_yrs_average   = isysList[[x]]$fwd_yrs_average,
                          fwd_yrs_rec_start = isysList[[x]]$fwd_yrs_rec_start,
                          fwd_yrs_sel       = isysList[[x]]$fwd_yrs_sel,
                          fwd_yrs_lf_remove = isysList[[x]]$fwd_yrs_lf_remove,
                          fwd_splitLD       = isysList[[x]]$fwd_splitLD))
      ctrl0  <- out$ctrl
      track0 <- out$tracking
      
    } else {
      stop("'isysmethod' must be a function")
    }
    
  } else {
    
    # If no implementation system method is provided by the user,
    # then assume that implemented advice must be in the form of TAC.
    # Hence, if advice is currently f-based, run a short-term forecast
    # to convert advice into a catch target
    
    # I need to allow for different management lags, including zero lag, and
    # intermediate year assumptions
    
    # ------------------------------------------------------------#
    # (Option 2) If catch-based, return TAC advice
    # ------------------------------------------------------------#
    
    ## If advice is catch-based, return TAC
    if(ctrl[[x]]@target[ctrl[[x]]@target[,"year"] == (ay+mlag),"quant"] == "catch") {
      
      ctrl0  <- ctrl[[x]]
      track0 <- tracking[[x]]
    } else
    
    # ------------------------------------------------------------#
    # (Option 3) If F-based, built-in short-term forecast to output TAC advice
    # ------------------------------------------------------------#
    
    ## If advice is F-based, perform a short-term forecast to get TAC
    if(ctrl[[x]]@target[ctrl[[x]]@target[,"year"] == (ay+mlag),"quant"] == "fbar") {
      
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
    
  } # END if is null isysMethod
  
  ## add implemented advice to tracking object
  if("value" %in% rownames(out$ctrl@iters[,,])) { # ctrl structure from hcrICES
    out$tracking$advice[1,ac(ay + mlag),] <- out$ctrl@iters[,,]["value",]
  }
  if("value" %in% colnames(out$ctrl@iters[,,])) { # ctrl structure from fixedF
    out$tracking$advice[1,ac(ay + mlag),] <- out$ctrl@iters[,,][,"value"][1]
  }
  
  return(list(ctrl     = ctrl0,
              tracking = track0))

}


