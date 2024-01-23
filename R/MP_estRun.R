# ---
# title: 'Management Procedure - Stock estimation methods'
# author: 'Matthew Pace'
# date: 'August 2022'
# ---
#
#' Wrapper for stock estimation methods
#'
#' Function is a wrapper for \code{estMixME}.
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

estRun <- function(stk,
                   flt,
                   idx,
                   ctrl = NULL, ## DO I REALLY NEED THIS OR CAN I GENERATE IN FUNCTION?
                   om   = NULL,
                   args,
                   estmethod = NULL,
                   tracking,
                   fitList = NULL,
                   fwdList = NULL){
  
  # ===================================#
  # SECTION 1: Extract global arguments
  # ===================================#
  
  ## If no methods supplied, then assume perfect observation
  if(is.null(estmethod)) {
    
    estmethod <- sapply(stk@names, function(x) "perfectObs",
                        USE.NAMES = TRUE, simplify = FALSE)
  }
  
  # ======================================#
  # SECTION 2: Iterative stock estimation
  # ======================================#
  
  ## If a list of stocks is given, loop over each stock
  if(is.list(stk)) {
    
    est_list <- lapply(stk@names, 
                       estMixME,
                       stk = stk,
                       flt = flt,
                       idx = idx,
                       ctrl = ctrl,
                       om   = om,
                       args = args,
                       estmethod = estmethod,
                       tracking  = tracking,
                       fitList = fitList,
                       fwdList = fwdList)
    
    ## Add names to list of estimated stocks
    names(est_list) <- stk@names
    
    ## Update tracking object
    for(x in stk@names) {
      tracking[[x]] <- est_list[[x]]$tracking
    }
    
    ## Handle FLStocks and FLBiols
    stkList <- lapply(est_list, "[[", "stk0")
    stk0 <- if(class(stkList[[1]]) == "FLStock") FLStocks(stkList) else FLBiols(stkList)
    flt0 <- lapply(est_list, "[[", "flt0")
    sr0  <- lapply(est_list, "[[", "sr0")
  }
  
  return(list(stk      = stk0,
              flt      = flt0,
              sr       = sr0,
              tracking = tracking))
}