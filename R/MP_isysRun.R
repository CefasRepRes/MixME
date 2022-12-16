# ---
# title: 'Wrapper for implementation system'
# author: 'Matthew Pace'
# date: 'December 2022'
# ---
#
#' Wrapper for advice implementation system
#' 
#' ...
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

isysRun <- function(stk,
                    sr = NULL,
                    ctrl,
                    args,
                    isysmethod = NULL,
                    tracking,
                    isysList = NULL) {
  
  # ----------------------------------------------#
  # 1. Extract global arguments
  # 2. Iterative advice implementation (by stock)
  # 3. Extract control and tracking into outputs
  # ----------------------------------------------#
  
  # NOTE: CURRENTLY ASSUMES THAT ADVICE IS ON A STOCK-BY-STOCK BASIS.
  #       I WANT TO RELAX THIS ASSUMPTION TO HANDLE MULTISTOCK ADVICE
  #       OUTPUTS
  
  # ===================================#
  # SECTION 1: Extract global arguments
  # ===================================#
  
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
  ctrlList <- lapply(stk@names, 
                     isysMixME,
                     stk  = stk,
                     sr   = sr,
                     ctrl = ctrl,
                     args = args,
                     isysmethod = isysmethod,
                     tracking   = tracking,
                     isysList   = isysList)
  
  # ======================================================#
  # SECTION 3: Extract control and tracking into outputs
  # ======================================================#
  
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