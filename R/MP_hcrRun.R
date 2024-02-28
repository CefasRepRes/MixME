# ---
# title: 'Wrapper for Harvest Control Rule methods'
# author: 'Matthew Pace'
# date: 'December 2022'
# ---
#
#' Wrapper for advice rule methods
#' 
#' ...
#' 
#' @param stk list of stocks
#' @param hcrpars (optional) Harvest Control Rule parameters for each stock. A named
#'                nested list. Each named list element corresponds to a stock
#'                and should contain a list of parameters for that stock.
#'                Defaults to \code{NULL}.
#' @param hcrmethod named list of advice rule methods for each stock.
#'                  List elements may be the name of an in-built advice rule
#'                  method or a user-supplied function that is applied to the
#'                  stock. Defaults to 'hcrICES'.
#' @param ctrg (optional) named list of target catch for each stock.
#' @param ftrg (optional) named list of target fishing mortality for each stock
#' @param tracking a named list of tracking objects to monitor emergent dynamic
#'                 properties.
#'
#' @export

hcrRun <- function(stk,
                   flt,
                   idx,
                   args,
                   hcrpars = NULL,
                   hcrmethod = NULL,
                   ctrg = NULL,
                   ftrg = NULL,
                   tracking) {
  
  ## TO DO - How to apply a multistock harvest control rule? Cannot loop over
  ##         each stock in that case... How to handle a mix of single stock and
  ##         multistock harvest control rules?
  
  ## TO DO - How to handle stocks that are not assessed but are simply by-catch?
  
  ## Extract the methods to be used for each stock
  #hcrmethod <- args$hcrmethod
  
  ctrlList <- lapply(names(stk), 
                     hcrMixME,
                     stk       = stk,
                     flt       = flt,
                     idx       = idx,
                     args      = args,
                     hcrpars   = hcrpars,
                     hcrmethod = hcrmethod,
                     ctrg      = ctrg,
                     ftrg      = ftrg,
                     tracking  = tracking)
  
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