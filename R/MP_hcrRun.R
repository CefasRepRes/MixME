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
                   hcrgroup  = NULL,
                   ctrg = NULL,
                   ftrg = NULL,
                   tracking) {
  
  ## TO DO - How to apply a multistock harvest control rule? Cannot loop over
  ##         each stock in that case... How to handle a mix of single stock and
  ##         multistock harvest control rules?
  
  ## TO DO - How to handle stocks that are not assessed but are simply by-catch?
  
  ## Extract the methods to be used for each stock
  #hcrmethod <- args$hcrmethod
  
  # ===================================#
  # SECTION 1: Handle missing arguments
  # ===================================#
  
  ## If no groups supplied, then assume single-stock estimation
  if(is.null(hcrgroup)) {
    
    hcrgroup <- sapply(stk@names, function(x) x,
                       USE.NAMES = TRUE, simplify = FALSE)
  }
  
  # ======================================#
  # SECTION 2: Iterative processing
  # ======================================#
  
  ctrlList <- lapply(names(hcrgroup), 
                     hcrMixME,
                     stk       = stk,
                     flt       = flt,
                     idx       = idx,
                     args      = args,
                     hcrpars   = hcrpars,
                     hcrmethod = hcrmethod,
                     hcrgroup  = hcrgroup,
                     ctrg      = ctrg,
                     ftrg      = ftrg,
                     tracking  = tracking)
  
  # If multi-stock HCR has been used, then these stocks will be nested
  # within the group of stocks. We want to flatten the list so that 'ctrlList'
  # is a list of stocks with nested ctrl and tracking objects
  
  if (any(sapply(hcrgroup, length) > 1)) {
    
    flatlist <- function(x, hcrgroup) {
      
      ## unpack grouped stocks into a single list of ctrl and tracking containing the stocks
      ## invert to get a list of stocks each containing ctrl and tracking
      tmp <- lapply(which(sapply(hcrgroup, length) > 1), function(y) {
        yy <- lapply(unlist(hcrgroup[y]), function(z) {
          lapply(x[[y]], "[[", z)
        })
        names(yy) <- unlist(hcrgroup[y])
        return(yy)})
      tmp1 <- unlist(tmp, recursive = FALSE)
      
      ## combine ungroups and groups stocks
      tmp <- c(x[sapply(hcrgroup, length) == 1],
               tmp1)
      
      ## assign names
      names(tmp) <- c(unlist(hcrgroup[which(sapply(hcrgroup, length) == 1)]),
                      unlist(hcrgroup[which(sapply(hcrgroup, length) > 1)]))
      tmp <- tmp[unlist(hcrgroup)]
      
      return(tmp)
    }
    
    ## partially flatten list
    ctrlList <- flatlist(ctrlList, hcrgroup)
    
  }
  
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