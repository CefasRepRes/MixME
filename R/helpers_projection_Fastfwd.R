#' Project stock forward one year using a bespoke c++ function
#' 
#' Function uses a bespoke c++ function to carry out a short-term forecast.
#' 
#' @param om Operating model
#' @param tracking Tracking object
#' @param pars Numeric vector. Vector of optimised fleet efforts
#' @param yr Integer. year
#' 
#' @return A named list containing the operating model and tracking object.

projectFastfwd <- function(om, 
                           tracking,
                           pars,
                           yr,
                           sr_residuals = NULL,
                           proc_res = NULL) {
  
  ## Handle unknown population types and recruitment function types
  recType <- sapply(om$stks, function(x) FLCore::SRModelName(x@rec@model))
  popType <- sapply(om$stks, function(x) ifelse(dim(x@n)[1] > 1, 0, 1))
  
  ## Handle Null stock-recruit residuals
  if(is.null(sr_residuals)) {
    sr_residuals <- sapply(om$stks, function(x) {
      xrec <- x@n[1,]
      xrec[] <- 1
      return(xrec)
    },simplify=FALSE, USE.NAMES = TRUE)
  }
  
  ## Carry our fast projection
  om_fwd <- fast_fwd(om   = om,
                     year = yr, 
                     effort = pars,
                     recType = recType,
                     popType = popType,
                     sr_residuals = sr_residuals)
  
  return(list(om       = om_fwd,
              tracking = tracking))
}