# ---
# title: 'Functions to help quality check simulation performance'
# author: Matthew Pace
# date: 'January 2026'
# ---
#
#' Check whether the correct choke effort has been identified for each fleet
#' 
#' This function checks whether there is residual under- or over-shoot of quota
#' in a 'min' or 'max' simulation and returns a list of the fleets for optimised 
#' effort does not correspond with expected quota uptake. If residuals fall 
#' within tolerance limits, representing correct uptake, `TRUE` is returned. 
#' 
#' In a 'min' simulation, residual quota for at least one stock should be zero
#' and an undershoot of all other stocks. Conversely, a 'max'
#' scenario should result in zero residual quota for at least one stock and 
#' and overshoot of all other stocks.
#' 
#' @param object List. Complete outputs from a `MixME` simulations, comprising
#'               an operating model (`om`), management procedure (`ctrl_obj`),
#'               and `tracking` 
#' @param yr Numeric. For which simulation year should the quality check be
#'           performed?
#' @param effortType Char. 'min' or 'max'
#' @param sig.fig Integer. Residuals below this threshold are ignored.
#' @param verbose Should summaries be printed to console? Defaults to `TRUE`
#' 
#' @return A dataframe containing year (`yr`), basis for calculating uptake performance,
#' (`metric`), fleet name (`fleet`), residual quota undershoot or overshoot that
#' exceed threshold (`residual`).
#' 
#' @export

## Simple function to check whether the correct choke effort has been identified 
checkRun <- function(object, yr, effortType = "min", sig.fig = 3, verbose = TRUE) {
  
  ## Get catchability
  q <- stockExceptionsMatrix(object$om, yr, "")
  
  ## remove exceptions
  q = q * object$ctrl_obj$fwd@args$exceptions
  
  ## get uptake and remove non-limiting catches
  uptake <- object$tracking$uptake[,,ac(yr),1]
  uptake[q==0] <- NA
  
  if (effortType == "min") {
    minu = round(apply(uptake, 2, min, na.rm = TRUE),sig.fig)
    maxu = round(apply(uptake, 2, max, na.rm = TRUE),sig.fig)
    
    if(verbose) {
      ## The minimum uptake-quota should be close to zero for each fleet
      cat(paste("minimum undershoot:", 
                all(minu == 0), colnames(uptake)[minu != 0], minu[minu != 0],
                "\n",sep = " "))
      
      ## The maximum uptake-quota  should be greater than zero for each fleet
      cat(paste("maximum undershoot:", 
                all(maxu >= 0), colnames(uptake)[maxu < 0], maxu[maxu < 0],
                "\n",sep = " "))
    }
    
    return(rbind(data.frame(year     = yr,
                            metric   = "minimum undershoot",
                            value    = all(minu == 0),
                            fleet    = if (!all(minu == 0)) colnames(uptake)[minu != 0] else NA,
                            residual = if (!all(minu == 0)) minu[minu != 0] else NA),
                 data.frame(year     = yr,
                            metric   = "maximum undershoot",
                            value    = all(maxu >= 0),
                            fleet    = if (!all(maxu >= 0)) colnames(uptake)[maxu < 0] else NA,
                            residual = if (!all(maxu >= 0)) maxu[maxu < 0] else NA)))
  }
  if (effortType == "max") {
    maxo = round(apply(uptake, 2, min, na.rm = TRUE),sig.fig)
    mino = round(apply(uptake, 2, max, na.rm = TRUE),sig.fig)
    
    if (verbose) {
      ## the maximum overshoot should be less than zero for each fleet
      cat(paste("maximum overshoot:",
                all(maxo <= 0), colnames(uptake)[maxo > 0], maxo[maxo > 0],
                "\n",sep = " "))
      
      
      ## the minimum overshoot should be close to zero for each fleet
      cat(paste("minimum overshoot:",
                all(mino == 0), colnames(uptake)[mino != 0], mino[mino != 0],
                "\n",sep = " "))
    }
    
    return(rbind(data.frame(year     = yr,
                            metric   = "maximum overshoot",
                            value    = all(maxo <= 0),
                            fleet    = if(!all(maxo <= 0)) colnames(uptake)[maxo > 0] else NA, 
                            residual = if(!all(maxo <= 0)) maxo[maxo > 0] else NA),
                 data.frame(year     = yr,
                            metric   = "minimum overshoot",
                            value    = all(mino == 0),
                            fleet    = if (!all(mino == 0)) colnames(uptake)[mino != 0] else NA,
                            residual = if (!all(mino == 0)) mino[mino != 0] else NA)))
  }
}