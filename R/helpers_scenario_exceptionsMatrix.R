# ---
# title: 'Functions to help generate exceptions matrix'
# author: Matthew Pace
# date: 'January 2026'
# ---
#
#' Generate scenario 'exceptions' matrix
#' 
#' The 'exceptions' matrix defines which stocks are effort-limiting for each 
#' fleet. This simple function generate an exceptions matrix of the correct
#' dimensions for a given `MixME` operating model.
#'
#' @param om List. A MixME operating model comprising two elements: 'stks' of
#'           class `FLBiols` and 'flts' of class `FLFIsheries`.
#' @param yr Numeric. The year in the operating model that will be used to
#'           determine if a given fleet has catchability for each stock. 
#' @param stockname Char (Optional). The name of a single stock that is assumed
#'                  to limit the effort of all fleets that catch it. Useful for
#'                  setting-up stock-specific scenarios.
#'                  
#' @return a matrix with stocks on the rows and fleets on the columns.
#' 
#' @export

stockExceptionsMatrix <- function(om, yr, stockname = NULL) {
  
  if (is.null(stockname)) {
    stockname <- ""
  }
  
  ## calculate matrix of stock x fleet containing metier-summed catchabilities
  q<-sapply(om$flts, function(x) {
    z <- rep(0, length(om$stks))
    names(z) <- names(om$stks)
    z[names(x)] <- sapply(x, function(y) {
      sum(catch.q(y)["alpha",ac(yr)])
    }, simplify = "array")
    return(z)
  }, simplify = "array")
  
  ## Fleets that catch target should ignore non-target quota
  q[rownames(q)!=stockname,q[rownames(q)==stockname,] > 0] <- 0  
  
  ## Fleets that catch target should be constrained by target quota
  q[rownames(q)==stockname,q[rownames(q)==stockname,] > 0] <- 1
  
  ## Fleets that do not catch target should be constrained by other stock quotas
  q[q>0] <- 1
  
  return(q)
}
