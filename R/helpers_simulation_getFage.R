# ---
# title: 'Function to calculate fishing mortality at age'
# author: 'Matthew Pace'
# date: 'January 2025'
# ---
#
#' Calculate fishing mortality at age for a defined stock
#' 
#' This function calculates fishing mortality at age given an \code{FLFisheries}
#' object.
#' 
#' @param stks Object of class \code{FLBiols}.
#' @param flts Object of class \code{FLFisheries}.
#' @param stkname Character. Name of the stock for which fishing mortality at 
#'                age is required
#' @param yr (Optional) Integer vector. Vector of years for which to subset the
#'           calculated fishing mortality at age.
#' @param use_fastF (Optional). Use C++ implementation of fishing mortality
#'                  calculation? Defaults to \code{TRUE}. 
#' 
#' @return A 6-dimensional array containing the fishing mortality at age for a user-defined
#'         stock.

getFage <- function(stks, flts, stkname, yr = NULL, use_fastF = TRUE) {
  
  ## define year range for output
  if (is.null(yr)) {
    yr <- dimnames(stks[[stkname]])$year
  }
  
  # There are two methods that could be applied here:
  # 
  # 1. Use C++ to rapidly calculate fishing mortality at age given an FLFisheries
  #    object and a stock name that corresponds to the catch slot name in an 
  #    FLFishery.
  
  if(use_fastF) {
    
    ## use c++ calculation of fishing mortality
    arr <- array(0, 
                 dim = c(dim(stks[[stkname]]),length(flts)), 
                 dimnames = c(dimnames(stks[[stkname]]), list(flt = names(flts))))
    pFa <- fa_cpp(arr = arr, flts = flts, stockname = stkname)
    Fage <- stks[[stkname]]@n
    Fage[] <- apply(pFa, 1:6, sum)
    Fage   <- Fage[,ac(yr)]
    
    return(Fage)
  }
  
  # 2. Use R code calculation of fishing mortality. This is a little slower and
  #    cannot handle metier structuring, but is a useful debugging tool and check 
  #    for C++ calculations during testing.
  
  fltFage <- sapply(1:length(flts), function(y){
    
    if(!is.null(flts[[y]][[stkname]])) {
      flts[[y]][[stkname]]@catch.q[1,ac(yr)] %*% 
        flts[[y]]@effort[,ac(yr)] %*% 
        flts[[y]][[stkname]]@catch.sel[,ac(yr)]
    } else {
      
      ## A bit of a hacky way to retrieve correct dimensions
      FLQuant(0, dimnames = list(age = dimnames(stks[[stkname]])$age, 
                                 year = yr, 
                                 iter = dimnames(stks[[stkname]])$iter))
    }
  }, simplify = "array")
  
  Fage <- apply(fltFage, c(1:6), sum)
  dimnames(Fage)$year <- ac(yr)
  return(Fage)
  
}