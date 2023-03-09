# ---
# title: 'Functions to help condition Operating Models'
# author: 'Various'
# date: 'November 2022'
# ---
# 
# Adapted from code by S.H. Fischer.
#
#' Calculate survey indices from \code{FLStocks} and \code{FLIndices}
#' 
#' Function calculate survey indices from the numbers-at-age contained in the
#' Operating model (\code{FLBiol}).
#' 
#' This is adapted from code used in WKNSMSE (2019).
#' 
#' @param stk Object of class \code{FLBiol} or \code{FLBiols} containing 
#'            information from the operating model for one or more stocks.
#' @param flt Object of class \code{FLFisheries} containing information from the
#'            operating model for one or more fishing fleets.
#' @param idx List, \code{FLIndices} or \code{FLIndex} object containing 
#'            survey information. 
#' @param use_q Logical. Calculate survey indices by multipling stock numbers 
#'              at age with survey catchability? Defaults to \code{TRUE}.
#' @param use_time Logical. Use survey timing to correct available stock numbers
#'                 for fishing and natural mortality. Defaults to \code{TRUE}.
#' 
#' @return an object of class \code{FLIndices} with the survey index stored in 
#' the \code{index} slot. 
#' 
#' @export

setGeneric("calculateSurvey", function(stk, flt, idx, 
                                       use_q = TRUE, use_time = TRUE) {
  standardGeneric("calculateSurvey")
})

## stk = FLBiol, flt = FLFisheries, idx = FLIndex
#' @rdname calculateSurvey
setMethod(f = "calculateSurvey",
          signature = signature(stk = "FLBiol",
                                flt = "FLFisheries",
                                idx = "FLIndex"),
          definition = function(stk, flt, idx, 
                                use_q = TRUE, use_time = TRUE) {
            
            calcSurveyIndex(stk = stk, flt = flt, idx = idx, 
                            use_q = use_q, use_time = use_time)
            
          })

## stk = FLBiol, flt = FLFisheries, idx = FLIndices
#' @rdname calculateSurvey
setMethod(f = "calculateSurvey",
          signature = signature(stk = "FLBiol",
                                flt = "FLFisheries",
                                idx = "FLIndices"),
          definition = function(stk, flt, idx, 
                                use_q = TRUE, use_time = TRUE) {
            
            FLCore::FLIndices(lapply(X = idx, 
                   FUN = calcSurveyIndex, 
                   stk = stk, flt = flt, use_q = use_q, use_time = use_time))
            
          })

## stk = FLBiols, flt = FLFisheries, idx = list
#' @rdname calculateSurvey
setMethod(f = "calculateSurvey",
          signature = signature(stk = "FLBiols",
                                flt = "FLFisheries",
                                idx = "list"),
          definition = function(stk, flt, idx, 
                                use_q = TRUE, use_time = TRUE) {
            
            Sidx <- lapply(X = names(stk),
                   function(x) {
                     calculateSurvey(stk = stk[[x]], 
                                     flt = flt,
                                     idx = idx[[x]],
                                     use_q = use_q, use_time = use_time)
                   })
            
            names(Sidx) <- names(stk)
            return(Sidx)
            
          })


calcSurveyIndex <- function(stk, flt, idx, use_q = TRUE, use_time = TRUE) {
  
  ## extract stock name
  stkname <- name(stk)
  
  ## survey type
  idxBiomass <- ifelse(all(dimnames(index(idx))$age == -1) |
                         all(dimnames(index(idx))$age == "all"),
         TRUE, FALSE)
  
  ## find ranges for ages 
  ## NOTE: if "-1", then this is a biomass survey. Use all ages to calculate SSB
  if(idxBiomass == TRUE) {
    ages <- dimnames(FLCore::n(stk))$age
  } else {
    ages <- intersect(dimnames(index(idx))$age, dimnames(FLCore::n(stk))$age)
  }
  
  ## find ranges for years & iters
  years <- intersect(dimnames(index(idx))$year, dimnames(FLCore::n(stk))$year)
  iter <- intersect(dimnames(index(idx))$iter, dimnames(FLCore::n(stk))$iter)
  
  ## timing of survey
  if (isTRUE(use_time)) {
    
    ## use mean of fishing period
    time <- mean(range(idx)[c("startf", "endf")])
    
  } else {
    
    ## otherwise assume beginning of year
    time <- 0
  }
  
  ## extract stock numbers for requested/available dimensions
  index.n <- FLCore::n(stk)[ac(ages), ac(years),,,, ac(iter)]
  
  ## Calculate F-at-age
  Fa <- sapply(1:length(flt), function(f){
    
    ## if fleet catches stock
    if(!is.null(flt[[f]][[stkname]])) {
      
      catch.q(flt[[f]][[stkname]])["alpha", ac(years), ac(iter)] %*% 
        flt[[f]]@effort[, ac(years),,,, ac(iter)] %*% 
        flt[[f]][[stkname]]@catch.sel[ac(ages), ac(years),,,, ac(iter)]
      
    } else {
      
      FLQuant(0, dimnames = list(age  = ages,
                                 year = years,
                                 iter = iter))
      
    }
  }, simplify = "array")
  
  Fa <- FLCore::FLQuant(apply(Fa, c(1:6), sum))
  
  ## get Z = M & F
  Z <- FLCore::m(stk)[ac(ages), ac(years),,,, ac(iter)] + Fa
  
  ## estimate stock numbers at time of survey
  index.n <- index.n * exp(-time * Z)
  
  # This next section is a little complex. If this is a non-commercial biomass
  # index, then calculate SSB. If this is a commercial landings per unit effort
  # index, then we want to calculate total landed biomass using landings weight.
  # Should this be a weighted average of all landings?
  
  ## if biomass, calculate SSB
  if(idxBiomass == TRUE) {
    index.n <- quantSums(index.n * 
                           # FLCore::mat(stk)[ac(ages), ac(years),,,, ac(iter)] *
                           FLCore::wt(stk)[ac(ages), ac(years),,,, ac(iter)])

    ## add catchability, if requested
    if (isTRUE(use_q)) {
      index.n <- index.n * index.q(idx)[, ac(years),,,, ac(iter)]
    }
    
    ## insert values into index
    index(idx)[, ac(years),,,, ac(iter)] <- index.n
    
  }
  
  if(idxBiomass == FALSE) {
    
    ## add catchability, if requested
    if (isTRUE(use_q)) {
      index.n <- index.n * index.q(idx)[ac(ages), ac(years),,,, ac(iter)]
    }
    
    ## insert values into index
    index(idx)[ac(ages), ac(years),,,, ac(iter)] <- index.n
    
  }
  
  return(idx)
}
