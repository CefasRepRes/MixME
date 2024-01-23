# ---
# title: 'Operating Model - Observation error model - trim data period'
# author: 'Matthew Pace'
# date: 'January 2024'
#
#' Trim FLStock to data period
#' 
#' Function takes stock observations in the form of \code{FLStock} and trims
#' the year dimension to the period for which we have catch or survey data.
#' 
#' @param stk0 An observed \code{FLStock}
#' @param ay Numeric. The assessment year.
#' @param idx_timing A named list of survey index timing relative to assessment year
#' @param catch_timing A named list of catch timing relative to assessment year
#' 
#' @return A named list of stock observations.

oemTrimFLStock <- function(stk0,
                           x,
                           ay,
                           idx_timing,
                           catch_timing) {
  
  ## We probably want to remove years preceding the data period - find minumum data year
  checkCatch <- iterSums(catch(stk0) > 0)
  checkCatch[is.na(checkCatch)] <- 0
  mindatayr <- dims(stk0[,checkCatch > 0])$minyear
  
  ## If survey data is more recent than catch data, then trim to survey year
  if(max(idx_timing[[x]]) > max(catch_timing[[x]])) {
    
    ## Trim stock object
    stk0 <- window(stk0, start = mindatayr, end = ay + max(idx_timing[[x]]))
    
    ## Remove data in years where no catch data is available
    yrs_remove <- (ay + catch_timing[[x]] + 1):ay
    
    catch(stk0)[, ac(yrs_remove)]       <- NA
    catch.n(stk0)[, ac(yrs_remove)]     <- NA
    catch.wt(stk0)[, ac(yrs_remove)]    <- NA
    landings(stk0)[, ac(yrs_remove)]    <- NA
    landings.n(stk0)[, ac(yrs_remove)]  <- NA
    landings.wt(stk0)[, ac(yrs_remove)] <- NA
    discards(stk0)[, ac(yrs_remove)]    <- NA
    discards.n(stk0)[, ac(yrs_remove)]  <- NA
    discards.wt(stk0)[, ac(yrs_remove)] <- NA
    
  } else {
    
    stk0 <- window(stk0, start = mindatayr, end = ay + max(catch_timing[[x]]))
    
  }
  
  ## return result
  return(stk0)
}

#' Trim FLBiol and FLFisheries to data period
#' 
#' Function takes stock observations in the form of \code{FLBiol} and \code{FLFisheries}
#' and trims the year dimension to the period for which we have catch or survey data.
#' 
#' @param stk0 An observed \code{FLBiol}
#' @param flt0 An observed \code{FLFisheries}
#' @param ay Numeric. The assessment year.
#' @param idx_timing A named list of survey index timing relative to assessment year
#' @param catch_timing A named list of catch timing relative to assessment year
#' 
#' @return A named list of stock observations.

oemTrimFLBiol <- function(stk0,
                          flt0,
                          x,
                          ay,
                          idx_timing,
                          catch_timing) {
  
  ## Find the fleets that catch x
  fltnames <- sapply(flt0, function(xx) x %in% names(xx))
  
  # A quick note here. This assumes that all the fleets are reporting catches on
  # the same time-scale. This means that if a fleet reports catches on a different
  # time-scale, then this is not accounted for.
  
  ## We probably want to remove years preceding the data period - find minimum data year
  stkcatch <- Reduce("+",catch(flt0))
  checkCatch <- iterSums(stkcatch > 0)
  checkCatch[is.na(checkCatch)] <- 0
  mindatayr <- dims(stk0@n[,checkCatch > 0])$minyear
  
  ## If survey data is more recent than catch data, then trim to survey year
  if(max(idx_timing[[x]]) > max(catch_timing[[x]])) {
    
    ## Trim stock object
    stk0 <- window(stk0, start = mindatayr, end = ay + max(idx_timing[[x]]))
    flt0 <- window(flt0, start = mindatayr, end = ay + max(idx_timing[[x]]))
    
    ## Remove data in years where no catch data is available
    yrs_remove <- (ay + catch_timing[[x]] + 1):ay
    
    ## loop over each fleet that catches x
    for(i in names(flt0)[fltnames]) {
      landings.n(flt0[[i]][[x]])[, ac(yrs_remove)] <- NA
      landings.wt(flt0[[i]][[x]])[,ac(yrs_remove)] <- NA
      discards.n(flt0[[i]][[x]])[, ac(yrs_remove)] <- NA
      discards.wt(flt0[[i]][[x]])[,ac(yrs_remove)] <- NA
    }
    
  } else {
    
    stk0 <- window(stk0, start = mindatayr, end = ay + max(catch_timing[[x]]))
    flt0 <- window(flt0, start = mindatayr, end = ay + max(catch_timing[[x]]))
    
  }
  
  ## return result
  return(list(stk = stk0,
              flt = flt0))
}