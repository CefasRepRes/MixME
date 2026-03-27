# ---
# title: 'Functions to extend year dimensions - stf_stocks'
# author: 'Matthew Pace'
# date: 'March 2026'
# ---
#
#' Function to extend `FLStocks`/`FLBiols`
#' 
#' @param stocks \code{FLBiols} containing the relevant operating model stock information.
#' @param method Character. The method to estimate future biological and fishery parameters. See details.
#' @param nyears Integer. The number of projection years to extend each fleet and stock slot.
#' @param wts.nyears Integer. The number of recent data years from which future 
#'                   biological (stock weights \code{wt}, natural mortality \code{m}, 
#'                   maturity \code{mat}, fecundity \code{fec}), fishery
#'                   (landings weight \code{landings.wt}, discards weight \code{discards.wt},
#'                   landed fraction \code{landings.n}, discarded fraction \code{discards.n})
#'                   parameters are estimated.
#' @param wts_samples Integer vector. The sampled vector of year indexes from which parameters should be drawn.
#' @param year_wts Integer vector. The years to be sampled using `wts_samples`.
#' @param years_proj Integer vector. The projection years to be populated.
#' @param ni Integer. The number of iterations.
#' @param verbose Logical. Should progress be reported? Defaults to \code{TRUE}.
#' @param seed (Optional) Integer. Random seed for reproducibility if using RNG.
#' 
#' @returns \code{FLBiols} with projection years and estimated stock parameters.

stf_stocks <- function(stocks,
                       stocks_ref = NULL,
                       method,
                       nyears,
                       wts.nyears,
                       wts_samples,
                       year_wts,
                       year_proj,
                       ni,
                       verbose) {
  
  ## do parameters
  null_ref <- is.null(stocks_ref)
  
  for (x in names(stocks)) {
    
    ## print stock name
    if (verbose) 
      cat(x, "; ")
    
    ## extend stock information
    stocks[[x]] <- FLasher::stf(stocks[[x]], 
                                 nyears = nyears, 
                                 wts.nyears = wts.nyears)
    
    ## If resampling, I need to overwrite the default projected data
    if (class(stocks[[x]]) == "FLStock" & method == "resample") {
      stocks[[x]]@catch.wt[,year_proj]    <- stfQuant(catch.wt(stocks[[x]]),    
                                                       method  = method, 
                                                       samples = wts_samples,
                                                       ni      = ni)
      stocks[[x]]@stock.wt[,year_proj]    <- stfQuant(stock.wt(stocks[[x]]),    
                                                       method  = method, 
                                                       samples = wts_samples,
                                                       ni      = ni)
      stocks[[x]]@landings.wt[,year_proj] <- stfQuant(landings.wt(stocks[[x]]), 
                                                       method  = method, 
                                                       samples = wts_samples,
                                                       ni      = ni)
      stocks[[x]]@discards.wt[,year_proj] <- stfQuant(discards.wt(stocks[[x]]), 
                                                       method  = method, 
                                                       samples = wts_samples,
                                                       ni      = ni)
      stocks[[x]]@m[,year_proj]           <- stfQuant(FLCore::m(stocks[[x]]),   
                                                       method  = method, 
                                                       samples = wts_samples,
                                                       ni      = ni)
      stocks[[x]]@mat[,year_proj]         <- stfQuant(FLCore::mat(stocks[[x]]), 
                                                       method  = method, 
                                                       samples = wts_samples,
                                                       ni      = ni)
    }
    
    ## If object is FLBiol, then manually fill 'mat' and 'fec' slots
    if (class(stocks[[x]]) == "FLBiol") {
      
      if (!null_ref) {
        ## Replace stock numbers
        stocks[[x]]@n[,year_proj] <- stocks_ref[[x]]@n[,year_proj]
      }
      
      if (method == "resample") {
        stocks[[x]]@wt[,year_proj] <- stfQuant(FLCore::wt(stocks[[x]])[,year_wts], 
                                                method  = method, 
                                                samples = wts_samples,
                                                ni      = ni)
        stocks[[x]]@m[,year_proj]  <- stfQuant(FLCore::m(stocks[[x]])[,year_wts],  
                                                method  = method, 
                                                samples = wts_samples,
                                                ni      = ni)
      }
      
      ## fill mat
      mat(stocks[[x]])[,year_proj] <- stfQuant(mat(stocks[[x]])[,year_wts], 
                                                method  = method, 
                                                samples = wts_samples,
                                                ni      = ni)
      
      ## fill fec
      fec(stocks[[x]])[,year_proj] <- stfQuant(fec(stocks[[x]])[,year_wts], 
                                                method  = method, 
                                                samples = wts_samples,
                                                ni      = ni)
    }
  }
  return(stocks)
}