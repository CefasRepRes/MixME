# ---
# title: 'Functions to extend year dimensions - stf_args'
# author: 'Matthew Pace'
# date: 'March 2026'
# ---
#
#' Function to generate arguments needed by `stf`
#' 
#' @param om A list of \code{FLBiols} and \code{FLFisheries} containing the relevant stock and fleet information in the Operating Model.
#' @param method Character. The method to estimate future biological and fishery parameters. See details.
#' @param nyears Integer. The number of projection years to extend each fleet and stock slot.
#' @param wts.nyears Integer. The number of recent data years from which future 
#'                   biological (stock weights \code{wt}, natural mortality \code{m}, 
#'                   maturity \code{mat}, fecundity \code{fec}), fishery
#'                   (landings weight \code{landings.wt}, discards weight \code{discards.wt},
#'                   landed fraction \code{landings.n}, discarded fraction \code{discards.n})
#'                   parameters are estimated.
#' @param sel.nyears Integer. The number of recent data years from which future fishery
#'                   (catch selectivity \code{catch.sel}, catchability \code{catch.q}, 
#'                   \code{capacity}, \code{effort}) and survey (survey catchability \code{index.q})
#'                   parameters are estimated.
#' @param qs.nyears Integer. The number of recent data years from which future 
#'                  quota-shares (\code{quotashare}) are estimated.
#' @param seed (Optional) Integer. Random seed for reproducibility if using RNG.
#' 
#' @returns A named list containing the arguments to be supplied to other `stf`
#'          modules.

stf_args <- function(om,
                     method,
                     nyears, 
                     wts.nyears,
                     sel.nyears,
                     qs.nyears,
                     seed) {
  
  ## Extract final data year
  ydata <- as.numeric(tail(dimnames(om$stks[[1]])$year,1))
  
  ## Extract number of iterations
  ni    <- dim(om$stks[[1]])[6]
  
  ## Define vectors of years from which to source data
  year_wts <- tail(dimnames(om$stks[[1]])$year, wts.nyears) # weights
  year_sel <- tail(dimnames(om$stks[[1]])$year, sel.nyears) # selectivity & catchability
  year_qs  <- tail(dimnames(om$stks[[1]])$year, qs.nyears)  # quota-share
  
  ## Define vectors of years to overwrite
  year_proj <- ac((ydata+1):(ydata+nyears))
  
  if (method == "resample") {
    
    # NOTE: selectivity, catchability and effort need to take the same indices 
    #       for later calculations to work
    
    set.seed(seed)
    wts_samples <- sample(x = year_wts, size = nyears * ni, replace = TRUE)
    sel_samples <- sample(x = year_sel, size = nyears * ni, replace = TRUE)
    qs_samples  <- sample(x = year_qs,  size = nyears * ni, replace = TRUE)
    
  } else {
    wts_samples <- NULL
    sel_samples <- NULL
    qs_samples  <- NULL
  }
  
  return(list(ydata     = ydata,
              year_proj = year_proj,
              year_wts  = year_wts,
              year_sel  = year_sel,
              year_qs   = year_qs,
              wts_samples = wts_samples,
              sel_samples = sel_samples,
              qs_samples  = qs_samples,
              ni = ni))
}