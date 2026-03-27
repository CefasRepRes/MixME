# ---
# title: 'Functions to carry out hindcast to check predictive skill'
# author: Matthew Pace
# date: 'March 2026'
# ---
#
#' Carry out a one-year projection of historical data
#' 
#' This function carries out a one year projection for a historical data year
#' and returns the updated MixME operating model. The function is intended to be
#' run as part of a hindcast routine to predict historical observations.
#' 
#' @param biols `FLBiols`. The stocks in the operating model.
#' @param fisheries `FLFisheries`. The fleets in the operating model.
#' @param yr Integer. The year to be projected as part of the hindcast.
#' 
#' @returns A named list of updated stocks and fleets.

hindcast_fwd <- function(biols, 
                         fisheries,
                         yr,
                         effort  = NULL,
                         popType = NULL) {
  
  if (is.null(effort)) {
    nf <- length(fisheries)
    ni <- dims(fisheries[[1]])$iter
    
    ## Extract fleet total effort
    effort <- matrix(NA, nrow = nf, ncol = ni)
    effort[] <- sapply(fisheries, function(x) areaSums(x@effort)[,as.character(yr)])
  }
  
  ## Trim biols to match fisheries
  if(dims(biols[[1]])$year != dims(fisheries[[1]])$year) {
    biols <- lapply(biols, function(x) window(x, 
                                              dims(fisheries[[1]])$minyear,
                                              dims(fisheries[[1]])$maxyear))
  }
  
  ## Get the baseline stock catches for target year
  fwd0 <- MixME:::projectFastfwd(om = list(stks = biols, flts = fisheries),
                                 tracking = NULL, 
                                 pars = effort, 
                                 yr = yr,
                                 popType = popType)
  return(fwd0$om)
}

