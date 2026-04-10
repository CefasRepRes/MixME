# ---
# title: 'Functions to calculate catch residuals to check internal consistency'
# author: Matthew Pace
# date: 'March 2026'
# ---
#
#' Calculate residuals between observed and predicted catches
#' 
#' This function carries out a one-year projection for a specified historical
#' data year and calculates the residual catch, landings or discards in terms of
#' numbers or biomass for that year as a check of the internal consistency of 
#' the operating model.
#' 
#' @param biols `FLBiols`. The stocks in the operating model.
#' @param fisheries `FLFisheries`. The fleets in the operating model.
#' @param component Character. Accepts 'catch', 'landings' or 'discards'.
#' @param quantity Character. Accepts 'numbers' or 'biomass'.
#' @param yr Integer. The year to be projected as part of the hindcast.
#' 
#' @returns A named list of length number of stocks containing to overall catch
#'          residuals for the operating model
#'          
#' @example 
#' ## load data
#' data("singlestock_MixME_om")
#' 
#' ## check internal consistency of catch production in 2019
#' catch_residuals(singlestock_MixME_om$stks, 
#'                 singlestock_MixME_om$flts,
#'                 "catch", "numbers", 2019)

catch_residuals <- function(biols, 
                            fisheries, 
                            component = c("catch","landings","discards"), 
                            quantity = c("numbers","biomass"), 
                            yr) {
  
  component <- match.arg(component)
  quantity  <- match.arg(quantity)
  
  out_baseline <- hindcast_fwd(biols, 
                               fisheries,
                               yr)
  
  if (quantity == "biomass" & component == "catch") {
    
    ## Get baseline catches
    catches_pred <- sapply(names(biols), function(x) {
      Lw <- MixME:::getCW(out_baseline$flts, x, "landings", FALSE)[,as.character(yr),,,,,,drop=F]
      Dw <- MixME:::getCW(out_baseline$flts, x, "discards", FALSE)[,as.character(yr),,,,,,drop=F]
      return(apply(Lw+Dw, c("fleet","year","iter"), sum))
    })
    
    ## Get historical catches
    catches_obs <- sapply(names(biols), function(x) {
      Lw <- MixME:::getCW(fisheries, x, "landings", FALSE)[,as.character(yr),,,,,,drop=F]
      Dw <- MixME:::getCW(fisheries, x, "discards", FALSE)[,as.character(yr),,,,,,drop=F]
      return(apply(Lw+Dw, c("fleet","year","iter"), sum))
    })
  }
  if (quantity == "biomass" & component == "landings") {
    
    ## Get baseline catches
    catches_pred <- sapply(names(biols), function(x) {
      Lw <- MixME:::getCW(out_baseline$flts, x, "landings", FALSE)[,as.character(yr),,,,,,drop=F]
      return(apply(Lw, c("fleet","year","iter"), sum))
    })
    
    ## Get historical catches
    catches_obs <- sapply(names(biols), function(x) {
      Lw <- MixME:::getCW(fisheries, x, "landings", FALSE)[,as.character(yr),,,,,,drop=F]
      return(apply(Lw, c("fleet","year","iter"), sum))
    })
  }
  if (quantity == "biomass" & component == "discards") {
    
    ## Get baseline catches
    catches_pred <- sapply(names(biols), function(x) {
      Dw <- MixME:::getCW(out_baseline$flts, x, "discards", FALSE)[,as.character(yr),,,,,,drop=F]
      return(apply(Dw, c("fleet","year","iter"), sum))
    })
    
    ## Get historical catches
    catches_obs <- sapply(names(biols), function(x) {
      Dw <- MixME:::getCW(fisheries, x, "discards", FALSE)[,as.character(yr),,,,,,drop=F]
      return(apply(Dw, c("fleet","year","iter"), sum))
    })
  }
  
  if (quantity == "numbers" & component == "catch") {
    
    ## Get baseline catches
    catches_pred <- sapply(names(biols), function(x) {
      browser()
      Ln <- MixME:::getC(out_baseline$flts, x, "landings.n", FALSE)[,as.character(yr),,,,,,drop=F]
      Dn <- MixME:::getC(out_baseline$flts, x, "discards.n", FALSE)[,as.character(yr),,,,,,drop=F]
      return(apply(Ln+Dn, c("fleet","year","iter"), sum))
    })
    
    ## Get historical catches
    catches_obs <- sapply(names(biols), function(x) {
      Ln <- MixME:::getC(fisheries, x, "landings.n", FALSE)[,as.character(yr),,,,,,drop=F]
      Dn <- MixME:::getC(fisheries, x, "discards.n", FALSE)[,as.character(yr),,,,,,drop=F]
      return(apply(Lw+Dw, c("fleet","year","iter"), sum))
    })
  }
  if (quantity == "numbers" & component == "landings") {
    
    ## Get baseline catches
    catches_pred <- sapply(names(biols), function(x) {
      Ln <- MixME:::getC(out_baseline$flts, x, "landings.n", FALSE)[,as.character(yr),,,,,,drop=F]
      return(apply(Ln, c("fleet","year","iter"), sum))
    })
    
    ## Get historical catches
    catches_obs <- sapply(names(biols), function(x) {
      Ln <- MixME:::getC(fisheries, x, "landings.n", FALSE)[,as.character(yr),,,,,,drop=F]
      return(apply(Ln, c("fleet","year","iter"), sum))
    })
  }
  if (quantity == "numbers" & component == "discards") {
    
    ## Get baseline catches
    catches_pred <- sapply(names(biols), function(x) {
      Dn <- MixME:::getC(out_baseline$flts, x, "discards.n", FALSE)[,as.character(yr),,,,,,drop=F]
      return(apply(Dn, c("fleet","year","iter"), sum))
    })
    
    ## Get historical catches
    catches_obs <- sapply(names(biols), function(x) {
      Dn <- MixME:::getC(fisheries, x, "discards.n", FALSE)[,as.character(yr),,,,,,drop=F]
      return(apply(Dn, c("fleet","year","iter"), sum))
    })
  }
  
  ## calculate residual catches
  catches_resid <- lapply(1:length(catches_pred), function(x) {
    catches_pred[[x]] - catches_obs[[x]]
  })
  names(catches_resid) <- names(biols)
  return(catches_resid)
}