# ---
# title: 'Functions to carry out hindcast to check predictive skill'
# author: Matthew Pace
# date: 'March 2026'
# ---
#
#' Carry out a multi-year hindcast
#' 
#' This function carries out a multi-year hindcast to assess the predictive 
#' skill of a MixME operating model. 
#' 
#' The function takes the inputs to a MixME simulation and applies the following 
#' procedure: 
#' 
#'  - Peel off 'x' data years.
#'  - Loop over FLFisheries. Recondition parameters as specified
#'  - Project a single year forward using observed effort for each fleet.
#'  - Calculate the discrepancy in the catches of each stock for the focal fleet/s. 
#' 
#' @param input A MixME object comprising, at minimum, an operating model (om) and
#'               global arguments (args) specifying the initial projection year
#' @param nyears Integer. The number of years over which to carry out the hind-cast.
#' @param basis Character vector (optional). Which parameters should be reconditioned?
#'              Defaults to `NULL`: historical estimates are used and this tests the
#'              internal numerical consistency of the model
#' @param fleets Character vector (optional). Should the hindcast be restricted to 
#'               one or more fleets? Defaults to `NULL`: all fleets included in
#'               the reconditioning.
#'               
#' @returns A list containing:
#' 
#'  - **Cobs**: A 4D array containing the observed catches per fleet, stock, year and iteration
#'  - **Csim**: A 5D array containing the predicted catches per fleet, stock, year, iteration and basis
#'  - **basis**: The names of fleet parameters reconditioned during the hindcast
#'  - **fleets**: The names of fleets that were reconditioned during the hindcast
#'  - **years**: The years over which the hindcast was applied
#'
#' @export

hindcast <- function(input, nyears, basis = NULL, fleets = NULL, ...) {
  
  ## Check inputs
  if(!is.list(input)) stop("input must be a list")
  if(!all(c("om","args") %in% names(input))) stop("input must be a named list containing 'om' and 'args'")
  if(!is.list(input$om)) stop("'om' must be a list")
  if(!all(c("stks","flts") %in% names(input$om))) stop("'om' must be a named list containing 'stks' and 'flts'")
  if(class(input$om$stks)[1] != "FLBiols") stop("'stks' must be of class FLBiols")
  if(class(input$om$flts)[1] != "FLFisheries") stop("'stks' must be of class FLFisheries")
  
  ## Handle arguments
  if(is.null(fleets)) fleets <- names(input$om$flts)
  if(!all(fleets %in% names(input$om$flts))) stop("'fleets' does not match FLFishery names")
  
  if(is.null(basis)) basis <- "baseline"
  if(!all(basis %in% c("baseline","catch.q","catch.sel","landfrac","landings.wt","discards.wt","effort-share")))
    stop("'basis' must be either 'baseline' or a vector of one or more of the following: \n
         'catch.q','catch.sel','landfrac','landings.wt','discards.wt','effort-share'")
  if(any(basis %in% "baseline") & length(basis)>1)
    stop("'basis' must be either 'baseline' or a vector of one or more of the following: \n
         'catch.q','catch.sel','landfrac','landings.wt','discards.wt','effort-share'")
  
  ## Get vector of hindcast years
  years <- input$args$iy - seq(nyears)
  
  ## Get vector of data years
  years_data <- input$args$y0:(input$args$iy-1)
  
  ## Get dimensions
  ns <- length(input$om$stks)
  nf <- length(input$om$flts)
  ny <- length(years_data)
  ni <- dim(input$om$stks[[1]])[6]
  
  ## Check on year dimension, stock availability
  
  ## Define hindcast output
  Cobs <- array(NA, dim = c(ns,       # stocks
                            nf,       # fleets
                            ny,       # years
                            ni),      # iterations
                dimnames = list(stks = names(input$om$stks),
                                flts = names(input$om$flts),
                                year = years_data,
                                iter = dimnames(input$om$stks[[1]])$iter))
  
  ## Extract true stock catches for target year
  catches_historical <- sapply(names(input$om$stks), function(x) {
    Lw <- MixME:::getCW(input$om$flts, x, "landings", FALSE)[,as.character(years_data),,,,,,drop=F]
    Dw <- MixME:::getCW(input$om$flts, x, "discards", FALSE)[,as.character(years_data),,,,,,drop=F]
    return(apply(Lw+Dw, c("fleet","year","iter"), sum))
  })
  
  for(st in names(catches_historical)) {
    fl <- dimnames(catches_historical[[st]])$fleet
    Cobs[st,fl,as.character(years_data),] <- catches_historical[[st]]
  }
  
  Csim <- array(NA, dim = c(ns,       # stocks
                            nf,       # fleets
                            ny,       # years
                            ni,
                            2),      # iterations
                dimnames = list(stks = names(input$om$stks),
                                flts = names(input$om$flts),
                                year = years_data,
                                iter = dimnames(input$om$stks[[1]])$iter,
                                basis = c("Baseline","Reconditioned")))
  
  ## Loop over each hindcast year
  for(yr in years) {
    
    ## Extract fleet total effort
    effs <- matrix(NA, nrow = nf, ncol = ni)
    effs[] <- sapply(input$om$flts, function(x) areaSums(x@effort)[,as.character(yr)])
    
    ## Run hindcast to get the baseline stock catches for target year
    om_fwd_baseline <- hindcast_fwd(input$om$stks,
                                input$om$flts,
                                yr,
                                effs,
                                input$args$popType)
    
    ## Get baseline catches
    catches_baseline <- sapply(names(input$om$stks), function(x) {
      Lw <- MixME:::getCW(om_fwd_baseline$flts, x, "landings", FALSE)[,as.character(yr),,,,,,drop=F]
      Dw <- MixME:::getCW(om_fwd_baseline$flts, x, "discards", FALSE)[,as.character(yr),,,,,,drop=F]
      return(apply(Lw+Dw, c("fleet","year","iter"), sum))
    })
    
    for(st in names(catches_baseline)) {
      fl <- dimnames(catches_baseline[[st]])$fleet
      Csim[st,fl,as.character(yr),,"Baseline"] <- catches_baseline[[st]]
    }
    
    ## If basis is other than baseline, recondition fleets and re-run
    if(any(basis != "baseline")) {
      
      ## Remove current data year
      reconditioned_om <- input$om
      
      ## The problem with peeling back a year like this is that we lose true effort
      ## - This is fine - we already have 'effs'
      reconditioned_om$stks <- FLCore::window(reconditioned_om$stks, input$args$y0, yr-1)
      reconditioned_om$flts <- FLCore::window(reconditioned_om$flts, input$args$y0, yr-1)
      
      ## extend OM and recondition
      reconditioning_args <- c(om = list(reconditioned_om), 
                               om_ref = list(input$om), 
                               basis = list(basis), 
                               nyears = 1, 
                               fleets = list(fleets),
                               list(...))
      reconditioned_om <- do.call(hindcast_stf, reconditioning_args)
      
      # test <- MixME::stfMixME(reconditioned_om, method = "yearMeans", nyears = 1)
      # lapply(test$stks, function(x) x@n)
      
      ## Run hindcast to get the baseline stock catches for target year
      om_fwd_recond <- hindcast_fwd(reconditioned_om$stks,
                                       reconditioned_om$flts,
                                       yr,
                                       effs,
                                       input$args$popType)
      
      ## Get reconditioned catches
      catches_reconditioned <- sapply(names(reconditioned_om$stks), function(x) {
        Lw <- MixME:::getCW(om_fwd_recond$flts, x, "landings", FALSE)[,as.character(yr),,,,,,drop=F]
        Dw <- MixME:::getCW(om_fwd_recond$flts, x, "discards", FALSE)[,as.character(yr),,,,,,drop=F]
        return(apply(Lw+Dw, c("fleet","year","iter"), sum))
      })
      
      for(st in names(catches_reconditioned)) {
        fl <- dimnames(catches_reconditioned[[st]])$fleet
        Csim[st,fl,as.character(yr),,"Reconditioned"] <- catches_reconditioned[[st]]
      }
      
    }
  }
  
  return(list(Cobs = Cobs, 
              Csim = Csim,
              basis = basis,
              fleets = fleets,
              years = years))
}

