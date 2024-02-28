# ---
# title: 'Functions to help condition Operating Models'
# author: 'Matthew Pace'
# date: 'December 2022'
# ---
#
#' Estimate stock quota-share from historical landings-share
#' 
#' This function calculate the historical share of stock landings (by weight) for
#' each fleet and uses this as a proxy for the historical share of stock quota.
#' These data are attached as an `FLQuant` attribute to each `FLCatch` in each
#' `FLFishery`.
#' 
#' @param stks An object of class `FLStocks` or `FLBiols`. 
#'             This is a required input but is not currently used in the function.
#' @param flts An object of class `FLFisheries`.
#' @param verbose `Logical`. Should progress be printed to console? Defaults to `TRUE`.
#' @param useCpp `Logical`. Should a C++ helper function be used to speed up 
#'               attachment of outputs. Defaults to `TRUE` - THIS IS HIGHLY RECOMMENDED.
#' 
#' @returns A named `list` containing the updated stocks and fleets
#' 
#' @export
#' @examples
#' \donttest{
#' ## load example data
#' data("singlestock_MixME_om")
#' 
#' ## calculate quota-share from landings share
#' out <- calculateQuotashare(stks = singlestock_MixME_om$stks, 
#'                            flts = singlestock_MixME_om$flts, verbose = FALSE)
#'                            
#' ## Overwrite with updated objects
#' singlestock_MixME_om$stks <- out$stks
#' singlestock_MixME_om$flts <- out$flts
#' }

calculateQuotashare <- function(stks,             # Not currently used
                                flts,             # FLFisheries
                                verbose = TRUE,   # Print progress?
                                useCpp = TRUE) {  # Use C++ to attach results
  
  ## Check that dimensions are identical for different fleets
  
  ## extract year and iter dimensions - these should be identical for all stocks
  ## and fleets
  yvector <- dimnames(flts[[1]])$year
  ivector <- dimnames(flts[[1]])$iter
  
  ## extract list of stocks caught by each fleet
  fltcatches <- lapply(flts, names)
  
  ## Check that each catch for each fleet is unique
  lapply(names(fltcatches), function(x){
    if(any(duplicated(fltcatches[[x]]))){
      stop("In fleet ",x,", catch names are duplicated!")
    }
  })
  
  ## generate a vector of stock caught by fleets
  stknames <- unique(unlist(fltcatches))
  
  ## loop over each stock
  for(st in stknames) {
    
    ## Print stock names
    if(verbose) cat("\n",st,"\n")
    
    ## extract the landings for stock s for each fleet
    land_s <- sapply(names(flts), function(x){
      if(!is.null(flts[[x]][[st]])) {
        landings(flts[[x]][[st]])
      } else {
        FLQuant(0, dimnames = list(age = "all",
                                   year = yvector,
                                   iter = ivector))
      }
    }, simplify = "array")
    
    ## calculate proportional landings-share per fleet
    qshare_s <- sweep(land_s, c(1:6), apply(land_s, c(1:6), sum, na.rm = TRUE), "/")
    names(dimnames(qshare_s))[7] <- "fishery"
    
    ## Replace cases of NaN (resulting from divide by zero) with 0
    qshare_s[is.nan(qshare_s)] <- 0
    
    ## logical vector of catches for each fleet
    f_catch <- apply(qshare_s, c(7), sum, na.rm = TRUE)
    
    ## loop over each fleet and attach the proportional share for stock s as an attribute
    for(fl in dimnames(qshare_s)$fishery){
      
      ## Only process fleets with catches
      if(f_catch[fl] > 0) {
        
        ## print fleet name
        if(verbose) cat(fl,"; ")
        
        ## create an FLQuant to store quota share
        qshare_flq <- FLQuant(qshare_s[,,,,,,fl, drop = FALSE], 
                              dimnames = list(year = dimnames(qshare_s)$year,
                                              iter = dimnames(qshare_s)$iter))
        
        if(useCpp == TRUE) {
          
          ## attach as attribute using C++
          flts <- attach_attribute(fisheries = flts, 
                                   attribute = qshare_flq, 
                                   fl = fl, 
                                   st = st, 
                                   attribute_name = "quotashare")
          
        } else {
          
          ## attach as attribute using R - this is extremely slow
          attr(flts[[fl]][[st]], "quotashare") <- qshare_flq
        }
      }
    }
  }
  
  return(list(stks = stks, flts = flts))
}