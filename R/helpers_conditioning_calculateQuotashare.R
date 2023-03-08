# ---
# title: 'Functions to help condition Operating Models'
# author: 'Matthew Pace'
# date: 'December 2022'
# ---
#
#' Estimate stock quota-share from historic landings-share
#' 
#' @export

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