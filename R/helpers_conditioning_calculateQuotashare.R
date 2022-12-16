# ---
# title: 'Functions to help condition Operating Models'
# author: 'Matthew Pace'
# date: 'December 2022'
# ---
#
#' Estimate stock quota-share from historic landings-share
#' 
#' @export

calculateQuotashare <- function(stks, flts, verbose = TRUE) {
  
  ## Check that dimensions are identical for different fleets
  
  ## extract year and iter dimensions - these should be identical for all stocks
  ## and fleets
  yvector <- dimnames(flts[[1]])$year
  ivector <- dimnames(flts[[1]])$iter
  
  ## generate a vector of stock caught by fleets
  stknames <- unique(unlist(lapply(flts, names)))
  
  ## loop over each stock
  for(s in stknames) {
    
    ## Print stock names
    if(verbose) cat("\n",s,"\n")
    
    ## extract the landings for stock s for each fleet
    land_s <- sapply(names(flts), function(x){
      if(!is.null(flts[[x]][[s]])) {
        landings(flts[[x]][[s]])
      } else {
        FLQuant(0, dimnames = list(age = "all",
                                   year = yvector,
                                   iter = ivector))
      }
    }, simplify = "array")
    
    ## calculate proportional landings-share per fleet
    qshare_s <- sweep(land_s, c(1:6), apply(land_s, c(1:6), sum, na.rm = TRUE), "/")
    names(dimnames(qshare_s))[7] <- "fishery"
    
    ## Replace cases of NaN (resulting from divide by zero) with NA
    qshare_s[is.nan(qshare_s)] <- NA
    
    ## logical vector of catches for each fleet
    f_catch <- apply(qshare_s, c(7), sum, na.rm = TRUE)
    
    ## loop over each fleet and attach the proportional share for stock s as an attribute
    for(f in dimnames(qshare_s)$fishery){
      
      ## Only process fleets with catches
      if(f_catch[f] > 0) {
        
        ## print fleet name
        if(verbose) cat(f,"; ")
        
        ## create an FLQuant to store quota share
        qshare_flq <- FLQuant(qshare_s[,,,,,,f, drop = FALSE], 
                              dimnames = list(year = dimnames(qshare_s)$year,
                                              iter = dimnames(qshare_s)$iter))
        
        ## attach as attribute - this is extremely slow
        suppressMessages(attr(flts[[f]][[s]], "quotashare") <- qshare_flq)
      }
    }
  }
  
  return(list(stks = stks, flts = flts))
}