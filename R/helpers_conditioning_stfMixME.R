# ---
# title: 'Functions to extend year dimensions'
# author: 'Matthew Pace'
# date: 'December 2022'
# ---
#
#' Function to extend a MixME Operating Model to future years for forecasting
#' 
#' @export

stfMixME <- function(om, 
                     nyears     = 3, 
                     wts.nyears = 3,
                     sel.nyears = 3,
                     qs.nyears  = 3,
                     verbose = TRUE) {
  
  for(x in names(om$stks)) {
    
    ## print stock name
    if(verbose) cat(x, "; ")
    
    ## Extract final data year
    ydata <- as.numeric(tail(dimnames(om$stks[[x]])$year,1))
    
    om$stks[[x]] <- FLasher::stf(om$stks[[x]], 
                                 nyears = nyears, 
                                 wts.nyears = wts.nyears)
    
    ## If object is FLBiol, then manually fill 'mat' and 'fec' slots
    if(class(om$stks[[x]]) == "FLBiol") {
      
      ## fill mat
      mat(om$stks[[x]])[,ac((ydata+1):(ydata+nyears))] <- 
        yearMeans(mat(om$stks[[x]])[,ac((ydata-wts.nyears+1):ydata)])
      
      ## fill fec
      fec(om$stks[[x]])[,ac((ydata+1):(ydata+nyears))] <- 
        yearMeans(fec(om$stks[[x]])[,ac((ydata-wts.nyears+1):ydata)])
    }
  }
  
  for(x in names(om$flts)) {
    
    ## print fleet name
    if(verbose) cat("\n", x, "\n")
    
    ## extract fishery object
    fishery <- om$flts[[x]]
    
    # NOTE: selectivity, catchability and effort need to take the same indices 
    #       for later calculations to work
    
    ## extract year indices for averaging
    year_wts <- tail(dimnames(fishery)$year, wts.nyears) # mean weights
    year_sel <- tail(dimnames(fishery)$year, sel.nyears) # selectivity & catchability
    year_qs  <- tail(dimnames(fishery)$year, qs.nyears)  # quota-share
    
    ## extract stock names
    stknames <- names(fishery)
    
    ## Extract final data year
    ydata <- as.numeric(tail(dimnames(fishery)$year,1))
    
    ## extend FLFishery
    fishery <- window(fishery, end = ydata+nyears)
    
    ## extend FLCatch - needs to be done simultaneously
    suppressMessages(om$flts[[x]] <- 
                       FLFishery::FLFishery(
                         # FLFishery::FLCatches(
                           lapply(names(fishery), function(y){
        
        ## print stock name
        if(verbose) cat(y, " ; ")
        
        ## define minimum year based on catchability data availability
        ymin <- min(dimnames(attr(fishery[[y]],"catchq"))$year)
        
        ## extend FLQuants
        stkflt_ext <- window(fishery[[y]], end = ydata+nyears)
        
        ## loop over slots and calculate mean
        FLFishery::landings.wt(stkflt_ext)[,ac((ydata+1):(ydata+nyears))] <- 
          FLCore::yearMeans(landings.wt(fishery[[y]])[,year_wts])
        FLFishery::discards.wt(stkflt_ext)[,ac((ydata+1):(ydata+nyears))] <- 
          FLCore::yearMeans(discards.wt(fishery[[y]])[,year_wts])
        FLFishery::catch.sel(stkflt_ext)  [,ac((ydata+1):(ydata+nyears))] <- 
          FLCore::yearMeans(catch.sel(fishery[[y]])[,year_sel])
        
        ## calculate landings and discards fractions
        ## and populate landings.n and discards.n
        landfraction <- 
          FLCore::yearMeans(FLCore::landings.n(fishery[[y]])[,year_wts] / 
                              FLCore::catch.n(fishery[[y]])[,year_wts])
        
        ## in cases where there are zero landings and discards resulting in NA
        ## set to zero
        landfraction[is.na(landfraction)] <- 0
        
        FLCore::landings.n(stkflt_ext)[,ac((ydata+1):(ydata+nyears))] <- landfraction
        FLCore::discards.n(stkflt_ext)[,ac((ydata+1):(ydata+nyears))] <- 1- landfraction
        
        ## Handle future catch.q - The catch.q FLPar needs to be propagated manually
        if(dims(stkflt_ext)$iter > 1 & 
           (dims(catch.q(stkflt_ext))$iter != dims(stkflt_ext)$iter)) {
          
          catch.q(stkflt_ext) <- propagate(catch.q(stkflt_ext), 
                                           iter = dims(stkflt_ext)$iter)
        } 
        
        ## Check if catchq attribute exists
        if(!is.null(attr(fishery[[y]],"catchq"))) {
          
          ## define single parameter
          catch.q(stkflt_ext)["alpha",] <- 
            apply(attr(fishery[[y]],"catchq")[,year_sel], 
                  c(1,3:6), mean)
          
          ## Save future catch.q to attr
          # - extend catchq FLQuant to full year range
          # - insert projection data
          attr(stkflt_ext,"catchq") <- window(attr(fishery[[y]],"catchq"), 
                                              start = min(dimnames(fishery[[y]])$year), 
                                              end   = (ydata+nyears))
          
          ## This is a really dumb way of doing this... but can't think of a better
          ## method right now
          for(yy in ac((ydata+1):(ydata+nyears))) {
            
            attr(stkflt_ext,"catchq")[,yy] <- catch.q(stkflt_ext)["alpha",]
            
          }
          
        } else {
          warning("attr 'catchq' for fishery ",x," and stock ",y," does not exist!")
        }
      
        
        ## Handle future quotashare - extend FLQuant
        if(!is.null(attr(fishery[[y]],"quotashare"))) {
          
          attr(stkflt_ext,"quotashare") <- 
            window(attr(fishery[[y]],"quotashare"), end = (ydata+nyears))
          
          ## estimate future quota share
          attr(stkflt_ext,"quotashare")[,ac((ydata+1):(ydata+nyears))] <- 
            apply(attr(fishery[[y]],"quotashare")[,year_qs], c(1,3:6), mean)
          
        } else {
          warning("attr 'quotashare' for fishery ",x," and stock ",y," does not exist!")
        }
        
        return(stkflt_ext)
      }) # end lapply
      #) # close FLCatches
      ) # close FLFishery
    ) # close suppress messages
    
    # -------------------------#
    # Insert fishery-level data
    # -------------------------#
    
    ## Insert capacity. If NA replace with 1
    suppressMessages(capacity(om$flts[[x]]) <- capacity(fishery))
    suppressMessages(capacity(om$flts[[x]])[,ac((ydata+1):(ydata+nyears))] <- 
                       yearMeans(capacity(fishery)[,year_sel]))
    suppressMessages(
      capacity(om$flts[[x]])[is.na(capacity(om$flts[[x]]))] <- 1)
    
    ## Insert effort
    suppressMessages(effort(om$flts[[x]]) <- fishery@effort)
    suppressMessages(effort(om$flts[[x]])[,ac((ydata+1):(ydata+nyears))] <- 
                       yearMeans(fishery@effort[,year_sel]))
    
    # hperiod(stkfltAll$flts[[x]])  <- hperiod(fishery)
    # vcost(stkfltAll$flts[[x]])    <- vcost(fishery)
    # fcost(stkfltAll$flts[[x]])    <- fcost(fishery)
    suppressMessages(orevenue(om$flts[[x]]) <- orevenue(fishery))
    
    ## Insert names
    suppressMessages(names(om$flts[[x]]) <- stknames)
    
  }
  
  return(om)
}