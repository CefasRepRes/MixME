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
                     method     = "yearMeans",
                     nyears     = 3, 
                     wts.nyears = 3,
                     sel.nyears = 3,
                     qs.nyears  = 3,
                     verbose = TRUE,
                     seed    = NULL) {
  
  # ----------------------------------------------------------------------------
  # Prepare arguments
  # ----------------------------------------------------------------------------
  
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
  
  
  # ----------------------------------------------------------------------------
  # Process FLStocks/FLBiols
  # ----------------------------------------------------------------------------
  
  for (x in names(om$stks)) {
    
    ## print stock name
    if (verbose) 
      cat(x, "; ")
    
    ## extend stock information
    om$stks[[x]] <- FLasher::stf(om$stks[[x]], 
                                 nyears = nyears, 
                                 wts.nyears = wts.nyears)
    
    ## If resampling, I need to overwrite the default projected data
    if (class(om$stks[[x]]) == "FLStock" & method == "resample") {
      om$stks[[x]]@catch.wt[,year_proj]    <- stfQuant(catch.wt(om$stks[[x]]),    
                                                       method  = method, 
                                                       samples = wts_samples,
                                                       ni      = ni)
      om$stks[[x]]@stock.wt[,year_proj]    <- stfQuant(stock.wt(om$stks[[x]]),    
                                                       method  = method, 
                                                       samples = wts_samples,
                                                       ni      = ni)
      om$stks[[x]]@landings.wt[,year_proj] <- stfQuant(landings.wt(om$stks[[x]]), 
                                                       method  = method, 
                                                       samples = wts_samples,
                                                       ni      = ni)
      om$stks[[x]]@discards.wt[,year_proj] <- stfQuant(discards.wt(om$stks[[x]]), 
                                                       method  = method, 
                                                       samples = wts_samples,
                                                       ni      = ni)
      om$stks[[x]]@m[,year_proj]           <- stfQuant(FLCore::m(om$stks[[x]]),   
                                                       method  = method, 
                                                       samples = wts_samples,
                                                       ni      = ni)
      om$stks[[x]]@mat[,year_proj]         <- stfQuant(FLCore::mat(om$stks[[x]]), 
                                                       method  = method, 
                                                       samples = wts_samples,
                                                       ni      = ni)
    }
    
    ## If object is FLBiol, then manually fill 'mat' and 'fec' slots
    if (class(om$stks[[x]]) == "FLBiol") {
      if (method == "resample") {
        om$stks[[x]]@wt[,year_proj] <- stfQuant(FLCore::wt(om$stks[[x]])[,year_wts], 
                                                method  = method, 
                                                samples = wts_samples,
                                                ni      = ni)
        om$stks[[x]]@m[,year_proj]  <- stfQuant(FLCore::m(om$stks[[x]])[,year_wts],  
                                                method  = method, 
                                                samples = wts_samples,
                                                ni      = ni)
      }
      
      ## fill mat
      mat(om$stks[[x]])[,year_proj] <- stfQuant(mat(om$stks[[x]])[,year_wts], 
                                                method  = method, 
                                                samples = wts_samples,
                                                ni      = ni)
      
      ## fill fec
      fec(om$stks[[x]])[,year_proj] <- stfQuant(fec(om$stks[[x]])[,year_wts], 
                                                method  = method, 
                                                samples = wts_samples,
                                                ni      = ni)
    }
  }
  
  # ----------------------------------------------------------------------------
  # Process FLFisheries
  # ----------------------------------------------------------------------------
  
  for (x in names(om$flts)) {
    
    ## print fleet name
    if (verbose) 
      cat("\n", x, "\n")
    
    ## extract fishery object
    fishery <- om$flts[[x]]
    
    ## extract stock names
    stknames <- names(fishery)
    
    ## extend FLFishery
    fishery <- window(fishery, end = ydata+nyears)
    
    ## extend FLCatch - needs to be done simultaneously
    om$flts[[x]] <- FLFishery::FLFishery(
      # FLFishery::FLCatches(
      lapply(names(fishery), function(y){
        
        ## print stock name
        if (verbose) 
          cat(y, " ; ")
        
        ## extend FLQuants
        stkflt_ext <- window(fishery[[y]], end = ydata+nyears)
        
        # Weights and catch selection at age
        # ------------------------------------#
        
        ## loop over slots and calculate mean
        FLFishery::landings.wt(stkflt_ext)[,year_proj] <- stfQuant(landings.wt(fishery[[y]])[,year_wts], 
                                                                   method  = method, 
                                                                   samples = wts_samples,
                                                                   ni      = ni)
        FLFishery::discards.wt(stkflt_ext)[,year_proj] <- stfQuant(discards.wt(fishery[[y]])[,year_wts], 
                                                                   method  = method, 
                                                                   samples = wts_samples,
                                                                   ni      = ni)
        FLFishery::catch.sel(stkflt_ext)  [,year_proj] <- stfQuant(catch.sel(fishery[[y]])[,year_sel],   
                                                                   method  = method, 
                                                                   samples = sel_samples,
                                                                   ni      = ni)
        
        ## calculate landings and discards fractions
        ## and populate landings.n and discards.n
        landfraction <- stfQuant(FLCore::landings.n(fishery[[y]])[,year_wts] / FLCore::catch.n(fishery[[y]])[,year_wts],
                                 method  = method, 
                                 samples = wts_samples,
                                 ni      = ni)
        
        ## in cases where there are zero landings and discards resulting in NA
        ## set to zero
        landfraction[is.na(landfraction)] <- 0
        
        FLCore::landings.n(stkflt_ext)[,year_proj] <- landfraction
        FLCore::discards.n(stkflt_ext)[,year_proj] <- 1- landfraction
        
        # Catchability
        # ---------------#
        
        ## The catch.q FLPar may need to be propagated manually
        if(dims(stkflt_ext)$iter > 1 & 
           (dims(catch.q(stkflt_ext))$iter != dims(stkflt_ext)$iter)) {
          
          catch.q(stkflt_ext) <- propagate(catch.q(stkflt_ext), 
                                           iter = dims(stkflt_ext)$iter)
        } 
        
        ## The catch.q FLPar may need to be extended manually
        if (dim(catch.q(stkflt_ext))["year"] != dims(stkflt_ext)$year) {
          
          ## generate new FLPar with correct year extent
          catchq_ext <- FLPar(0, 
                              dimnames=list(params=c('alpha','beta'), 
                                            year = dimnames(stkflt_ext)$year, 
                                            iter = dimnames(stkflt_ext)$iter),
                              units='NA')
          ## insert historic data
          catchq_ext[,dimnames(catch.q(stkflt_ext))$year] <- catch.q(stkflt_ext)
          
          ## overwrite existing FLPar
          catch.q(stkflt_ext) <- catchq_ext
        }
        
        ## propagate "catchq" attribute if needed
        # if (dims(stkflt_ext)$iter > 1 & 
        #     (dims(attr(fishery[[y]],"catchq"))$iter != dims(stkflt_ext)$iter)) {
        #   
        #   attr(fishery[[y]],"catchq") <- propagate(attr(fishery[[y]],"catchq"),
        #                                            iter = dims(stkflt_ext)$iter)
        # }
        
        if (method == "yearMeans") {
          catchq_new <- apply(catch.q(stkflt_ext)["alpha",year_sel], c(1,3), mean)
          
          ## This is a really dumb way of doing this... but can't think of a better
          ## method right now
          for(yy in year_proj) {
            
            catch.q(stkflt_ext)["alpha", yy] <- catchq_new
            
          } # END Loop over proj years
        } # END if "yearMeans"
        
        if (method == "resample") {
          
          catchq_new <- lapply(1:ni, function(x) {
            xx <- ((x-1)* (length(sel_samples)/ni)) + (1:(length(sel_samples)/ni))
            return(c(catch.q(stkflt_ext)["alpha", sel_samples[xx], x]))
          })
          catchq_new <- unlist(catchq_new)
          catch.q(stkflt_ext)["alpha", year_proj] <- catchq_new
          
        } # END if "resample"
        
        # Quota-share
        # ---------------#
      
        ## Handle future quotashare - extend FLQuant
        if(!is.null(attr(fishery[[y]],"quotashare"))) {
          
          ## propagate "quotashare" attribute if needed
          if(dims(stkflt_ext)$iter > 1 & 
             (dims(attr(fishery[[y]],"quotashare"))$iter != dims(stkflt_ext)$iter)) {
            
            attr(fishery[[y]],"quotashare") <- propagate(attr(fishery[[y]],"quotashare"),
                                                         iter = dims(stkflt_ext)$iter)
          }
          
          ## extend quota shares
          attr(stkflt_ext,"quotashare") <- 
            window(attr(fishery[[y]],"quotashare"), end = (ydata+nyears))
          
          # Remove NAs to handle cases where no quota-share is recorded in a given year.
          
          ## estimate future quota share
          attr(stkflt_ext,"quotashare")[,year_proj] <- 
            stfQuant(attr(fishery[[y]],"quotashare")[,year_qs], 
                     method  = method, 
                     samples = qs_samples,
                     ni      = ni)
          
        } else {
          warning("attr 'quotashare' for fishery ",x," and stock ",y," does not exist!")
        }
        
        return(stkflt_ext)
      }) # end lapply
      #) # close FLCatches
    ) # close FLFishery
    
    # -------------------------#
    # Insert fishery-level data
    # -------------------------#
    
    ## Insert capacity. If NA replace with 1
    capacity(om$flts[[x]]) <- capacity(fishery)
    capacity(om$flts[[x]])[,year_proj] <- stfQuant(capacity(fishery)[,year_sel], 
                                                   method  = method, 
                                                   samples = sel_samples,
                                                   ni      = ni)
    capacity(om$flts[[x]])[is.na(capacity(om$flts[[x]]))] <- 1
    
    ## Insert effort
    effort(om$flts[[x]]) <- fishery@effort
    effort(om$flts[[x]])[,year_proj] <- stfQuant(fishery@effort[,year_sel], 
                                                 method  = method, 
                                                 samples = sel_samples,
                                                 ni      = ni)
    
    # hperiod(stkfltAll$flts[[x]])  <- hperiod(fishery)
    # vcost(stkfltAll$flts[[x]])    <- vcost(fishery)
    # fcost(stkfltAll$flts[[x]])    <- fcost(fishery)
    orevenue(om$flts[[x]]) <- orevenue(fishery)
    
    ## Insert names
    names(om$flts[[x]]) <- stknames
    
  }
  
  # ----------------------------------------------------------------------------
  # (OPTIONAL) Process FLIndices
  # ----------------------------------------------------------------------------
  #
  # The stf function within FLasher also does not work with FLIndex objects. We 
  # will need to extend these manually. I should use the same reference years as 
  # commercial catch selectivity.
  # 
  # slots to be extended are: index.q
  
  if (!is.null(om$idxs)) {
    for(x in 1:length(om$idxs)) {
      
      om$idxs[[x]] <- window(om$idxs[[x]], end = ydata+nyears)
      
      for(y in 1:length(om$idxs[[x]])) {
        om$idxs[[x]][[y]]@index.q[,year_proj,,,,] <- stfQuant(om$idxs[[x]][[y]]@index.q[,year_sel], 
                                                              method  = method, 
                                                              samples = sel_samples,
                                                              ni      = ni)
      }
    }
  }
  
  return(om)
}

#' Function to handle different methods of extending stock/fleet properties to future years
#' ----------------------------------------------------------------------------------------

stfQuant <- function(object, method, samples = NULL, ni = NULL) {
  
  if (method == "yearMeans") {
    return(yearMeans(object))
  }
  if (method == "resample") {
    vec <- lapply(1:ni, function(x) {
      
      xx <- ((x-1)* (length(samples)/ni)) + (1:(length(samples)/ni))
      return(c(object[, samples[xx], , , , x]))
    })
    return(unlist(vec))
  }
}