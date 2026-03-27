# ---
# title: 'Functions to extend year dimensions - stf_fisheries'
# author: 'Matthew Pace'
# date: 'March 2026'
# ---
#
#' Function to extend `FLFisheries`
#' 
#' @param fisheries \code{FLFisheries} containing the relevant operating model fleet information.
#' @param method Character. The method to estimate future biological and fishery parameters. See details.
#' @param nyears Integer. The number of projection years to extend each fleet and stock slot.
#' @param wts_samples Integer vector. The sampled vector of year indexes from which parameters should be drawn.
#' @param wts_year Integer vector. The years to be sampled using `wts_samples`.
#' @param years_proj Integer vector. The projection years to be populated.
#' @param ni Integer. The number of iterations.
#' @param verbose Logical. Should progress be reported? Defaults to \code{TRUE}.
#' @param seed (Optional) Integer. Random seed for reproducibility if using RNG.
#' 
#' @returns \code{FLBiols} with projection years and estimated stock parameters.

stf_fisheries <- function(fisheries,
                          fisheries_ref = NULL,
                          method,
                          basis = NULL,
                          fleets = NULL,
                          nyears,
                          ydata,
                          wts_samples,
                          wts_year,
                          sel_samples,
                          sel_year,
                          qs_samples,
                          qs_year,
                          year_proj,
                          ni,
                          verbose) {
  
  ## do parameters
  null_ref <- is.null(fisheries_ref)
  if (!null_ref) {
    do_landings_wts <- "landings.wt"  %in% basis
    do_discards_wts <- "discards.wt"  %in% basis
    do_catchsel     <- "catch.sel"    %in% basis
    do_landfrac     <- "landfrac"     %in% basis
    do_catchq       <- "catch.q"      %in% basis
    do_effshare     <- "effort-share" %in% basis
  }
  
  ## Loop over fleets
  fisheries0 <- lapply(names(fisheries), function(x) {
    
    ## print fleet name
    if (verbose) 
      cat("\n", x, "\n")
    
    ## do fleet
    do_fleet <- x %in% fleets
    
    ## extract fishery object
    fishery <- fisheries[[x]]
    
    ## extract stock names
    stknames <- names(fishery)
    
    ## extend FLFishery
    fishery <- window(fishery, end = ydata+nyears)
    
    ## extend FLCatch - needs to be done simultaneously
    fishery@.Data <- 
      # FLFishery::FLCatches(
      lapply(names(fishery), function(y){
        
        ## print stock name
        if (verbose) 
          cat(y, " ; ")
        
        ## extend FLQuants
        stkflt_ext <- window(fishery[[y]], end = ydata+nyears)
        
        # Weights and catch selection at age
        # ------------------------------------#
        
        ## landings weights
        if (null_ref || (do_landings_wts & do_fleet)) {
          FLFishery::landings.wt(stkflt_ext)[,year_proj] <- stfQuant(landings.wt(fishery[[y]])[,wts_year], 
                                                                     method  = method, 
                                                                     samples = wts_samples,
                                                                     ni      = ni, ignoreZero = TRUE)
        } else {
          FLFishery::landings.wt(stkflt_ext)[,year_proj] <- fisheries_ref[[x]][[y]]@landings.wt[,year_proj]
        }
        
        ## discards weights
        if (null_ref || (do_discards_wts & do_fleet)) {
          FLFishery::discards.wt(stkflt_ext)[,year_proj] <- stfQuant(discards.wt(fishery[[y]])[,wts_year], 
                                                                     method  = method, 
                                                                     samples = wts_samples,
                                                                     ni      = ni, ignoreZero = TRUE)
        } else {
          FLFishery::discards.wt(stkflt_ext)[,year_proj] <- fisheries_ref[[x]][[y]]@discards.wt[,year_proj]
        }

        ## catch selection at age        
        if (null_ref || (do_catchsel & do_fleet)) {
          FLFishery::catch.sel(stkflt_ext)  [,year_proj] <- stfQuant(catch.sel(fishery[[y]])[,sel_year],   
                                                                     method  = method, 
                                                                     samples = sel_samples,
                                                                     ni      = ni, ignoreAllZero = TRUE)
        } else {
          FLFishery::catch.sel(stkflt_ext)  [,year_proj] <- fisheries_ref[[x]][[y]]@catch.sel[,year_proj]
        }

        ## calculate landings and discards fractions
        ## and populate landings.n and discards.n
                
        ## landings fraction
        if (null_ref || (do_landfrac & do_fleet)) {

          landfraction <- stfQuant(FLCore::landings.n(fishery[[y]])[,wts_year] / FLCore::catch.n(fishery[[y]])[,wts_year],
                                   method  = method, 
                                   samples = wts_samples,
                                   ni      = ni)
        } else {
          landfraction <- fisheries_ref[[x]][[y]]@landings.n[,year_proj]/catch.n(fisheries_ref[[x]][[y]])[,year_proj]
        }
        
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
          
          ## Generate updated dimensions
          dnames <- dimnames(fishery[[y]]@catch.q)
          posyr  <- match("year",names(fishery[[y]]@catch.q))
          dnames[[posyr]] <- dimnames(stkflt_ext)$year
          
          ## generate new FLPar with correct year extent
          catchq_ext <- FLPar(0, 
                              dimnames=dnames,
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
          catchq_new <- apply(catch.q(stkflt_ext)["alpha",sel_year], c(1,3), mean)
          
          ## This is a really dumb way of doing this... but can't think of a better
          ## method right now
          for(yy in year_proj) {
            
            catch.q(stkflt_ext)["alpha", yy] <- catchq_new
            
          } # END Loop over proj years
        } # END if "yearMeans"
        
        if (method == "resample") {
          
          catchq_new <- lapply(1:ni, function(x) {
            
            ## Get year from samples vector
            xx <- ((x-1)* (length(sel_samples)/ni)) + (1:(length(sel_samples)/ni))
            
            ## Get dimensions to read from
            dnames <- dimnames(catch.q(stkflt_ext))
            dnames$year <- sel_samples[xx]
            dnames$iter <- x
            posv   <- c(match("year", names(catch.q(stkflt_ext))),
                        match("area", names(catch.q(stkflt_ext))),
                        match("iter", names(catch.q(stkflt_ext))))
            posv <- posv[!is.na(posv)]
            names(dnames) <- c('i', c('i', 'j', 'k', 'l', 'm', 'n')[posv])
            
            return(c(do.call('[', c(list(x = catch.q(stkflt_ext)), dnames))))
            # return(c(catch.q(stkflt_ext)["alpha", sel_samples[xx], x]))
          })
          # catchq_new <- unlist(catchq_new)
          # catch.q(stkflt_ext)["alpha", year_proj] <- catchq_new
          
          dnames <- dimnames(catch.q(stkflt_ext))
          dnames$year <- year_proj
          posv   <- c(match("year", names(catch.q(stkflt_ext))),
                      match("area", names(catch.q(stkflt_ext))),
                      match("iter", names(catch.q(stkflt_ext))))
          posv <- posv[!is.na(posv)]
          names(dnames) <- c('i', c('i', 'j', 'k', 'l', 'm', 'n')[posv])
          
          catch.q(stkflt_ext) <- do.call('[<-', c(list(x     = catch.q(stkflt_ext), 
                                                       value = unlist(catchq_new)),
                                                  dnames))
        } # END if "resample"
        
        ## if catch.q is not in basis, then use true values
        if(!null_ref) {
          if (!(do_catchq & do_fleet)) {
            catch.q(stkflt_ext)["alpha", year_proj] <- fisheries_ref[[x]][[y]]@catch.q["alpha", year_proj]
          }
        }
        
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
            stfQuant(attr(fishery[[y]],"quotashare")[,qs_year], 
                     method  = method, 
                     samples = qs_samples,
                     ni      = ni)
          
        } else {
          warning("attr 'quotashare' for fishery ",x," and stock ",y," does not exist!")
        }
        
        return(stkflt_ext)
      }) # end lapply
    #) # close FLCatches
    
    # -------------------------#
    # Insert fishery-level data
    # -------------------------#
    
    ## Insert capacity. If NA replace with 1
    capacity(fishery)[,year_proj] <- stfQuant(capacity(fishery)[,sel_year], 
                                              method  = method, 
                                              samples = sel_samples,
                                              ni      = ni)
    capacity(fishery)[is.na(capacity(fisheries[[x]]))] <- 1
    
    ## Insert effort
    if (null_ref || (do_effshare & do_fleet)) {
      effort(fishery)[,year_proj] <- stfQuant(fishery@effort[,sel_year], 
                                              method  = method, 
                                              samples = sel_samples,
                                              ni      = ni)
    } else {
      effort(fishery)[,year_proj] <- fisheries_ref[[x]]@effort[,year_proj]
    }
    
    ## Insert hperiod
    hperiod(fishery)[,year_proj] <- stfQuant(hperiod(fishery)[,sel_year], 
                                             method  = method, 
                                             samples = sel_samples,
                                             ni      = ni)
    
    ## temporary check for comparison
    # fishery@range[c(3,4)] <- 1
    
    # hperiod(stkfltAll$flts[[x]])  <- hperiod(fishery)
    # vcost(stkfltAll$flts[[x]])    <- vcost(fishery)
    # fcost(stkfltAll$flts[[x]])    <- fcost(fishery)
    
    ## Insert names
    names(fishery) <- stknames
    fishery@name <- x
    
    return(fishery)
  })
  names(fisheries0) <- names(fisheries)
  FLFisheries(fisheries0)
  
}