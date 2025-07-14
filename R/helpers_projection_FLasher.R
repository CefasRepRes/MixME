#' Project stock forward one year using FLasher
#' 
#' Function uses the \code{FLasher} package to carry out a short-term forecast.
#' 
#' @param om Operating model
#' @param tracking Tracking object
#' @param pars Numeric vector. Vector of optimised fleet efforts
#' @param yr Integer. year
#' @param effort_max Maximum allowed fleet effort
#' @param sr_residuals (Optional) Stock-Recuitment noise
#' @param proc_res (Optional) Process error
#' 
#' @return A named list containing the operating model and tracking object.

projectFLasher <- function(om, 
                           tracking,
                           pars,
                           yr,
                           effort_max,
                           sr_residuals = NULL,
                           proc_res = NULL,
                           testfwd,
                           adviceType) {
  
  # -------------------------------------------------------------------------#
  # Prepare forward control object
  # -------------------------------------------------------------------------#
  
  ## Generate FCB matrix
  fcb <- makeFCB(biols = om$stks, flts = om$flts)
  
  # I need to project forward 2 year to get spawning stock biomass as the beginning
  # of the second year to input to the effort optimiser. However, in the final
  # year of the simulation I want to only project 1 year and extract the resulting
  # fleet efforts and catches
  
  ## maximum year to use in projection
  maxyr <- ifelse(yr < dims(om$stks[[1]])$maxyear, yr+1, yr)
  
  ## Generate arguments for effort-based forecast control
  ctrlArgs <- lapply(1:nrow(pars), function(x) {
    list(year = yr:maxyr,
         quant = "effort",
         fishery = names(om$flts)[x],
         value = rep(exp(pars[x,]), each = length(yr:maxyr)))
  })
  ctrlArgs$FCB <- fcb
  
  ## Generate effort-based FLasher::fwd forecast control
  flasher_ctrl <- do.call(FLasher::fwdControl, ctrlArgs)
  
  # ===========================================================================#
  # Project forward using FLasher::fwd
  # ===========================================================================#
  
  ## extend max effort for each stock
  effort_max <- rep(effort_max, length(om$flts))
  
  ## combine arguments
  fwdArgs <- list(object = om$stks,
                  fishery  = om$flts,
                  control  = flasher_ctrl,
                  effort_max = effort_max)
  
  if (!is.null(sr_residuals)) {
    fwdArgs$deviances <- sr_residuals
  }
  
  ## carry out projection
  om_fwd <- do.call(FLasher::fwd, fwdArgs)
  
  ## add process error if supplied
  if(!is.null(proc_res)) {
    for(s in names(proc_res)){
      ## implement process error
      FLCore::n(om_fwd$biols[[s]])[,ac(yr+1)] <- FLCore::n(om_fwd$biols[[s]])[,ac(yr+1)] * proc_res[[s]][, ac(yr+1)]
    }
  }
  
  # ===========================================================================#
  # Update Operating Model slots
  # ===========================================================================#
  
  ## extract FLBiols results
  if(yr < dims(om$stks[[1]])$maxyear) {
    for(s in names(om_fwd$biols)) {
      om$stks[[s]]@n[,ac(yr+1)] <- om_fwd$biols[[s]]@n[,ac(yr+1)]
    }
  }
  
  ## define list of fleet catches
  if(testfwd == TRUE) {
    
    fleetcatches <- lapply(om$flts, names)
    
    ## extract FLFisheries results
    for(f in names(om_fwd$fisheries)){
      
      om$flts[[f]]@effort[,ac(yr)] <- om_fwd$fisheries[[f]]@effort[,ac(yr)]
      
      for(s in fleetcatches[[f]]){
        
        om$flts[[f]][[s]]@landings.n[,ac(yr)] <- om_fwd$fisheries[[f]][[s]]@landings.n[,ac(yr)]
        om$flts[[f]][[s]]@discards.n[,ac(yr)] <- om_fwd$fisheries[[f]][[s]]@discards.n[,ac(yr)]
        
        ## Advice type simply determines the basis of how over-quota catches
        ## are calculated. If advice is landings-based, then overquota discards will
        ## take landings mean weights-at-age. If advice is catch-based, then overquota
        ## discards will take catch mean weights-at-age.
        
        cat(f, "...", s, "\n")
        
        ## Check if any landings or discards are missing weight information.
        ## If so, throw error.
        if(any(landings.n(om_fwd$fisheries[[f]][[s]])[,ac(yr)] > 0 &
               landings.wt(om_fwd$fisheries[[f]][[s]])[,ac(yr)] == 0))
          stop(paste("In 'fwdMixME': for fleet ", f, " and stock ", s,
                     ", landings.n > 0 but 'landings.wt' is 0"))
        
        if(any(discards.n(om_fwd$fisheries[[f]][[s]])[,ac(yr)] > 0 &
               discards.wt(om_fwd$fisheries[[f]][[s]])[,ac(yr)] == 0))
          stop(paste("In 'fwdMixME': for fleet ", f, " and stock ", s,
                     ", discards.n > 0 but 'discards.wt' is 0"))
        
        ## If advice is landings based
        if(adviceType == "landings") {
          
          overquota_ind <-
            which(landings(om_fwd$fisheries[[f]][[s]])[,ac(yr)] > tracking$quota[s,f,ac(yr),])
          
          if(length(overquota_ind) > 0){
            
            ## If landings exceed quota, add excess to discards
            ## - which iterations?
            ## - calculate proportion of excess landings numbers to be removed
            ##   - overquota * (lbiomass-at-age/sum(lbiomass)) / lmeanweight-at-age
            ## - calculate proportion of discards numbers that are overquota (used to update discard weights)
            
            ## Calculate over-quota biomass
            overquota_qty <-
              (landings(om_fwd$fisheries[[f]][[s]])[,ac(yr)][overquota_ind] -
                 tracking$quota[s,f,ac(yr),][overquota_ind])
            
            # NOTE:
            # In reality I would expect the selection pattern to be more heavily
            # skewed towards larger fish than the within-quota landings fraction
            # but we don't have data to parameterise this.
            
            ## Calculate landings biomass distribution for relevant iterations
            landings_dist <- sweep((landings.wt(om_fwd$fisheries[[f]][[s]])[,ac(yr)][overquota_ind] %*%
                                      landings.n(om_fwd$fisheries[[f]][[s]])[,ac(yr)][overquota_ind]),
                                   c(2:6),
                                   (landings(om_fwd$fisheries[[f]][[s]])[,ac(yr)][overquota_ind]),
                                   "/")
            
            ## Calculate equivalent over-quota numbers
            overquota_num <- sweep(overquota_qty %*% landings_dist,
                                   c(1:6),
                                   landings.wt(om_fwd$fisheries[[f]][[s]])[,ac(yr)][overquota_ind],
                                   "/")
            
            ## update landings and discards numbers
            om$flts[[f]][[s]]@landings.n[,ac(yr)][overquota_ind] <-
              om$flts[[f]][[s]]@landings.n[,ac(yr)][overquota_ind] - overquota_num
            om$flts[[f]][[s]]@discards.n[,ac(yr)][overquota_ind] <-
              om$flts[[f]][[s]]@discards.n[,ac(yr)][overquota_ind] + overquota_num
            
            ## Update tracking object
            tracking$overquota[s,f, ac(yr),overquota_ind] <-
              quantSums(om$flts[[f]][[s]]@landings.wt[,ac(yr)][overquota_ind] %*% overquota_num)
            
            ## proportion of over-quota landings and discards numbers
            overdisc_prop <-
              sweep(overquota_num, c(1:6),
                    (discards.n(om_fwd$fisheries[[f]][[s]])[,ac(yr)][overquota_ind] +
                       overquota_num), "/")
            
            # Discards mean weight will now be higher because of discarding of
            # larger fish - calculate weighted mean for each age
            
            om$flts[[f]][[s]]@discards.wt[,ac(yr)][overquota_ind] <-
              om$flts[[f]][[s]]@discards.wt[,ac(yr)][overquota_ind] %*% (1- overdisc_prop) +
              om$flts[[f]][[s]]@landings.wt[,ac(yr)][overquota_ind] %*% (overdisc_prop)
            
          }
        }
        
        ## If advice is catch-based
        if(adviceType == "catch"){
          
          overquota_ind <- which(catch(om_fwd$fisheries[[f]][[s]])[,ac(yr)] >
                                   tracking$quota[s,f,ac(yr),])
          
          if(length(overquota_ind) > 0) {
            
            ## Calculate overquota biomass
            overquota_qty <-
              (catch(om_fwd$fisheries[[f]][[s]])[,ac(yr)][,,,,,overquota_ind] -
                 tracking$quota[s,f,ac(yr),][overquota_ind])
            
            ## Calculate landings biomass fraction for relevant iterations
            landings_frac <- sweep((landings.wt(om_fwd$fisheries[[f]][[s]])[,ac(yr)][,,,,,overquota_ind] %*%
                                      landings.n(om_fwd$fisheries[[f]][[s]])[,ac(yr)][,,,,,overquota_ind]),
                                   c(1:6),
                                   (catch.wt(om_fwd$fisheries[[f]][[s]])[,ac(yr)][,,,,,overquota_ind] %*%
                                      catch.n(om_fwd$fisheries[[f]][[s]])[,ac(yr)][,,,,,overquota_ind]),
                                   "/")
            
            ## Calculate landings biomass distribution for relevant iterations
            landings_dist <- sweep((landings.wt(om_fwd$fisheries[[f]][[s]])[,ac(yr)][,,,,,overquota_ind] %*%
                                      landings.n(om_fwd$fisheries[[f]][[s]])[,ac(yr)][,,,,,overquota_ind]),
                                   c(2:6),
                                   (landings(om_fwd$fisheries[[f]][[s]])[,ac(yr)][,,,,,overquota_ind]),
                                   "/")
            
            ## Calculate discards biomass distribution for relevant iterations
            discards_dist <- sweep((discards.wt(om_fwd$fisheries[[f]][[s]])[,ac(yr)][,,,,,overquota_ind] %*%
                                      discards.n(om_fwd$fisheries[[f]][[s]])[,ac(yr)][,,,,,overquota_ind]),
                                   c(2:6),
                                   (discards(om_fwd$fisheries[[f]][[s]])[,ac(yr)][,,,,,overquota_ind]),
                                   "/")
            
            ## Gneerate objects to hold overquota numbers
            overquota_landings_num <- landings.n(om_fwd$fisheries[[f]][[s]])[,ac(yr)] %=% 0
            overquota_discards_num <- discards.n(om_fwd$fisheries[[f]][[s]])[,ac(yr)] %=% 0
            
            ## Calculate overquota numbers
            overquota_landings_num[,,,,,overquota_ind] <-
              FLCore::FLQuant(
                sweep(sweep(landings_dist, c(2:6), overquota_qty, "*") * landings_frac,
                      c(1:6),
                      landings.wt(om_fwd$fisheries[[f]][[s]])[,ac(yr)][,,,,,overquota_ind],"/"))
            
            overquota_discards_num[,,,,,overquota_ind] <- FLCore::FLQuant(sweep(
              sweep(discards_dist, c(2:6), overquota_qty, "*") * (1 - landings_frac),
              c(1:6),
              discards.wt(om_fwd$fisheries[[f]][[s]])[,ac(yr)][,,,,,overquota_ind],
              "/"))
            
            ## If landings/discards weight is zero for any age, this will result in NaN.
            ## Replace these cases with zero numbers
            overquota_landings_num[landings.wt(om_fwd$fisheries[[f]][[s]])[,ac(yr)] == 0] <- 0
            overquota_discards_num[discards.wt(om_fwd$fisheries[[f]][[s]])[,ac(yr)] == 0] <- 0
            
            ## update landings and discards numbers
            om$flts[[f]][[s]]@landings.n[,ac(yr)][,,,,,overquota_ind] <-
              om$flts[[f]][[s]]@landings.n[,ac(yr)][,,,,,overquota_ind] -
              overquota_landings_num[,,,,,overquota_ind]
            
            om$flts[[f]][[s]]@discards.n[,ac(yr)][,,,,,overquota_ind] <-
              om$flts[[f]][[s]]@discards.n[,ac(yr)][,,,,,overquota_ind] +
              overquota_landings_num[,,,,,overquota_ind]
            
            # NOTE: I need to update the tracking object before I update discard
            #       mean weights-at-age
            
            ## Update tracking object
            tracking$overquota[s,f, ac(yr),overquota_ind] <-
              quantSums(om$flts[[f]][[s]]@discards.wt[,ac(yr)][,,,,,overquota_ind] *
                          overquota_discards_num[,,,,,overquota_ind] +
                          om$flts[[f]][[s]]@landings.wt[,ac(yr)][,,,,,overquota_ind] *
                          overquota_landings_num[,,,,,overquota_ind])
            
            ## Calculate proportion of overquota discards numbers
            overdisc_prop <- sweep(overquota_landings_num[,,,,,overquota_ind], c(1:6),
                                   om$flts[[f]][[s]]@discards.n[,ac(yr)][,,,,,overquota_ind], "/")
            
            ## Adjust discards mean weight-at-age
            om$flts[[f]][[s]]@discards.wt[,ac(yr)][,,,,,overquota_ind] <-
              om$flts[[f]][[s]]@discards.wt[,ac(yr)][,,,,,overquota_ind] * (1 - overdisc_prop) +
              om$flts[[f]][[s]]@landings.wt[,ac(yr)][,,,,,overquota_ind] * (overdisc_prop)
            
          }
        }
      }
    }
  }
  
  if(testfwd == FALSE) {
    
    ## extract FLFisheries results & calculate overquota catches
    out <- fwd_update_fleets(om = om, om_fwd = om_fwd, tracking = tracking, year = yr, adviceType = adviceType)
    
    om       <- out$om
    tracking <- out$tracking
    
  }
  
  return(list(om       = om,
              tracking = tracking))
}