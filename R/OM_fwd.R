# ---
# title: 'Operating Model projection methods'
# author: 'Matthew Pace'
# date: 'August 2022'
# ---
#
#
#' Forward projection of stocks - including process error
#'
#' This function takes the output of the implementation error model and
#' projects the stocks and fleets forward one time step.
#'
#' @param om A list of \code{FLBiols} and \code{FLFisheries} containing the relevant
#'           stock and fleet information in the Operating Model.
#' @param ctrl Control object.
#' @param args     list of additional arguments
#' @param tracking a named list of tracking objects to monitor emergent dynamic
#'                 properties
#' @param sr_residuals_mult (Optional) Logical. Are stock recruitment residuals
#'                          multiplicative? Defaults to \code{TRUE}.
#' @param effort_max (Optional) Numeric value indicating the maximum allowed
#'                   fishing effort for any fleet. Defaults to 100 and
#'                   cannot be \code{NULL}.
#' @param proc_res (Optional) Character. Where is process error noise stored?
#'                 If \code{NULL}, no process error is applied to stock numbers.
#'                 Defaults to \code{NULL}.
#'
#' @return A list containing the \code{FLBiols} and \code{FLFisheries} objects
#'         with projected stock numbers, fleet efforts, and fleet-stock landings
#'         and discards numbers.
#'
#' @export

fwdMixME <- function(om,                       # FLBiols/FLFisheries
                     ctrl,                     # Control object
                     args,                     # Additional arguments
                     tracking,                 # Tracking object
                     sr_residuals_mult = TRUE, # are stock recruitment residuals multiplicative?
                     effort_max = 100,         # maximum allowed fishing effort
                     proc_res = NULL,          # where is process error noise stored?
                     ...) {

  # CURRENTLY ASSUMES THAT PROJECTION IS A SINGLE YEAR... PROBABLY UNAVOIDABLE
  # BECAUSE WE NEED TO RECALCULATE EFFORT FOR EACH TIMESTEP (EVEN IF ADVICE
  # DOES NOT CHANGE...)
  # NEED TO THINK ABOUT HOW TO HANDLE MULTI-ANNUAL ADVICE...

  # FLasher DOES NOT ALLOW EFFORT TO BE ZERO. I WILL NEED TO EITHER REPLACE
  # FLasher OR BUILD A ROUTINE TO PROJECT ZERO EFFORT CASES.

  # AT THE MOMENT, ADVICETYPE CAN ONLY TAKE A SINGLE VALUE - I WILL NEED TO ADAPT
  # THIS SO THAT DIFFERENT STOCKS CAN HAVE LANDINGS, CATCH OR F-BASED ADVICE.

  # ===========================================================================#
  # Extract Arguments
  # ===========================================================================#

  ni   <- dims(om$stks[[1]])$iter
  yr   <- args$ay
  iy   <- args$iy
  mlag <- args$management_lag
  
  ## Extract basis of advice
  if(!is.null(args$adviceType)) {
    adviceType <- args$adviceType
  } else {
    adviceType <- "catch"
  }
  
  ## How many times to retry if effort optimization does not identify choke-stocks?
  if(!is.null(args$maxRetry)) {
    maxRetry <- args$maxRetry
  } else {
    maxRetry <- 1
  }
  
  ## use R or c++ to update fleet catches - Default to c++ - DELETE THIS LATER
  if(!is.null(args$testfwd)) {
    testfwd <- args$testfwd
  } else {
    testfwd <- FALSE # use c++
  }
  
  ## use R or TMB to optimise fleet efforts - DELETE THIS LATER
  if(!is.null(args$useTMB)) {
    useTMB <- args$useTMB
  } else {
    useTMB <- TRUE
  }
  
  ## use last year effort as initial parameters? Default = FALSE
  if(!is.null(args$useEffortAsInit)) {
    useEffortAsInit <- args$useEffortAsInit
  } else {
    useEffortAsInit <- FALSE
  }
  
  # ===========================================================================#
  # Do not project if initial projection year with management lag
  # ===========================================================================#
  #
  # If it is the initial year and there is a management lag, we expect that there 
  # is already catch for the year and stock information for the next year provide
  # as part of the operating model conditioning. What is needed is the advice for
  # the next year and this has already been prepared in the previous modules.
  
  # Therefore, do not project the system. 
  
  if(yr == iy & mlag > 0) {
    
    ## update tracking object
    tracking <- updateTrackingOM(om = om, tracking = tracking, args = args, yr = yr)
    
    ## return unprojected stock
    return(list(om       = om,
                tracking = tracking))
    
  }

  
  # ===========================================================================#
  # Process Advice
  # ===========================================================================#
  
  ## extract advice from tracking object
  advice <- lapply(om$stks@names, function(x) {
    c(tracking[[x]]$advice[1,ac(yr),])
  })

  ## process advice to a different format -- VERY HACKY
  # advice <- lapply(om$stks@names, function(x) {
  # 
  #   ## if control object contains more than 1 iteration, extract vector
  #   if(dim(ctrl[[x]]@iters)[3] > 1){
  #     ctrl[[x]]@iters[,,]["value",]
  # 
  #     ## Otherwise repeat elements to generate vector
  #   } else {
  # 
  #     ## HAVE A SMARTER WAY OF HANDLING MULTIPLE YEARS
  #     if (length(ctrl[[x]]@iters[,"value",]) > 1) {
  #       rep(tail(ctrl[[x]]@iters[,"value",],1), ni)
  #     } else {
  #       rep(ctrl[[x]]@iters[,"value",], ni)
  #     }
  #   }
  # })
  names(advice) <- om$stks@names

  # ===========================================================================#
  # Advice implementation given mixed fisheries technical interactions
  # ===========================================================================#

  ## Advice is a TAC per stock
  if(adviceType %in% c("catch","landings")) {

    # -------------------------------------------------------------------------#
    # Reorganise data for optimisation
    # -------------------------------------------------------------------------#

    ## generate simplified list to pass to optimiser
    omList <- FLBiols2List(om = om,
                           year = yr,
                           advice = advice,
                           useCpp = TRUE)

    # -------------------------------------------------------------------------#
    # Optimise fleet activity
    # -------------------------------------------------------------------------#
    
    ## optimise effort
    effOptimised <- effortBaranov(omList       = omList,
                                  adviceType   = adviceType,
                                  maxRetry     = maxRetry,
                                  useEffortAsInit = useEffortAsInit,
                                  useTMB       = useTMB,
                                  correctResid = FALSE)

    ## Extract effort parameters for each fleet
    pars <- sapply(1:ni, function(x) { effOptimised[[x]]$par})

    # -------------------------------------------------------------------------#
    # TRACKING
    # -------------------------------------------------------------------------#

    ## save quota stock-fleet
    tracking$quota[,,ac(yr),] <- sapply(1:ni, function(y) {
      omList[[y]]$quota
    }, simplify = "array")

    ## save optimisation results to tracker
    tracking$optim[,ac(yr),] <- sapply(1:ni, function(x){
      c(effOptimised[[x]]$objective,
        effOptimised[[x]]$convergence)
    })

    ## save optimisation messages to tracker
    tracking$message[1,ac(yr),] <- sapply(1:ni, function(x){
      effOptimised[[x]]$message[1]
    })

    ## save re-scaling messages to tracker
    tracking$message[2,ac(yr),] <- sapply(1:ni, function(x){
      effOptimised[[x]]$message[2]
    })

    ## save quota uptake to tracker
    tracking$uptake[,,ac(yr),] <- sapply(1:ni, function(x){
      
      if(useTMB == TRUE) {
        tracking$quota[,,ac(yr),x] - effOptimised[[x]]$Cfleet
      } else {
        tracking$quota[,,ac(yr),x] - catchBaranov(par = exp(effOptimised[[x]]$par),
                                                  dat = omList[[x]],
                                                  adviceType = adviceType,
                                                  islog = FALSE)
      }

    }, simplify = "array")
    
    ## save choke stock vector to tracker
    tracking$choke[,ac(yr),] <- sapply(1:ni, function(x){
      effOptimised[[x]]$stkLim
    })

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
           value = rep(exp(pars[x,]), each = 2))
    })
    ctrlArgs$FCB <- fcb

    ## Generate effort-based FLasher::fwd forecast control
    flasher_ctrl <- do.call(FLasher::fwdControl, ctrlArgs)

  } else if(adviceType == "f") {

    # Technically, f-based advice is really effort restriction. The challenge is
    # work out how effort translates to F for each stock --- and what the least
    # allowable effort will be.
    #
    # How is f-based advice translated into allowable effort in practice? How is
    # effort allocated to different fleets?
    #
    # Would we expect the proportional activity across fleets to remain
    # constant? --- if so, we simply scale all efforts to meet the limiting F.

    stop("f-based advice not currently implemented")
  } else {
    stop("Advice format (adviceType) must be 'f', 'catch' or 'landings'")
  }

  # ===========================================================================#
  # Project forward using FLasher::fwd
  # ===========================================================================#

  ## extend max effort for each stock
  effort_max <- rep(effort_max, length(om$flts))

  ## carry out projection
  om_fwd <- FLasher::fwd(object = om$stks,
                         fishery  = om$flts,
                         control  = flasher_ctrl,
                         effort_max = effort_max)

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

  # (Optional) Add process error noise if available
  #
  # This is uncertainty around the survival process (natural mortality)
  # Only affects stock numbers

  if(!is.null(proc_res)){


  }

  # ===========================================================================#
  # Update tracking object
  # ===========================================================================#
  
  tracking <- updateTrackingOM(om = om, tracking = tracking, args = args, yr = yr)
  
  # ===========================================================================#
  # Return outputs
  # ===========================================================================#

  ## return projected stock
  return(list(om       = om,
              tracking = tracking))

}


#' Automatic generation of FCB matrix for FLasher
#'
#' This function takes an FLBiols and FLFisheries object and generates an FCB
#' matrix
#' 
#' @export

makeFCB <- function(biols, flts){

  ## number of biols and fleets
  nbiols <- length(biols)
  nflts  <- length(flts)

  ## biols names
  biolnames <- names(biols)

  # Calculate the positions of: Fishery in Fisheries object
  #                             Catches in Fishery object
  #                             Biols caught by Catches object

  nums <- lapply(1:nflts, function(x){

    ## extract catch names from fleet object
    catchnames <- names(flts[[x]])

    ## position of catch name in fleet
    catchpos <- 1:length(catchnames)

    ## position of biol name that matches catch name
    biolspos <- sapply(1:length(catchnames), function(y){
      which(catchnames[y] == biolnames)
    })
    
    ## Warning if any catches don't have equivalent biols positions
    if(is.list(biolspos)) {
      missingbiols <- catchnames[unlist(lapply(biolspos, function(x) length(x) == 0))]
      warning(paste0("in fleet ",
                     names(flts)[x],
                     ", catches do not have biols: ",
                     missingbiols,"\n"))
      
      for(i in which(unlist(lapply(biolspos, function(x) length(x) == 0)))) {
        biolspos[[i]] <- 0
      }
      biolspos <- unlist(biolspos)
    }

    ## A hacky solution to get the numbers in the right order...
    matrix(c(rep(x, length(catchnames)),
               catchpos,
               biolspos),
             ncol = 3, byrow = FALSE)
  })
  
  ## combine into single matrix
  fcb <- do.call(rbind, nums)
  
  ## remove rows with missing biols 
  fcb <- fcb[fcb[,3] > 0,]
  
  ## Define row and column names
  dimnames(fcb) <- list(1:nrow(fcb), c("F", "C", "B"))

  return(fcb)
}

