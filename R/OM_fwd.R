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
                     args,
                     tracking,
                     adviceType = "catch",     # is advice basis TAC?
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

  ni <- dims(om$stks[[1]])$iter
  yr <- args$ay

  # ===========================================================================#
  # Process Advice
  # ===========================================================================#

  ## process advice to a different format -- VERY HACKY
  advice <- lapply(om$stks@names, function(x) {

    ## if control object contains more than 1 iteration, extract vector
    if(dim(ctrl[[x]]@iters)[3] > 1){
      ctrl[[x]]@iters[,,]["value",]

      ## Otherwise repeat elements to generate vector
    } else {

      ## HAVE A SMARTER WAY OF HANDLING MULTIPLE YEARS
      if (length(ctrl[[x]]@iters[,,][,"value"]) > 1) {
        rep(tail(ctrl[[x]]@iters[,,][,"value"],1), ni)
      } else {
        rep(ctrl[[x]]@iters[,,][,"value"], ni)
      }
    }
  })
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
    effOptimised <- effortBaranov(omList = omList,
                                  adviceType = adviceType,
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

      round(tracking$quota[,,ac(yr),x] - catchBaranov(par = exp(effOptimised[[x]]$par),
                                                      dat = omList[[x]],
                                                      adviceType = adviceType,
                                                      islog = FALSE),3)
    }, simplify = "array")

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

  ## extract FLFisheries results
  for(f in names(om_fwd$fisheries)){

    om$flts[[f]]@effort[,ac(yr)] <- om_fwd$fisheries[[f]]@effort[,ac(yr)]

    for(s in names(om_fwd$biols)){

      om$flts[[f]][[s]]@landings.n[,ac(yr)] <- om_fwd$fisheries[[f]][[s]]@landings.n[,ac(yr)]
      om$flts[[f]][[s]]@discards.n[,ac(yr)] <- om_fwd$fisheries[[f]][[s]]@discards.n[,ac(yr)]

      ## Advice type simply determines the basis of how over-quota catches
      ## are calculated. If advice is landings-based, then overquota discards will
      ## take landings mean weights-at-age. If advice is catch-based, then overquota
      ## discards will take catch mean weights-at-age.
      
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

  # (Optional) Add process error noise if available
  #
  # This is uncertainty around the survival process (natural mortality)
  # Only affects stock numbers

  if(!is.null(proc_res)){


  }

  # ===========================================================================#
  # Update tracking object
  # ===========================================================================#

  ## Update tracking object - True Stock Properties
  for(x in om$stks@names) {

    ## Update stock numbers
    tracking[[x]]$stk["B.om",  ac(yr)] <- quantSums(om$stks[[x]]@n[,ac(yr)] * om$stks[[x]]@wt[,ac(yr)])
    tracking[[x]]$stk["SB.om", ac(yr)] <- quantSums(om$stks[[x]]@n[,ac(yr)] * om$stks[[x]]@wt[,ac(yr)] * om$stks[[x]]@mat$mat[,ac(yr)])

    ## Calculate overall landings and discards
    fltlandings <- sapply(1:length(om$flts), function(y){
      landings(om$flts[[y]][[x]])[,ac(yr)]
    }, simplify = "array")

    fltdiscards <- sapply(1:length(om$flts), function(y){
      discards(om$flts[[y]][[x]])[,ac(yr)]
    }, simplify = "array")

    fltlandings <- apply(fltlandings, c(1:6), sum)
    fltdiscards <- apply(fltdiscards, c(1:6), sum)

    ## update landings, discards and catch numbers in tracking object
    tracking[[x]]$stk["L.om",  ac(yr)] <- fltlandings
    tracking[[x]]$stk["D.om",  ac(yr)] <- fltdiscards
    tracking[[x]]$stk["C.om",  ac(yr)] <- fltlandings + fltdiscards

    ## Update harvest
    fltFage <- sapply(1:length(om$flts), function(y){
      om$flts[[y]][[x]]@catch.q[1,] %*% om$flts[[y]]@effort[,ac(yr)] %*% om$flts[[y]][[x]]@catch.sel[,ac(yr)]
    }, simplify = "array")

    Fage <- apply(fltFage, c(1:6), sum)
    tracking[[x]]$stk["F.om",  ac(yr)] <- apply(Fage[ac(args$frange[[x]][1]:args$frange[[x]][2]),,,,,,drop = FALSE], c(2:6), mean)

    ## Save Selectivity
    tracking[[x]]$sel_om[,ac(yr)] <-  sweep(Fage, c(2:6), tracking[[x]]$stk["F.om",  ac(yr)], "/")
  }

  # ===========================================================================#
  # Return outputs
  # ===========================================================================#

  ## return projected stock
  return(list(om       = om,
              tracking = tracking))

}

# Automatic generation of FCB matrix for FLasher
#
# This function takes an FLBiols and FLFisheries object and generates an FCB
# matrix

makeFCB <- function(biols, flts){

  ## number of biols and fleets
  nbiols <- length(biols)
  nflts  <- length(flts)

  ## biols names
  biolnames <- names(biols)

  # Calculate the positions of: Fishery in Fisheries object
  #                             Catches in Fishery object
  #                             Biols caught by Catches object

  nums <- sapply(1:nflts, function(x){

    ## extract catch names from fleet object
    catchnames <- names(flts[[x]])

    ## position of catch name in fleet
    catchpos <- 1:length(catchnames)

    ## position of biol name that matches catch name
    biolspos <- sapply(1:length(catchnames), function(y){
      which(catchnames[y] == biolnames)
    })

    ## A hacky solution to get the numbers in the right order...
    c(matrix(c(rep(x, length(catchnames)),
               catchpos,
               biolspos),
             ncol = 3, byrow = TRUE))
  })

  ## calculate number of rows in fcb matrix
  nrows <- ncol(nums) * nrow(nums) /3

  ## Create matrix
  fcb <- matrix(c(nums),
                nrow = nrows, ncol = 3,
                dimnames = list(1:nrows, c("F", "C", "B")),
                byrow = TRUE)

  return(fcb)
}
