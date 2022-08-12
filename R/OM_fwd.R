# ---
# title: 'Operating Model projction methods'
# ---
#
# Summary
# =======
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

fwd_MIME <- function(om,                       # FLBiols/FLFisheries
                     ctrl,                     # Control object
                     sr_residuals_mult = TRUE, # are stock recruitment residuals multiplicative?
                     effort_max = 100,        # maximum allowed fishing effort
                     proc_res = NULL,          # where is process error noise stored?
                     ...) {

  # CURRENTLY ASSUMES THAT PROJECTION IS A SINGLE YEAR... PROBABLY UNAVOIDABLE
  # BECAUSE WE NEED TO RECALCULATE EFFORT FOR EACH TIMESTEP (EVEN IF ADVICE
  # DOES NOT CHANGE...)
  # NEED TO THINK ABOUT HOW TO HANDLE MULTI-ANNUAL ADVICE...

  # FLasher DOES NOT ALLOW EFFORT TO BE ZERO. I WILL NEED TO EITHER REPLACE
  # FLasher OR BUILD A ROUTINE TO PROJECT ZERO EFFORT CASES.

  ## Extract advice format from control object
  adviceType <- ctrl$adviceType
  advice     <- ctrl$advice

  # ------------------------------------#
  # Prepare forward control object
  # ------------------------------------#

  ## Advice is a TAC per stock
  if(adviceType == "TAC") {

    ## generate simplified list to pass to optimiser
    omList <- FLBiols2List(om = om,
                           year = yr,
                           advice = advice,
                           useCpp = TRUE)

    ## optimise effort
    effOptimised <- effortBaranov(omList = omList,
                                  adviceType = "catch",
                                  correctResid = TRUE)

    ## Extract effort parameters for each fleet
    pars <- sapply(1:ni, function(x) { effOptimised[[x]]$par})

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
           value = exp(pars[x,]))
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
    stop("Advice format (adviceType) must be 'f' or 'TAC'")
  }

  # ------------------------------------#
  # Project forward using FLasher::fwd
  # ------------------------------------#

  ## extend max effort for each stock
  effort_max <- rep(effort_max, length(om$flts))

  ## carry out projection
  om_fwd <- FLasher::fwd(object = om$stks,
                         fishery  = om$flts,
                         control  = flasher_ctrl,
                         effort_max = effort_max)

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

    }
  }

  # (Optional) Add process error noise if available
  #
  # This is uncertainty around the survival process (natural mortality)
  # Only affects stock numbers

  if(!is.null(proc_res)){


  }

  ## return projected stock
  return(om)

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
