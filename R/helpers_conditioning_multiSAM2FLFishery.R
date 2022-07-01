# ---
# title: 'Functions to help condition Operating Models'
# author: 'Matthew Pace'
# date: 'July 2022'
# ---
#
#' Convert a SAM fitted object into an \code{FLFishery} object
#'
#' This function takes a SAM fitted stock assessment object as input and
#' returns a \code{FLFishery} object.
#'
#' Returned \code{FLFishery} object contains data for:
#' \itemize{
#' \item partial fishing mortality-at-age (\code{catch.q})
#' \item landings numbers-at-age (\code{landings.n})
#' \item landings mean weight-at-age (\code{landings.wt})
#' \item proportion catch retained-at-age (\code{landings.sel})
#' \item discards numbers-at-age (\code{discards.n})
#' \item discards mean weight-at-age (\code{discards.wt})}
#'
#' Fbar range is ignored because it does not provide meaningful information
#' at the partial fishing mortality level.
#'
#' NOTE: The partial fishing mortality-at-age for each stock is inserted in
#'       the respective catchability-at-age slot. Further processing into
#'       catchability-at-age is needed.
#'
#' @param SAMfit SAM fitted stock assessment model object of class \code{sam}
#' @param stkname Character. stock name associated with SAM object.
#' @param useSAMcatch Optional argument. If \code{TRUE}, the fitted catches estimated
#'                    by SAM are used. Otherwise, the observed catches are used.
#'                    Defaults to \code{TRUE}.
#' @param add Optional argument. Defaults to \code{FALSE}. If \code{TRUE},
#'            \code{FLCatch} information is appended onto an existing \code{FLFishery}
#'            structure
#'
#' @return An \code{FLFishery} object
#'
#' @section  Warning:
#' This function requires \code{FLCore} to operate.
#'
#' @export

multiSAM2FLFleet <- function(SAMfit,
                             stkname = NULL,
                             useSAMcatch = TRUE,
                             add = FALSE,
                             fleets = NULL) {

  # ==================================#
  # Check that inputs are correct
  # ==================================#

  ## Stock name must be provided

  ## If add == TRUE, then FLFleets object must be provided
  if(is.null(fleets) & add == TRUE)
    stop("If add == TRUE, fleets must be provided as FLFleets object")

  ## If fleets are defined, fleet names must match fleets in SAMfit


  ## I MIGHT NEED TO DO SOME MORE SOPHISTICATED CHECKS

  # ==================================#
  # SECTION 1: Define stock dimensions
  # ==================================#

  ## Extract dimensions
  ages       <- SAMfit$conf$minAge:SAMfit$conf$maxAge
  years      <- SAMfit$data$years
  fbar_range <- SAMfit$conf$fbarRange

  ## Generate empty FLQuant object
  qnt <- FLCore::FLQuant(NA, dimnames = list(year = years,
                                             age  = ages,
                                             iter = 1))

  ## How many commercial fleets?
  fleet_idx  <- which(SAMfit$data$fleetTypes %in% c(0,1,7))
  fleet_n    <- length(fleet_idx)
  fleetnames <- attr(SAMfit$data, "fleetNames")[fleet_idx]

  # =====================================#
  # SECTION 2: Create or Append FLFleets
  # =====================================#

  ## Generate new structure if add == FALSE
  if(add == FALSE) {

    ## loop over each fleet
    fleets <- FLFleets(lapply(1:fleet_n, function(x){

      ## min and max ages harvested by each fleet
      # SAMfit$data$maxAgePerFleet
      # SAMfit$data$minAgePerFleet

      ## Create a blank FLFleet structure containing FLMetier and FLCatches
      fleet <- FLFleet::FLFleet(FLMetier(name = "mtr", catches = FLCatches(FLCatch(qnt))))

      ## Define FLCatch stock name
      fleet@metiers$met@catches@names <- stkname

      ## Return fleets
      return(fleet)

    }))

    fleets@names <- fleetnames

  } else if(add == TRUE){

    ##
    fleets

    ##

  } else {

    stop("'add' must be TRUE or FALSE")

  }

  # =====================================#
  # SECTION 3:   Fill FLCatch slots
  # =====================================#
  # SECTION 3.1: Landings fraction-at-age
  # -------------------------------------#



  # -------------------------------------#
  # SECTION 3.2: Landings numbers-at-age
  #              Discards numbers-at-age
  # -------------------------------------#



  # -----------------------------------------#
  # SECTION 3.3: Landings mean weight-at-age
  #              Discards mean weight-at-age
  # -----------------------------------------#



  # -----------------------------------------------#
  # SECTION 3.4: Update landings and discards slot
  # -----------------------------------------------#

  computeLandings()
  computeDiscards()

  return(fleets)
}
