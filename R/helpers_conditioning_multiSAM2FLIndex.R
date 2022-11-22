# ---
# title: 'Functions to help condition Operating Models'
# author: 'Matthew Pace'
# date: 'November 2022'
# ---
#
#' Extract survey data from a SAM fitted object to an \code{FLIndices} object
#'
#' Function takes a fitted SAM object as input and returns an \code{FLIndices}
#' object containing the observed survey index values and the fitted
#' stock catchability.
#'
#' @param SAMfit
#' @param yearRange
#'
#' @return an \code{FLIndices} object
#'
#' @export

multiSAM2FLIndex <- function(SAMfit,
                             yearRange) {

  # ------------------------------#
  # 1. Extract observed indices
  # 2. Extract survey catchability
  # ------------------------------#

  ## Some checks
  if(class(SAMfit) != "sam")
    stop("'SAMfit' must be class 'sam'")

  # ===================================#
  # SECTION 1: Extract observed indices
  # ===================================#

  ## Extract dimensions
  years <- SAMfit$data$years

  ### survey fleet index/indices
  survey_fleets <- which(SAMfit$data$fleetTypes %in% c(2,3))
  survey_names  <- attr(SAMfit$data, "fleetNames")[survey_fleets]
  survey_desc <- SAMfit$data$aux

  ## convert year from index to actual year - only do this if the max index
  ## year vector matches actual year vector length
  if(!(min(survey_desc[,"year"]) %in% years) &
     (max(survey_desc[,"year"]) == length(years))) {

    ## update year index to actual year
    survey_desc[,"year"] <- survey_desc[,"year"] + min(years) - 1

  }

  ## Combine observed numbers and year, fleet and age indices
  surveyn <- cbind(survey_desc,
                   index = exp(SAMfit$data$logobs))

  ## Loop over each survey fleet
  idxs <- FLCore::FLIndices(lapply(survey_fleets, function(i) {

    ## subset for the i'th survey fleet
    surveyi <- surveyn[surveyn[,"fleet"] %in% i,]

    ## extract year and age vector
    agei <- unique(surveyi[,"age"])
    yeari <- unique(surveyi[,"year"])

    # Next re-organise the year index and create an empty matrix to cater for cases
    # where there are missing data

    ## Generate blank matrix which has full age and year dimensions
    Smatrix <- matrix(nrow = length(agei),
                      ncol = length(yeari),
                      dimnames = list(age = agei,
                                      year = yeari))

    ## insert catches into blank matrix
    Smatrix[cbind(ac(surveyi[,"age"]), ac(surveyi[,"year"]))] <- surveyi[,"index"]
    Smatrix[is.na(Smatrix)] <- 0

    ## Generate empty FLQuant object
    qnt <- FLCore::FLQuant(NA, dimnames = list(year = yeari,
                                               age  = agei,
                                               iter = 1))
    ## Generate empty FLIndex object
    idx <- FLCore::FLIndex(qnt)

    ## Fill index slot with values
    FLCore::index(idx)[ac(agei),ac(yeari)] <- Smatrix

    return(idx)

  }))

  ## update fleet names
  names(idxs) <- survey_names

  # ======================================#
  # SECTION 2: Extract survey catchability
  # ======================================#



  return(idxs)
}
