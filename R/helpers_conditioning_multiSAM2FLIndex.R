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
                             yearRange   = NULL,
                             uncertainty = FALSE,
                             niter       = 1000,
                             samVariates = NULL,
                             seed        = NULL) {

  # -----------------------------------------#
  # 1. Define dimensions and identify surveys
  #
  # 2. (Optional) Sample uncertainty from MVN distribution
  #
  # 3. Process each survey
  # 3.1 ... Extract observed indices
  # 3.2 ... Extract survey catchability
  # 3.3 ... (Optional) catchability uncertainty
  # 3.3.1 ... (Optional) extend stock year dimension to new range
  # 3.3.2 ... (Optional) extend stock iteration dimension and add uncertainty
  # 3.4 ... Survey timing
  # 3.5 ... Survey time-invariant standard deviation
  #
  # -----------------------------------------#

  ## Some checks
  if(class(SAMfit) != "sam")
    stop("'SAMfit' must be class 'sam'")

  ## stop if number of iterations are not provided
  if(uncertainty == TRUE & is.null(niter))
    stop("If 'uncertainty' is TRUE, the number of replicates 'niter' must be provided")

  # ===================================#
  # SECTION 1: Define dimensions and identify surveys
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

  # ===================================#
  # SECTION 2: (Optional) Sample uncertainty
  # ===================================#

  if(uncertainty == TRUE) {

    ## Sample variates from MVN distribution if none provided
    if(!is.null(samVariates)) {

      variates <- samVariates[[1]]

    } else {

      ## Set random number seed if provided
      if(!is.null(seed)) set.seed(seed)

      ## Calculate standard deviation of model parameters
      . <- capture.output(sds <- TMB::sdreport(obj = SAMfit$obj,
                                               par.fixed = SAMfit$opt$par,
                                               getJointPrecision = TRUE))

      ## Best-fit values for parameters
      est <- c(sds$par.fixed, sds$par.random)

      ## Variance-Covariance matrix of all model parameters
      cov <- solve(sds$jointPrecision)

      ## generate a number of random variates by sampling from a multivariate
      ## normal distribution
      variates <- stockassessment::rmvnorm((niter-1), est, cov) # col = parameters, row = replicates
      colnames(variates) <- names(est)

    }

  }

  # ===================================#
  # SECTION 3: Process each survey
  # ===================================#

  ## Loop over each survey fleet
  idxs <- FLCore::FLIndices(lapply(survey_fleets, 
                                   extract_idx,
                                   SAMfit      = SAMfit, 
                                               uncertainty = uncertainty, 
                                               variates    = variates, 
                                               niter       = niter, 
                                               surveyn     = surveyn, 
                                               yearRange   = yearRange))

  ## update fleet names
  names(idxs) <- survey_names
  
  # ===================================#
  # SECTION 4: return FLR outputs
  # ===================================#

  ## return stock survey indices
  return(idxs)
}


# Extract information associated with a single survey 
# ===================================================#
# loading into a separate function for easier debugging...

extract_idx <- function(i, 
                        SAMfit, 
                        uncertainty, 
                        variates, 
                        agei, 
                        yeari, 
                        niter, 
                        surveyn, 
                        yearRange) {
  
  # -------------------------------------#
  # SECTION 3.1: Extract observed indices
  # -------------------------------------#
  
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
  
  # ----------------------------------------#
  # SECTION 3.2: Extract survey catchability
  # ----------------------------------------#
  
  ## Generate an index to subset survey catchability
  surveyq_index <- SAMfit$conf$keyLogFpar[i,] + 1
  surveyq_index <- surveyq_index[surveyq_index > 0]
  
  ## extract fitted survey catchability
  surveyq <- exp(SAMfit$pl$logFpar)[surveyq_index]
  
  ## insert catchability
  FLCore::index.q(idx)[] <- surveyq
  
  # ------------------------------------------------#
  # SECTION 3.3: (Optional) catchability uncertainty
  # ------------------------------------------------#
  # SECTION 3.3.1: (Optional) extend stock year dimension to new range
  # -------------------------------------------------------------------#
  
  if(!is.null(yearRange)) {
    idx <- window(idx, end = yearRange[2])
  }
  
  # -------------------------------------------------------------------#
  # SECTION 3.3.2: (Optional) extend stock iteration dimension and add
  #                catchability uncertainty
  # -------------------------------------------------------------------#
  
  if(uncertainty == TRUE & niter > 1) {
    
    ## Extend stock iteration dimensions
    idx <- propagate(idx, niter)
    
    ## Fill index.q slot
    FLCore::index.q(idx)[,,,,,-1] <- 
      exp(t(variates[, colnames(variates) == "logFpar", 
                     drop = FALSE][, surveyq_index]))
    
  }
  
  # ------------------------------------------------#
  # SECTION 3.4: Survey timing
  # ------------------------------------------------#
  
  range(idx)[c("startf", "endf")] <- 
    SAMfit$data$sampleTimes[i]
  
  # -----------------------------------------------------#
  # SECTION 3.5: Survey time-invariant standard deviation
  # -----------------------------------------------------#
  # This is the standard deviation parameter of the log-normal distribution
  # for survey observations 
  
  ## generate single-year FLQuant
  qnt_sd <- FLCore::FLQuant(NA, dimnames = list(age  = agei,
                                                year = "all",
                                                iter = 1:niter))
  
  ## extract index for survey observation variance
  idx_sd <- SAMfit$conf$keyVarObs[i,] + 1
  idx_sd <- idx_sd[idx_sd > 0]
  
  ## insert maximum-likelihood values
  qnt_sd[,,,,,1] <- exp(SAMfit$pl$logSdLogObs)[idx_sd]
  
  ## (optional) insert sampled values
  if(uncertainty == TRUE) {
    qnt_sd[,,,,,-1] <- 
      exp(t(variates[, colnames(variates) == "logSdLogObs", 
                     drop = FALSE][, idx_sd]))
  }
  
  ## Attach as attribute
  attr(idx, "survey_sd") <- qnt_sd
  
  return(idx)
  
}