# ---
# title: 'Functions to help condition Operating Models'
# author: 'Matthew Pace'
# date: 'July 2022'
# ---
#
#' Convert SAM fitted object into \code{FLStocks},\code{FLFisheries} and
#' \code{FLIndices} objects
#'
#' This function takes a SAM fitted stock assessment object as input and
#' returns a list of \code{FLStocks}, \code{FLFisheries} and a nested list of
#' \code{FLIndices} objects.
#'
#' \code{FLStocks} contain one or more \code{FLStock} objects. Each
#' \code{FLStock} object contains data for a given stock in the following slots:
#' \itemize{
#'   \item stock numbers-at-age (\code{stock.n})
#'   \item catch numbers-at-age (\code{catch.n})
#'   \item landings numbers-at-age (\code{landings.n})
#'   \item discards numbers-at-age (\code{discards.n})
#'   \item proportion mature-at-age (\code{mat})
#'   \item natural mortality-at-age (\code{m})
#'   \item fishing mortality-at-age (\code{harvest})
#'   \item stock mean weight-at-age (\code{stock.wt})
#'   \item catch mean weight-at-age (\code{catch.wt})
#'   \item landings mean weight-at-age (\code{landings.wt})
#'   \item discards mean weight-at-age (\code{discards.wt})
#' }
#'
#' Catch numbers and fishing mortality-at-age are summed across commercial
#' fleets, whereas catch, landings and discards mean weight-at-age are averages
#' weighted by the catch number proportions by commercial fleets. Landings and
#' discards numbers-at-age are calculated using the catch proportion weighted
#' mean landings fraction.
#'
#' The function also updates the Fbar range for the stock from the fitted
#' SAM object.
#'
#' Returned \code{FLFisheries} objects contain one or more \code{FLFishery}
#' objects, each representing a distinct fishing fleet. Each \code{FLFishery}
#' object contains an \code{FLCatch} for each stock harvested. \code{FLCatch}
#' contains data for:
#' \itemize{
#'   \item partial fishing mortality-at-age (\code{harvest})
#'   \item catch numbers-at-age (\code{catch.n})
#'   \item landings numbers-at-age (\code{landings.n})
#'   \item discards numbers-at-age (\code{discards.n})
#'   \item catch mean weight-at-age (\code{catch.wt})
#'   \item landings mean weight-at-age (\code{landings.wt})
#'   \item discards mean weight-at-age (\code{discards.wt})
#' }
#'
#' Returned \code{FLIndices} objects contain one or more \code{FLIndex} objects.
#' These represent the survey indices for a given stock. Each \code{FLIndex}
#' contains data for:
#' \itemize{
#'   \item survey index values (\code{index})
#'   \item survey catchability (\code{index.q})
#' }
#'
#' @param SAMfit A SAM fitted stock assessment model object or
#'               a named list of SAM fitted stock assessment model objects
#' @param stkname Stock name to be used in  \code{FLFishery} or \code{FLFleet}
#'                object. Ignored if object of class \code{sam_list} is supplied.
#' @param useSAMcatch Optional argument. If \code{TRUE}, the fitted catches estimated
#'                    by SAM are used. Otherwise, the observed catches are used.
#'                    Defaults to \code{TRUE}.
#' @param uncertainty Optional argument. If \code{TRUE}, the iteration dimension
#'                    of the returned objects contains replicates (uncertainty)
#'                    sampled from the variance-covariance matrix of the fitted
#'                    SAM object. Defaults to \code{FALSE}
#' @param niter Optional integer. The number of replicates sampled if
#'              \code{uncertainty} is \code{TRUE}. Default is 1000 replicates.
#' @param seed Optional integer. This is a random number seed to generate
#'             reproducible outputs. Defaults to \code{NULL}.
#'
#'
#' @return A list containing \code{FLStocks}, \code{FLFisheries} and
#'         \code{FLIndices} objects
#'
#' @section  Warning:
#' This function requires \code{FLCore} and \code{FLFishery} to operate.
#'
#' @export


setGeneric("multiSAM2FLR", function(SAMfit,
                                    stkname = NULL,
                                    useSAMcatch = TRUE,
                                    uncertainty = FALSE,
                                    niter = 1000,
                                    seed = NULL,
                                    ...) {

  standardGeneric("multiSAM2FLR")
})

## If SAMfit == "sam"
#' @rdname multiSAM2FLR
setMethod(f = "multiSAM2FLR",
          signature = signature(SAMfit = "sam"),
          definition = function(SAMfit,
                                stkname = NULL,
                                useSAMcatch = TRUE,
                                uncertainty = FALSE,
                                niter = 1000,
                                seed = NULL) {

            # ==============================#
            # (optional) sample uncertainty
            # ==============================#
            #
            # We will want to have consistent sampled data across fleets for
            # a given stock, so sample here.
            if(uncertainty == TRUE){

              ## Set random number seed if provided
              if(!is.null(seed)) set.seed(seed)

              # TO DO - build a tryCatch here to handle error outputs.
              # sdreport will throw an error if applying a newer version of TMB
              # to an object fitted using older version.
              
              variates <- multiSAMvariates(SAMfit, niter)

              # ------------------------------------------#
              # Calculate corresponding predicted catches
              # ------------------------------------------#

              res_n <- multiSAMcay(SAMfit, variates, niter, option = 2)

              samVariates <- list(variates = variates,
                                  res_n    = res_n)

            }

            # ==============================#
            # Process Stock data
            # ==============================#

            # Generate FLStock
            stk <- multiSAM2FLStock(SAMfit = SAMfit,
                                    useSAMcatch = useSAMcatch,
                                    uncertainty = uncertainty,
                                    niter = niter,
                                    samVariates = samVariates,
                                    seed = seed)

            # Coerce to FLStocks and define name
            stks <- FLCore::FLStocks(stk)
            names(stks) <- stkname

            # ==============================#
            # Process Fleet data
            # ==============================#

            flts <- multiSAM2FLFishery(SAMfit = SAMfit,
                                       stkname = stkname,
                                       useSAMcatch = useSAMcatch,
                                       add = FALSE,
                                       uncertainty = uncertainty,
                                       niter = niter,
                                       samVariates = samVariates,
                                       seed = seed)

            # ==============================#
            # Process Survey data
            # ==============================#

            idx <- multiSAM2FLIndex(SAMfit  = SAMfit,
                                    uncertainty = uncertainty,
                                    niter = niter,
                                    samVariates = samVariates,
                                    seed = seed)
            idxs <- list(idx)
            names(idxs) <- stkname

            # return outputs
            return(list(stks = stks,
                        flts = flts,
                        idxs = idxs))


          })

## If SAMfit == "sam_list"
#' @rdname multiSAM2FLR
setMethod(f = "multiSAM2FLR",
          signature = signature(SAMfit = "sam_list"),
          definition = function(SAMfit,
                                useSAMcatch = TRUE,
                                uncertainty = FALSE,
                                niter = 1000,
                                seed = NULL,
                                ...) {

            if(is.null(names(SAMfit)))
              stop("SAMfit must be a named list")

            ## How many stocks?
            nstks <- length(SAMfit)

            ## min and max years
            dim_minyear <- min(sapply(1:nstks, function(x) min(SAMfit[[x]]$data$years)))
            dim_maxyear <- max(sapply(1:nstks, function(x) max(SAMfit[[x]]$data$years)))

            # ------------------------------#
            # (optional) sample uncertainty
            # ------------------------------#
            #
            # We will want to have consistent sampled data across fleets for
            # a given stock, so sample here.
            if(uncertainty == TRUE){

              ## Sample variates for each stock
              variatesList <- lapply(1:nstks, function(x){

                ## Set random number seed if provided
                if(!is.null(seed)) set.seed(seed)

                # ------------------------------------------#
                # Sample replicates from MVN distribution
                # ------------------------------------------#

                variates <- multiSAMvariates(SAMfit[[x]], niter)

                # ------------------------------------------#
                # Calculate corresponding predicted catches
                # ------------------------------------------#
                
                res_n <- multiSAMcay(SAMfit[[x]], variates, niter, option = 2)

                return(list(variates = variates,
                            res_n    = res_n))

              })
            } else {

              ##  A bit of a hacky solution - I supply an empty variatesList
              ## if uncertainty == FALSE
              variatesList <- lapply(1:nstks, function(x){
                return(NULL)
              })

            }

            # ------------------------------#
            # Process Stock data
            # ------------------------------#
            # Process into FLStocks
            stks <- FLStocks(lapply(1:nstks, function(x){

              stk <- multiSAM2FLStock(SAMfit = SAMfit[[x]],
                                      useSAMcatch = useSAMcatch,
                                      yearRange = c(dim_minyear, dim_maxyear),
                                      uncertainty = uncertainty,
                                      niter = niter,
                                      samVariates = variatesList[[x]],
                                      seed = seed)
              stk@name <- names(SAMfit)[x]

              return(stk)

            }))

            ## Define names for FLStocks
            names(stks) <- names(SAMfit)

            # ------------------------------#
            # Process Fleet data
            # ------------------------------#

            ## Loop over each stock object
            for(i in 1:nstks) {

              ## If first stock - create new fleet, otherwise append
              ## to existing object
              if(i == 1) {
                flts <- multiSAM2FLFishery(SAMfit = SAMfit[[i]],
                                           stkname = names(SAMfit)[i],
                                           useSAMcatch = useSAMcatch,
                                           add = FALSE,
                                           yearRange = c(dim_minyear, dim_maxyear),
                                           uncertainty = uncertainty,
                                           niter = niter,
                                           samVariates = variatesList[[i]],
                                           seed = seed)
              } else {
                flts <- multiSAM2FLFishery(SAMfit = SAMfit[[i]],
                                           stkname = names(SAMfit)[i],
                                           useSAMcatch = useSAMcatch,
                                           add = TRUE,
                                           fleets = flts,
                                           yearRange = c(dim_minyear, dim_maxyear),
                                           uncertainty = uncertainty,
                                           niter = niter,
                                           samVariates = variatesList[[i]],
                                           seed = seed)
              }
            }

            # ------------------------------#
            # Process Survey data
            # ------------------------------#

            idxs <- lapply(1:nstks, function(x) {

              multiSAM2FLIndex(SAMfit = SAMfit[[x]],
                               yearRange = c(dim_minyear, dim_maxyear),
                               uncertainty = uncertainty,
                               niter = niter,
                               samVariates = variatesList[[x]],
                               seed = seed)

            })
            names(idxs) <- names(SAMfit)

            return(list(stks = stks,
                        flts = flts,
                        idxs = idxs))

          })


#' Sample variates from fitted SAM object
#' -----------------------------------------------------------------------------
#' 
#' Function takes a fitted SAM object and samples a number of variates for each
#' estimated parameter from a multivariate normal distribution. Function returns
#' a matrix.

multiSAMvariates <- function(SAMfit, niter) {
  
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
  # variates <- MASS::mvrnorm((niter-1), est, cov) # col = parameters, row = replicates
  
  ## add parameter names to variates
  colnames(variates) <- names(est)
  
  return(variates)
}

#' Extract fleet F-at-age from sampled SAM parameters
#' -----------------------------------------------------------------------------
#' 
#' Function takes a fitted SAM object and previously sampled parameter variates
#' and coerces the fleet F-at-age into an array

multiSAMfay <- function(SAMfit, variates, niter, catch_fleets = NULL) {
  
  ## Automatically identify catch fleets if null
  if(is.null(catch_fleets)) catch_fleets <- which(SAMfit$data$fleetTypes == 0)
  
  ## Extract fishing mortality variates to a separate object
  Fvariates <- variates[, colnames(variates) == "logF"]
  
  ## extract dimensions - natural mortality is a safer reference than catch
  nm <- SAMfit$data$natMor
  aa <- colnames(nm[,,1])
  yy <- rownames(nm[,,1])
  
  # Loop over each fleet that is identified and extract the corresponding
  # partial fishing mortality-at-age matrices
  
  ## calculate fleet F-at-age
  Farray <- sapply(catch_fleets, function(x){
    
    ## define indices for fleet x
    idx <- (SAMfit$conf$keyLogFsta + 1)[x,] # index for fleet x
    fa   <- aa[idx > 0]                    # vector of fished ages
    idx <-  idx[idx > 0]                    # remove cases of no F-at-age
    
    # because I am subsetting sampled logF for a multifleet model, logF columns
    # are associated with both age and year.
    
    ## generate an index to handle ages and years
    idxy <- c(sapply(1:length(yy), function(y) idx + max(SAMfit$conf$keyLogFsta + 1)*(y-1)))
    
    ## generate an array to hold age, year, iteration data
    F_array_i <- array(0, dim = c(length(aa), 
                                  length(yy), 
                                  niter-1))
    rownames(F_array_i) <- aa # full ages
    colnames(F_array_i) <- yy # full years
    
    ## insert fleet F-at-age data
    F_array_i[fa,,] <- c(exp(t(Fvariates[,idxy])))

    return(F_array_i)
    
  },simplify = "array", USE.NAMES = TRUE)
  return(Farray)
}


#' Calculate predicted catches from sampled SAM parameters
#' -----------------------------------------------------------------------------
#' 
#' Function takes a fitted SAM object and previously sampled parameter variates
#' and calculates the corresponding predicted catches.
#' 
#' There are two ways of doing this. Either updated the objective function with
#' the sampled parameter values and extract the predicted catches from TMB, or
#' directly calculate predicted catches from sampled parameters given inputted
#' estimates of natural mortality.
#' 
#' The advantage of the latter methods is that catch predictions are extended
#' into the aggregated data period.

multiSAMcay <- function(SAMfit, variates, niter, option = 2) {
  
  ### catch fleet index/indices
  catch_fleets <- which(SAMfit$data$fleetTypes == 0)
  catch_desc   <- SAMfit$data$aux
  years        <- SAMfit$data$years
  
  # Option 1 involves iteratively running the SAM objective function with sampled
  # parameter values and extracting the reported predicted observations. These are
  # then coerced into an array with age, year and fleet dimensions. Note that
  # predictions are generated for only the disaggregated data period.
  
  # dimensions: age, year, fleet, iter
  
  if(option == 1) {
    
    ## convert year from index to actual year - only do this if the max index
    ## year vector matches actual year vector length
    if(!(min(catch_desc[,"year"]) %in% years) & (max(catch_desc[,"year"]) == length(years))) {
      
      ## update year index to actual year
      catch_desc[,"year"] <- catch_desc[,"year"] + min(years) - 1
      
    }
    
    ## Calculate years for which we have catch data
    catch_years <- unique(catch_desc[catch_desc[,"fleet"] %in% catch_fleets,"year"])
    
    ## Loop over each iteration
    . <- capture.output(res_n <- sapply(1:(niter-1), function(iter_i) {
      
      ## run the observation function for the using sampled fixed parameters
      SAMfit$obj$fn(variates[iter_i, 1:length(sds$par.fixed)])
      
      ## extract predicted observation estimates
      tmp <- cbind(catch_desc, est = SAMfit$obj$report()$predObs)
      
      ## Subset for commercial fleets
      tmp <- tmp[tmp[, "fleet"] %in% catch_fleets, ]
      
      ## Exponentiate to work in real catches
      tmp[,"est"] <- exp(tmp[,"est"])
      
      ## reorder to insert in the correct order
      tmp <- tmp[order(tmp[,"fleet"], tmp[,"year"], tmp[,"age"]),]
      
      ## Generate blank array which has full age and year dimensions
      Cmatrix <- array(NA,
                       dim = c(length(unique(tmp[,"age"])),                # age
                               length(unique(tmp[,"year"])),               # year
                               length(catch_fleets)),                      # fleets
                       dimnames = list(age = sort(unique(tmp[,"age"])),    # age names
                                       year = sort(unique(tmp[,"year"])))) # year names
      
      ## insert catches into blank matrix
      Cmatrix[] <- tmp[,"est"]
      
      return(Cmatrix)
      
    }, simplify = "array")) ## END sapply over iteration
  } ## END option 1
  
  # Option 2 involves extracting the sampled fishing mortality at age for each fleet,
  # and the sampled stock numbers at age, then calculating catch numbers using the
  # Baranov catch equation given estimates of natural mortality at age.
  
  # dimensions: age, year, iter, fleet
  
  if(option == 2) {
    
    ## extract fleet F-at-age
    F_array <- multiSAMfay(SAMfit, variates, niter)
    
    ## calculate numbers-at-age
    N_array   <- array(NA, dim = c(length(SAMfit$conf$minAge:SAMfit$conf$maxAge), # ages
                                   length(years),                                 # years
                                   niter-1))                                      # iterations
    N_array[] <- exp(t(variates[, colnames(variates) == "logN"]))
    
    ## calculate total F-at-age and total Z-at-age
    Ftotal <- apply(F_array, c(1,2,3), sum, na.rm = TRUE)
    Ztotal <- sweep(Ftotal, c(1,2), t(SAMfit$data$natMor), "+")
    
    ## calculate Catch at age for each fleet
    res_n <- sapply(catch_fleets, function(x) {
      (F_array[,,,x] / Ztotal) * (1 - exp(-Ztotal)) * N_array
    }, simplify = "array", USE.NAMES = TRUE)
  } ## END option 2
  
  ## return result
  return(res_n)
}