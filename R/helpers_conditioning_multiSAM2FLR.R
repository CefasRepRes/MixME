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

              # ------------------------------------------#
              # Calculate corresponding predicted catches
              # ------------------------------------------#

              ### catch fleet index/indices
              catch_fleets <- which(SAMfit$data$fleetTypes == 0)
              catch_desc <- SAMfit$data$aux
              years      <- SAMfit$data$years

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
                                 dim = c(length(unique(tmp[,"age"])),
                                         length(unique(tmp[,"year"])),
                                         length(catch_fleets)),
                                 dimnames = list(age = sort(unique(tmp[,"age"])),
                                                 year = sort(unique(tmp[,"year"]))))

                ## insert catches into blank matrix
                Cmatrix[] <- tmp[,"est"]

                return(Cmatrix)

              }, simplify = "array"))


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

                ## Calculate standard deviation of model parameters
                . <- capture.output(sds <- TMB::sdreport(obj = SAMfit[[x]]$obj,
                                                         par.fixed = SAMfit[[x]]$opt$par,
                                                         getJointPrecision = TRUE))

                ## Best-fit values for parameters
                est <- c(sds$par.fixed, sds$par.random)

                ## Variance-Covariance matrix of all model parameters
                cov <- solve(sds$jointPrecision)

                ## generate a number of random variates by sampling from a multivariate
                ## normal distribution
                variates <- stockassessment::rmvnorm((niter-1), est, cov) # col = parameters, row = replicates
                colnames(variates) <- names(est)

                # ------------------------------------------#
                # Calculate corresponding predicted catches
                # ------------------------------------------#

                ### catch fleet index/indices
                catch_fleets <- which(SAMfit[[x]]$data$fleetTypes == 0)
                catch_desc   <- SAMfit[[x]]$data$aux
                years        <- SAMfit[[x]]$data$years

                ## convert year from index to actual year - only do this if the max index
                ## year vector matches actual year vector length
                if(!(min(catch_desc[,"year"]) %in% years) & (max(catch_desc[,"year"]) == length(years))) {

                  ## update year index to actual year
                  catch_desc[,"year"] <- catch_desc[,"year"] + min(years) - 1

                }

                ## Calculate years for which we have catch data
                catch_years <- unique(catch_desc[catch_desc[,"fleet"] %in% catch_fleets,"year"])

                ## Loop over each iteration
                . <- capture.output(res_n <- sapply(1:(niter-1), function(iter_i){

                  ## run the observation function for the using sampled fixed parameters
                  SAMfit[[x]]$obj$fn(variates[iter_i, 1:length(sds$par.fixed)])

                  ## extract predicted observation estimates
                  tmp <- cbind(catch_desc, est = SAMfit[[x]]$obj$report()$predObs)

                  ## Subset for commercial fleets
                  tmp <- tmp[tmp[, "fleet"] %in% catch_fleets, ]

                  ## Exponentiate to work in real catches
                  tmp[,"est"] <- exp(tmp[,"est"])

                  ## reorder to insert in the correct order
                  tmp <- tmp[order(tmp[,"fleet"], tmp[,"year"], tmp[,"age"]),]

                  ## Generate blank array which has full age and year dimensions
                  Cmatrix <- array(NA,
                                   dim = c(length(unique(tmp[,"age"])), length(unique(tmp[,"year"])), length(catch_fleets)),
                                   dimnames = list(age = sort(unique(tmp[,"age"])),
                                                   year = sort(unique(tmp[,"year"]))))

                  ## insert catches into blank matrix
                  Cmatrix[] <- tmp[,"est"]

                  return(Cmatrix)

                }, simplify = "array"))

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
