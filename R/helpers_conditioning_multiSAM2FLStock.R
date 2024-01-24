# ---
# title: 'Functions to help condition Operating Models'
# author: 'Matthew Pace'
# date: 'June 2022'
# ---
#
#' Convert a SAM fitted object into a \code{FLStock} object
#'
#' This function takes a SAM fitted stock assessment object as input and
#' returns an \code{FLStock} object.
#'
#' Returned \code{FLStock} object contains data for:
#' \itemize{
#' \item stock numbers-at-age (\code{stock.n})
#' \item catch numbers-at-age (\code{catch.n})
#' \item landings numbers-at-age (\code{landings.n})
#' \item discards numbers-at-age (\code{discards.n})
#' \item proportion mature-at-age (\code{mat})
#' \item natural mortality-at-age (\code{m})
#' \item fishing mortality-at-age (\code{harvest})
#' \item stock mean weight-at-age (\code{stock.wt})
#' \item catch mean weight-at-age (\code{catch.wt})
#' \item landings mean weight-at-age (\code{landings.wt})
#' \item discards mean weight-at-age (\code{discards.wt})}
#'
#' Catch numbers and fishing mortality-at-age are summed across commercial fleets,
#' whereas catch, landings and discards mean weight-at-age are averages weighted
#' by the catch number proportions by commercial fleets. Landings and discards
#' numbers-at-age are calculated using the catch proportion weighted mean
#' landings fraction.
#'
#' The function also updates the Fbar range for the stock from the fitted
#' SAM object.
#'
#' If \code{uncertainty} is \code{TRUE}, the first replicate of the iteration
#' dimension contains the best-fit SAM estimates for stock numbers, harvest rate
#' and catch, whereas the
#'
#' @param SAMfit A SAM fitted stock assessment model object
#' @param useSAMcatch Optional argument. If \code{TRUE}, the fitted catches estimated
#'                    by SAM are used. Otherwise, the observed catches are used.
#'                    Defaults to \code{TRUE}.
#' @param yearRange   Optional argument to extend the FLStock year dimension to
#'                    a user-supplied range. Consists of an integer vector
#'                    of two elements. First element is the minimum year. Second
#'                    element is the maximum year.
#' @param uncertainty Optional argument. If \code{TRUE}, the iteration dimension
#'                    of the returned objects contains replicates (uncertainty)
#'                    sampled from the variance-covariance matrix of the fitted
#'                    SAM object. Defaults to \code{FALSE}
#' @param niter Optional integer. The number of replicates sampled if
#'              \code{uncertainty} is \code{TRUE}. Default is 1000 replicates.
#' @param seed Optional integer. This is a random number seed to generate
#'             reproducible outputs. Defaults to \code{NULL}.
#'
#' @return An \code{FLStock} object
#'
#' @section  Warning:
#' This function requires \code{FLCore} to operate.
#'
#' @export

multiSAM2FLStock <- function(SAMfit,
                             useSAMcatch = TRUE,
                             yearRange = NULL,
                             uncertainty = FALSE,
                             niter = 1000,
                             samVariates = NULL,
                             seed = NULL) {

  ## Check that inputs are correct
  ## I MIGHT NEED TO DO SOME MORE SOPHISTICATED CHECKS

  ## NOTE - FUNCTION DOES NOT CURRENTLY HANDLE CATCH MULTIPLIERS!!!

  # ----------------------------------#
  # 1.  Define stock dimensions
  #
  # 2.  Stock properties
  # 2.1 ... Stock numbers-at-age
  # 2.2 ... Stock mean weight-at-age
  # 2.3 ... Natural mortality-at-age
  # 2.4 ... Proportion mature-at-age
  #
  # 3.  Catch summaries and weights
  # 3.1 ... Catch numbers-at-age
  # 3.2 ... Catch mean weight-at-age
  # 3.3 ... Landings numbers-at-age
  # 3.4 ... Landings mean weight-at-age
  # 3.5 ... Discards numbers-at-age
  # 3.6 ... Discards mean weight-at-age
  # 3.7 ... Fishing mortality-at-age
  #
  # 4.  Proportions of mortality before spawning
  # 5.  Update stock, catch, landings, discards Biomass slots
  # 6.  (Optional) extend stock year dimension to new range
  #
  # 7.  (Optional) extend stock iteration dimension and add uncertainty
  # 7.1 ... Sample replicates from MVN distribution
  # 7.2 ... Insert sampled stock numbers
  # 7.3 ... Insert sampled fishing mortality-at-age
  # 7.4 ... Insert sampled catch numbers
  #
  # 8.  (Optional) extract process error (logSdLogN)
  #
  # ----------------------------------#

  ## stop if number of iterations are not provided
  if(uncertainty == TRUE & is.null(niter))
    stop("If 'uncertainty' is TRUE, the number of replicates 'niter' must be provided")

  ## stop if observed catch and uncertainty is requested
  if(uncertainty == TRUE & useSAMcatch == FALSE)
    stop("If 'uncertainty' is TRUE, predicted catches must be used")

  # ==================================#
  # SECTION 1: Define stock dimensions
  # ==================================#

  ## Extract dimensions
  ages       <- SAMfit$conf$minAge:SAMfit$conf$maxAge
  years      <- SAMfit$data$years

  ## Generate empty FLQuant object
  qnt <- FLCore::FLQuant(NA, dimnames = list(year = years,
                                            age  = ages,
                                            iter = 1))
  ## Generate empty FLStock object
  stk <- FLCore::FLStock(qnt)

  ## How many commercial fleets?
  fleet_n <- length(which(SAMfit$data$fleetTypes %in% c(0,1,7)))

  # ================================#
  # SECTION 2:   Stock properties
  # ================================#
  # SECTION 2.1: Stock numbers-at-age
  # --------------------------------#

  ## Extract and fill stock numbers-at-age
  FLCore::stock.n(stk)[FLCore::ac(ages), FLCore::ac(years)] <- exp(SAMfit$pl$logN)

  # ------------------------------------#
  # SECTION 2.2: Stock mean weight-at-age
  # ------------------------------------#

  ## Extract stock weights
  stockwt <- t(SAMfit$data$stockMeanWeight)

  ## dimnames are available. Use these to insert data
  swt_age <- rownames(stockwt)
  swt_yrs <- colnames(stockwt)

  ## Fill stock weights
  FLCore::stock.wt(stk)[FLCore::ac(swt_age), FLCore::ac(swt_yrs)] <- stockwt

  # ------------------------------------#
  # SECTION 2.3: Natural mortality-at-age
  # ------------------------------------#

  ## Extract natural mortality-at-age (transpose to match FLQuant dimensions)
  stk_m <- t(SAMfit$data$natMor)
  if(ncol(stk_m) != length(years)) stk_m <- t(stk_m)

  ## Fill natural mortality-at-age
  FLCore::m(stk)[FLCore::ac(ages), FLCore::ac(years)] <- stk_m

  # --------------------------------------#
  # SECTION 2.4: Proportion mature-at-age
  # --------------------------------------#

  ## Extract maturity-at-age
  stk_mat <- t(SAMfit$data$propMat)
  if(ncol(stk_mat) != length(years)) stk_mat <- t(stk_mat)

  ## Fill natural mortality-at-age
  FLCore::mat(stk)[FLCore::ac(ages), FLCore::ac(years)] <- stk_mat

  # ========================================#
  # SECTION 3:   Catch summaries and weights
  # ========================================#
  # SECTION 3.1: Catch numbers-at-age
  # ----------------------------------------#

  ## Use fitted catches
  if(isTRUE(useSAMcatch)) {

    ## combine catch and year, fleet and age indices
    catchn <- cbind(SAMfit$data$aux,
                    catch = exp(SAMfit$rep$predObs))

    ## Use observed catches
  } else {

    ## combine catch and year, fleet and age indices
    catchn <- cbind(SAMfit$data$aux,
                    catch = exp(SAMfit$data$logobs))

  }

  # The next if/else statement controls for cases of multiple
  # fleets. If multiple fleets are present, subset. Otherwise
  # use the first fleet identified

  ## If multiple commercial fleets are present
  if(fleet_n > 1) {

    ## select commercial fleets
    catchn <- catchn[SAMfit$data$aux[,"fleet"] %in% which(SAMfit$data$fleetTypes %in% c(0,1,7)),]

  } else {

    ## select commercial fleets
    catchn <- catchn[SAMfit$data$aux[,"fleet"] == 1,]

  }

  ## check if year is correctly formatted - only do this if index year vector
  ## length matches actual year vector length
  if(!(min(catchn[,"year"]) %in% years) & (max(catchn[,"year"]) - min(catchn[,"year"]) != tail(years,1) - head(years,1)))
    stop("logObs data index for year does not match length of data year vector")
  if(!(min(catchn[,"year"]) %in% years) & (max(catchn[,"year"]) - min(catchn[,"year"]) == tail(years,1) - head(years,1))) {

    ## update year index to actual year
    catchn[,"year"] <- catchn[,"year"] + min(years) - 1

  }

  ## sum across fleets
  catchnn <- stats::aggregate(catch ~ age + year, data = catchn, FUN = sum)

  # Next re-organise the year index and create an empty matrix to cater for cases
  # where there are missing data

  ## Generate blank matrix which has full age and year dimensions
  Cmatrix <- matrix(nrow = length(unique(catchnn[,"age"])),
                    ncol = length(unique(catchnn[,"year"])),
                    dimnames = list(age = sort(unique(catchnn[,"age"])),
                                    year = sort(unique(catchnn[,"year"]))))

  ## insert catches into blank matrix
  Cmatrix[cbind(ac(catchnn[,"age"]), ac(catchnn[,"year"]))] <- catchnn[,"catch"]

  ## insert into catch.n slot
  FLCore::catch.n(stk)[] <- 0
  FLCore::catch.n(stk)[rownames(Cmatrix), colnames(Cmatrix)] <- Cmatrix

  # --------------------------------------#
  # SECTION 3.2: Catch mean weight-at-age
  # --------------------------------------#

  if(fleet_n > 1) {

    ## If Catch mean weight is available per fleet?
    if(length(dim(SAMfit$data$catchMeanWeight)) > 2) {

      ## Extract catch mean weight for each fleet
      catchwt <- SAMfit$data$catchMeanWeight

      ## summarise across fleets
      catchwt <- flt_wt_avg(ar = catchwt, cn = catchn)

    } else {

      catchwt <- t(SAMfit$data$catchMeanWeight)
    }

  } else {

    ## Data might still be held in array format
    if(length(dim(SAMfit$data$catchMeanWeight)) > 2) {

      catchwt <- SAMfit$data$catchMeanWeight
      catchwt <- t(catchwt[,,,drop = TRUE]) # drop redundant third dimension and transpose

    } else {

      catchwt <- t(SAMfit$data$catchMeanWeight)

    }
  }
  
  ## Some cases where year dimensions might differ
  FLCore::catch.wt(stk)[FLCore::ac(rownames(catchwt)), FLCore::ac(colnames(catchwt))] <- catchwt

  # ------------------------------------#
  # SECTION 3.3: Landings numbers-at-age
  # ------------------------------------#

  ## Extract landings fraction
  lf <- SAMfit$data$landFrac

  ## If multiple commercial fleets are present
  if(fleet_n > 1) {

    ## summarise across fleets
    lf <- flt_wt_avg(ar = lf, cn = catchn)
    lf_qnt <- qnt
    lf_age <- rownames(lf)
    lf_yrs <- colnames(lf)
    lf_qnt[FLCore::ac(lf_age), FLCore::ac(lf_yrs)] <- lf

  } else {

    if(length(dim(lf)) > 2) {

      ## drop redundant third dimension and transpose
      lf <- t(lf[,,,drop = TRUE])

    } else {
      
      ## Otherwise just transpose...
      lf <- t(lf)
      
    }

    # Use the FLquant template to create a landings fraction FLquant
    # This should cater for cases where a landings fraction is not
    # available for the full time-series
    lf_qnt <- qnt
    lf_age <- rownames(lf)
    lf_yrs <- colnames(lf)
    lf_qnt[FLCore::ac(lf_age), FLCore::ac(lf_yrs)] <- lf

  }

  ## use landings fraction to calculate landings from total catch
  FLCore::landings.n(stk) <- lf_qnt * catch.n(stk)

  # -----------------------------------------#
  # SECTION 3.4: Landings mean weight-at-age
  # -----------------------------------------#

  ## Extract landings mean weight
  landwt <- SAMfit$data$landMeanWeight

  if(fleet_n > 1) {

    ## If Catch mean weight is available per fleet?
    if(length(dim(landwt)) > 2) {

      ## summarise across fleets
      landwt <- flt_wt_avg(ar = landwt, cn = catchn)

    } else {

      landwt <- t(landwt)
    }

  } else {

    if(length(dim(landwt)) > 2) {

      ## drop redundant third dimension and transpose
      landwt <- t(landwt[,,,drop = TRUE])

    } else {

      landwt <- t(landwt)
    }
  }
  
  ## Some cases where year dimensions might differ
  FLCore::landings.wt(stk)[FLCore::ac(rownames(landwt)), FLCore::ac(colnames(landwt))] <- landwt

  # ------------------------------------#
  # SECTION 3.5: Discards numbers-at-age
  # ------------------------------------#

  ## use landings fraction to calculate discards from total catch
  FLCore::discards.n(stk) <- (1 - lf_qnt) * FLCore::catch.n(stk)

  # -----------------------------------------#
  # SECTION 3.6: Discards mean weight-at-age
  # -----------------------------------------#

  ## Extract discards mean weight
  discwt <- SAMfit$data$disMeanWeight

  if(fleet_n > 1) {

    ## If Catch mean weight is available per fleet?
    if(length(dim(discwt)) > 2) {

      ## summarise across fleets
      discwt <- flt_wt_avg(ar = discwt, cn = catchn)

    } else {

      discwt <- t(SAMfit$data$disMeanWeight)
    }

  } else {

    if(length(dim(discwt)) > 2) {

      ## drop redundant third dimension and transpose
      discwt <- t(discwt[,,,drop = TRUE])

    } else {

      discwt <- t(SAMfit$data$disMeanWeight)
    }
  }

  ## Some cases where year dimensions might differ
  FLCore::discards.wt(stk)[FLCore::ac(rownames(discwt)), FLCore::ac(colnames(discwt))] <- discwt

  # ------------------------------------#
  # SECTION 3.7: Fishing mortality-at-age
  # ------------------------------------#

  ## Define harvest rate units
  FLCore::units(stk)$harvest <- "f"

  ## Define Fbar range
  range(stk)["minfbar"] <- SAMfit$conf$fbarRange[1]
  range(stk)["maxfbar"] <- SAMfit$conf$fbarRange[2]

  # Extract fishing mortality at age
  # ---------------------------------#

  if(fleet_n > 1) {

    # Loop over each fleet that is identified and extract the corresponding
    # partial fishing mortality-at-age matrices

    Farray <- sapply(which(SAMfit$data$fleetTypes == 0), function(x){

      idx <- (SAMfit$conf$keyLogFsta + 1)[x,] # index for fleet x
      F_matrix <- exp(SAMfit$pl$logF)[idx,]
      return(F_matrix)

    },simplify = "array")

    ## Sum the partial fishing mortalities into a total mortality
    Ftotal <- apply(Farray, c(1,2), sum)

  } else {

    ## Extract fishing mortality-at-age
    idx <- (SAMfit$conf$keyLogFsta + 1)[1,]
    Ftotal <- exp(SAMfit$pl$logF)[idx,]
  }
  
  # There are some cases (such as North Sea cod) where catch data extends to yr - 1
  # (as might be expected) but the stock dimensions extend to yr because we have
  # survey data from the current year. In these cases, SAM will predict F-at-age
  # for yr... but we don't want to retain this prediction (because it is not
  # constrained by catch data)

  ## Fill fishing mortality-at-age
  FLCore::harvest(stk)[] <- 0
  FLCore::harvest(stk)[rownames(Cmatrix), colnames(Cmatrix)] <- Ftotal[seq(rownames(Cmatrix)), seq(colnames(Cmatrix))]

  # -----------------------------------------------------#
  # SECTION 4:   Proportions of mortality before spawning
  # -----------------------------------------------------#
  # We need the proportion of mortality before spawning to
  # calculate SSB

  ## Extract proportion fishing mortality before spawning
  Fspwn <- SAMfit$data$propF

  ## If multiple fleets, then sum across the fleet dimensions of the array
  if(fleet_n > 1) {

    ## Sum the partial fishing mortalities into a total mortality
    Fspwn <- apply(Fspwn, c(1,2), sum)

  }

  ## Even if we do not have multiple fleets, the data might still be in a
  ## 3D array
  if(length(dim(Fspwn)) > 2) {

    Fspwn <- Fspwn[,,,drop = TRUE]
  }

  ## Fill proportion fishing mortality before spawning
  FLCore::harvest.spwn(stk)[FLCore::ac(ages), FLCore::ac(years)] <- t(Fspwn)

  ## Fill proportion natural mortality before spawning
  mspwn <- t(SAMfit$data$propM)
  FLCore::m.spwn(stk)[FLCore::ac(ages), FLCore::ac(years)] <- mspwn

  # ==================================================================#
  # SECTION 5:   Update stock, catch, landings, discards Biomass slots
  # ==================================================================#

  FLCore::stock(stk)    <- FLCore::computeStock(stk)
  FLCore::catch(stk)    <- FLCore::computeCatch(stk)
  FLCore::landings(stk) <- FLCore::computeLandings(stk)
  FLCore::discards(stk) <- FLCore::computeDiscards(stk)

  # ==================================================================#
  # SECTION 6:   (Optional) extend stock year dimension to new range
  # ==================================================================#

  if (!is.null(yearRange)) {
    stk <- FLCore::expand(stk, year = yearRange[1]:yearRange[2])
  }

  # ==================================================================#
  # SECTION 7:   (Optional) extend stock iteration dimension and
  #              add uncertainty
  # ==================================================================#
  #
  # NOTE: methods taken from S. Fischer - FLfse package

  if(uncertainty == TRUE & niter > 1){

    ## Extend stock object along the iteration dimension
    stk <- FLCore::propagate(stk, iter = niter)

    # -----------------------------------------------------#
    # SECTION 7.1: Sample replicates from MVN distribution
    # -----------------------------------------------------#

    ## Only sample if sampled variates are not provided
    if(!is.null(samVariates)){

      variates <- samVariates[[1]]

    } else {

      ## Set random number seed if provided
      if(!is.null(seed)) set.seed(seed)

      variates <- multiSAMvariates(SAMfit, niter)
    }

    # -----------------------------------------------------#
    # SECTION 7.2: Insert sampled stock numbers
    # -----------------------------------------------------#

    ## Exponentiate and insert stock numbers into iterations 2-n
    FLCore::stock.n(stk)[,ac(years),,,,-1] <- exp(t(variates[, colnames(variates) == "logN"]))

    # -----------------------------------------------------#
    # SECTION 7.3: Insert sampled fishing mortality-at-age
    # -----------------------------------------------------#

    ## I need to handle single fleet and multifleet SAM fits differently
    if(fleet_n > 1) {

      # Loop over each fleet that is identified and extract the corresponding
      # partial fishing mortality-at-age matrices
      Farray <- multiSAMfay(SAMfit, variates, niter) # age, years, iters, fleets

      ## Sum the partial fishing mortalities into a total mortality
      Ftotal <- apply(Farray, c(1,2,3), sum)
      
      ## Fill fishing mortality-at-age
      FLCore::harvest(stk)[, colnames(Cmatrix),,,,-1] <- Ftotal

    } else {
      
      ## Extract fishing mortality variates to a separate object
      Fvariates <- variates[, colnames(variates) == "logF"]

      ## Extract fishing mortality-at-age
      idx <- (SAMfit$conf$keyLogFsta + 1)[1,]
      Ftotal <- exp(t(Fvariates[,idx]))
      
      ## Fill fishing mortality-at-age
      FLCore::harvest(stk)[rownames(Cmatrix), colnames(Cmatrix),,,,-1] <- Ftotal

    }

    # -----------------------------------------------------#
    # SECTION 7.4: Insert sampled catch numbers
    # -----------------------------------------------------#

    if(!is.null(samVariates)){

      ## If available, extract catch number uncertainty and associate year vector
      res_n <- samVariates[[2]]
      catch_years <- dimnames(res_n)$year

      ## sum across fleets
      res_n <- apply(res_n, c(1,2,3), sum)

    } else {
      
      ## calculate catch numbers from sampled parameter values
      res_n <- multiSAMcay(SAMfit, variates, niter, option = 2) # age, year, iter, fleet
      res_n <- apply(res_n, c(1,2,3), sum)

    }

    ## insert into catch.n slot
    FLCore::catch.n(stk)[rownames(Cmatrix), FLCore::ac(catch_years),1,1,1,-1] <- res_n

    # -----------------------------------------------------#
    # SECTION 7.5: Update additional slots
    # -----------------------------------------------------#

    ## expand iteration dimension for landings fraction FLQuant
    lf_qnt_iter <- FLCore::propagate(lf_qnt, iter = niter)

    ## Calculate corresponding landings and discards uncertainty
    FLCore::landings.n(stk)[ac(ages), ac(catch_years)] <- lf_qnt_iter[ac(ages), ac(catch_years)] * catch.n(stk)[ac(ages), ac(catch_years)]
    FLCore::discards.n(stk)[ac(ages), ac(catch_years)] <- (1 - lf_qnt_iter[ac(ages), ac(catch_years)]) * catch.n(stk)[ac(ages), ac(catch_years)]

    ## compute total stock, catch, landings and discards weights
    FLCore::stock(stk)    <- FLCore::computeStock(stk)
    FLCore::catch(stk)    <- FLCore::computeCatch(stk)
    FLCore::landings(stk) <- FLCore::computeLandings(stk)
    FLCore::discards(stk) <- FLCore::computeDiscards(stk)
  }
  
  # ==================================================================#
  # SECTION 8: extract process error (SdLogN)
  # ==================================================================#
  #
  # NOTE: methods taken from S. Fischer - FLfse package
  
  ## get index for ages
  idx_SdLogN <- SAMfit$conf$keyVarLogN + 1
  
  ## FLQuant template
  SdLogN <- FLQuant(NA, dimnames = list(age  = dimnames(stk)$age, 
                                        year = "all",
                                        iter = 1:niter))
  
  ## insert maximum-likelihood values
  SdLogN[,,,,,1]  <- exp(SAMfit$pl$logSdLogN)[idx_SdLogN]
  
  ## (Optional) insert sampled values
  if(uncertainty == TRUE & niter > 1) {
    SdLogN[,,,,,-1] <- 
      exp(t(variates[, colnames(variates) == "logSdLogN", drop = FALSE][, idx_SdLogN]))
  }
  
  ## attach as attribute
  attr(stk, "SdLogN") <- SdLogN

  return(stk)
}

#' Summarise catch, landings, discards mean weights and landings fraction for multiple fleets
#' -----------------------------------------------------------------------------------------
#'
#' This function takes an array of mean weights or landings fraction
#' and a dataframe of catch numbers, each indexed by age, year and fleet.
#' Catch numbers are used to calculate a weighted mean for each age and year.

flt_wt_avg <- function(ar, cn){

  # The easiest way to calculate a weighted mean might be to convert
  # the array into the format used for obs data

  idx <- expand.grid(age = colnames(ar),
                     year = rownames(ar),
                     fleet = 1:dim(ar)[3])

  ## reorder to match array
  idx <- idx[order(idx$fleet,idx$age,idx$year),]
  idx$metric <- c(ar)

  ##  match with catch numbers
  Cmerge <- merge(idx, cn, all = TRUE, by = c("year","age","fleet"))

  ## Fill NA with zero - if no weight data, catch data should be removed
  Cmerge$catch[is.na(Cmerge$metric)] <- 0
  Cmerge$catch[is.na(Cmerge$catch)] <- 0

  ## calculate proportional commercial catch numbers per year, per age
  Ctotal <- tapply(Cmerge$catch, INDEX = list(Cmerge$age, Cmerge$year), sum)
  Ctotal_idx <- expand.grid(year = colnames(Ctotal),
                            age = rownames(Ctotal))
  Ctotal_idx <- Ctotal_idx[order(Ctotal_idx$year, Ctotal_idx$age),]
  Ctotal_idx$Ctotal <- c(Ctotal)

  ## If we have zero catch data, then we should weight each age-year record
  ## equally - use -99 code to mark these
  Ctotal_idx$Ctotal[Ctotal_idx$Ctotal == 0] <- -99

  ## merge catch proportions with catch mean weights
  Cmerge <- merge(Cmerge, Ctotal_idx, all = TRUE, by = c("year","age"))

  ## Calculate weighted mean for records with catch, simple mean for records without catch
  Cmerge$weighted <- Cmerge$metric * (Cmerge$catch / Cmerge$Ctotal)
  Cmean1 <- stats::aggregate(metric ~ age + year,  data = Cmerge[Cmerge$Ctotal == -99,], FUN = mean)
  Cmean2 <- stats::aggregate(weighted ~ age + year, data = Cmerge[Cmerge$Ctotal != -99,], FUN = sum)

  names(Cmean2) <- names(Cmean1)
  metricTotal <- rbind(Cmean1, Cmean2)

  ## Generate blank matrix which has full age and year dimensions
  Cmatrix <- matrix(nrow = length(unique(metricTotal[,"age"])),
                    ncol = length(unique(metricTotal[,"year"])),
                    dimnames = list(age = sort(unique(metricTotal[,"age"])),
                                    year = sort(unique(metricTotal[,"year"]))))

  ## insert summaries into blank matrix
  Cmatrix[cbind(ac(metricTotal[,"age"]), ac(metricTotal[,"year"]))] <- metricTotal[,"metric"]

  ## return summarise metric
  return(Cmatrix)
}
