#' ---
#' title: 'Functions to help condition Operating Models'
#' author: 'Matthew Pace'
#' date: 'June 2022'
#' ---
#'
#' Summary
#' =======
#'
#' This script contains several function to help condition multi-stock operating
#' models.
#'
#' The functions include:
#'
#'  - multiSAM2FLR
#'
#' Functions
#' =========
#'
#' Convert SAM fitted object into \code{FLStock} and \code{FLFleet} objects
#' ----------------------------------------------------------
#'
#' This function takes a SAM fitted stock assessment object as input and
#' returns a list of \code{FLStock} and \code{FLFleet} objects.
#'
#' Returned \code{FLStock} objects contain data for:
#' \itemize{
#' \item stock numbers-at-age (\code{stock.n})
#' \item catch numbers-at-age (\code{catch.n})
#' \item landings numbers-at-age (\code{landings.n})
#' \item discards numbers-at-age (\code{discards.n})
#' \item proportion mature-at-age (\code{mat})
#' \item natural mortality-at-age (\code{m})
#' \item fishing mortality-at-age (\code{harvest})
#' \item stock weight-at-age (\code{stock.wt})
#' \item catch weight-at-age (\code{catch.wt})}
#'
#' @param SAMfit A SAM fitted stock assessment model object or
#'               a named list of SAM fitted stock assessment model objects
#' @param useSAMcatch Optional argument. If \code{TRUE}, the fitted catches estimated
#'                    by SAM are used. Otherwise, the observed catches are used.
#'                    Defaults to \code{TRUE}.
#'
#' @return A list containing an \code{FLStock} and \code{FLFleets} object
#'
#' @section  Warning:
#' This function requires \code{FLCore} to operate.
#'
#' @export

multiSAM2FLR <- function(SAMfit,
                         useSAMcatch = TRUE) {

  ## Check that inputs are correct
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
  stock.wt(stk)[FLCore::ac(swt_age), FLCore::ac(swt_yrs)] <- stockwt

  # ------------------------------------#
  # SECTION 2.3: Natural mortality-at-age
  # ------------------------------------#

  ## Extract natural mortality-at-age (transpose to match FLQuant dimensions)
  stk_m <- t(SAMfit$data$natMor)
  if(ncol(stk_m) != length(years)) stk_m <- t(stk_m)

  ## Fill natural mortality-at-age
  FLCore::m(stk)[FLCore::ac(ages), FLCore::ac(years)] <- stk_m

  # --------------------------------#
  # SECTION 2.4: Maturity-at-age
  # --------------------------------#

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
  catch.n(stk)[FLCore::ac(ages), FLCore::ac(years)] <- Cmatrix

  # --------------------------------------#
  # SECTION 3.4: Catch mean weight-at-age
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

  FLCore::catch.wt(stk)[FLCore::ac(ages), FLCore::ac(years)] <- catchwt

  # ------------------------------------#
  # SECTION 3.2: Landings numbers-at-age
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
  landings.n(stk) <- lf_qnt * catch.n(stk)

  # -----------------------------------------#
  # SECTION 3.5: Landings mean weight-at-age
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

  FLCore::landings.wt(stk)[FLCore::ac(ages), FLCore::ac(years)] <- landwt

  # ------------------------------------#
  # SECTION 2.3: Discards numbers-at-age
  # ------------------------------------#

  ## use landings fraction to calculate discards from total catch
  discards.n(stk) <- (1 - lf_qnt) * catch.n(stk)

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

  FLCore::discards.wt(stk)[FLCore::ac(ages), FLCore::ac(years)] <- discwt

  # ------------------------------------#
  # SECTION 2.5: Fishing mortality-at-age
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

  ## Fill fishing mortality-at-age
  FLCore::harvest(stk)[FLCore::ac(ages), FLCore::ac(years)] <- Ftotal


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

  stock(stk)    <- FLCore::computeStock(stk)
  catch(stk)    <- FLCore::computeCatch(stk)
  landings(stk) <- FLCore::computeLandings(stk)
  discards(stk) <- FLCore::computeDiscards(stk)

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



###################################################################################################################
# DELETE BELOW IF NOT USEFUL!!!
###################################################################################################################

## Define a function to calculate fleet F-at-age
ffaytable <- function(fit) {

  ## row index of fleets
  fleetIndex <- which(fit$data$fleetTypes == 0)
  fleetList  <- list()

  ## perform operation for each fleet
  for(ii in fleetIndex) {

    ## Extract index of F-at-age
    fayindex <- fit$conf$keyLogFsta[ii,] + 1
    names(fayindex) <- fit$conf$minAge:fit$conf$maxAge

    ffay_fit <- t(fit$pl$logF[fayindex,])

    # matrix(0, nrow = length(fit1$data$year),
    #        ncol = length(fit1$conf$minAge:fit1$conf$maxAge),
    #        dimnames = list(year = fit1$data$year,
    #                        age  = fit1$conf$minAge:fit1$conf$maxAge))

    rownames(ffay_fit) <- fit$data$years
    colnames(ffay_fit) <- names(fayindex)[fayindex > 0]

    fleetList[ii] <- list(as.data.frame(ffay_fit) %>% mutate(Fleet = ii) %>% rownames_to_column("year"))
  }

  return(fleetList)
}


## Define a function to extract observation variance
ObsVartable <- function(fit, nfleet) {

  ObsVarKey <- fit$conf$keyVarObs[1:nfleet,]
  ObsVar    <- fit$plsd$logSdLogObs

  ObsVarData <- ObsVarKey
  ObsVarData[] <- ObsVar[ObsVarKey + 1]

  if(is.null(dim(ObsVarData))) {
    names(ObsVarData) <- fit$conf$minAge : fit$conf$maxAge
  } else {
    colnames(ObsVarData) <- fit$conf$minAge : fit$conf$maxAge
    rownames(ObsVarData) <- attr(fit$data, "fleetNames")[1:nfleet]
  }

  return(ObsVarData)
}



### function to extract logObs for a fleet
fleetLogObs <- function(fit, fleet = NULL) {

  ## Extract index for fleet observations
  idx<-fit$data$aux[,"fleet"]%in%fleet

  ## Extract fitted index-at-age for fleet
  fitted_Obs   <- exp(fit$obj$report(c(fit$sdrep$par.fixed,fit$sdrep$par.random))$predObs[idx])

  ## Not confident about this... I have 3 different logSdLogObs in parameter table... but not separate
  ## by age... however, is this better than what I have below?
  # fitted_ObsSd <- exp(sqrt(rowSums(fit$obj$report(c(fit$sdrep$par.fixed,fit$sdrep$par.random))$obsCov[[fleet]])))

  ## Extract observed index-at-age for fleet
  data_Obs <- exp(fit$data$logobs[idx])

  ## Extract ancilliary data
  ages  <- fit$data$aux[idx,"age"]
  years <- fit$data$aux[idx,"year"]

  # ages_lookup <- data.frame(Sd  = fitted_ObsSd,
  #                           age = ages)

  return(data.frame(fitted   = fitted_Obs,
                    observed = data_Obs,
                    age      = ages,
                    year     = years) # %>%
         #left_join(ages_lookup, by = "age")
  )
}
