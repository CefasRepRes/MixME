# ---
# title: 'Conditioning: multiSAM2FLFishery'
# author: 'Matthew Pace'
# date: 'July 2022'
# ---
#
#' Convert a SAM fitted object into an \code{FLFisheries} object
#'
#' This function takes a SAM fitted stock assessment object as input and
#' returns a \code{FLFisheries} object.
#'
#' Returned \code{FLFisheries} object contains data for one or more
#' \code{FLFishery} objects. Each \code{FLFishery} object contains the following
#' slots:
#' \itemize{
#' \item capacity (\code{catch.q})
#' \item effort (\code{landings.n})
#' \item landings mean weight-at-age (\code{landings.wt})
#' \item proportion catch retained-at-age (\code{landings.sel})
#' \item discards numbers-at-age (\code{discards.n})
#' \item discards mean weight-at-age (\code{discards.wt})}
#'
#' Additionally, each \code{FLFishery} object contains one or more \code{FLCatch}
#' object with the following slots:
#'
#' \itemize{
#' \item landings numbers-at-age (\code{landings.n})
#' \item landings mean weight-at-age (\code{landings.wt})
#' \item discards numbers-at-age (\code{discards.n})
#' \item discards mean weight-at-age (\code{discards.wt})
#' \item proportional selectivity-at-age (\code{catch.sel})
#' \item mean price-per-unit-weight-at-age (\code{price})}
#'
#'
#' NOTE: The partial fishing mortality-at-age for each stock is inserted in
#'       the respective catchability-at-age slot. Further processing into
#'       catchability-at-age is needed.
#'
#' @param SAMfit SAM fitted stock assessment model object of class \code{sam}
#' @param stkname Character. Stock name associated with SAM object.
#' @param useSAMcatch Optional argument. If \code{TRUE}, the fitted catches estimated
#'                    by SAM are used. Otherwise, the observed catches are used.
#'                    Defaults to \code{TRUE}.
#' @param add Optional argument. Defaults to \code{FALSE}. If \code{TRUE},
#'            \code{FLCatch} information is appended onto an existing \code{FLFishery}
#'            structure
#' @param fleets Optional argument. An object of class \code{FLFisheries} to which
#'               fitted SAM object data are extracted and appended.
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
#' @return An \code{FLFishery} object
#'
#' @section  Warning:
#' This function requires \code{FLCore} to operate.
#'
#' @export

multiSAM2FLFishery <- function(SAMfit,
                               stkname = NULL,
                               useSAMcatch = TRUE,
                               add = FALSE,
                               fleets = NULL,
                               yearRange = NULL,
                               uncertainty = FALSE,
                               niter = 1000,
                               samVariates = NULL,
                               seed = NULL) {

  # ----------------------------------#
  # 1. Define stock dimensions
  # 2. (Optional) Sample replicates from MVN distribution if these are not provided
  #
  # 3.  Create or Append FLFisheries
  # 3.1 ... New FLFisheries
  # 3.1.1   ... Generate FLCatch for i'th Fleet
  # 3.1.2   ... Insert into FLFishery
  #
  # 3.2 ... Append FLFisheries (existing)
  # 3.2.1   ... Generate FLCatch for i'th Fleet
  # 3.2.2   ... Insert into FLFishery
  #
  # 3.3 ... Append FLFisheries (new)
  # 3.3.1   ... Generate FLCatch for i'th Fleet
  # 3.3.2   ... Insert into FLFishery
  #
  # ----------------------------------#

  # ==================================#
  # Check that inputs are correct
  # ==================================#

  ## Stock name must be provided
  if(is.null(stkname))
    stop("Stock name (stkname) must be provided")

  ## If add == TRUE, then FLFishery object must be provided
  if(is.null(fleets) & add == TRUE)
    stop("If add == TRUE, fleets must be provided as an 'FLFisheries' object")

  ## Fleets in the SAM object must be named


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
                                             iter = 1:ifelse(uncertainty == TRUE,
                                                           niter, 1)))

  ## How many commercial fleets? - do not process aggregated fleet
  fleet_idx  <- which(SAMfit$data$fleetTypes %in% c(0,1))
  fleet_n    <- length(fleet_idx)
  fleetnames <- attr(SAMfit$data, "fleetNames")[fleet_idx]

  # =========================================================================================#
  # SECTION 2: (Optional) Sample replicates from MVN distribution if these are not provided
  # =========================================================================================#

  if(isTRUE(uncertainty)) {

    ## Only sample if sampled variates are not provided
    if(!is.null(samVariates)) {

      variates <- samVariates[[1]]
      res_n    <- samVariates[[2]]

    } else {

      ## Set random number seed if provided
      if(!is.null(seed)) set.seed(seed)

      # ------------------------------------------#
      # Sample replicates from MVN distribution
      # ------------------------------------------#

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
      
      colnames(variates) <- names(est)

      # ------------------------------------------#
      # Calculate corresponding predicted catches
      # ------------------------------------------#

      ### catch fleet index/indices
      catch_fleets <- which(SAMfit$data$fleetTypes == 0)
      catch_desc <- SAMfit$data$aux

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
                         dim = c(length(unique(tmp[,"age"])), length(unique(tmp[,"year"])), length(catch_fleets)),
                         dimnames = list(age = sort(unique(tmp[,"age"])),
                                         year = sort(unique(tmp[,"year"]))))

        ## insert catches into blank matrix
        Cmatrix[] <- tmp[,"est"]

        return(Cmatrix)

      }, simplify = "array"))
    }
  } else {

    ## Generate null objects if no uncertainty
    variates <- NULL
    res_n    <- NULL

  }

  # =====================================#
  # SECTION 3: Create or Append FLFisheries
  # =====================================#

  ## Generate new structure if add == FALSE
  if(add == FALSE) {

    # =====================================#
    # SECTION 3.1:   New FLFisheries
    # =====================================#

    # If we have no fleet structure to add to, we can simply loop over each
    # fleet described in the fitted multifleet SAM object and generate an
    # FLCatch and FLFishery object.
    #
    # Indexing fleet information is simple. We index using fleet names for
    # landings, discards and landings fraction data (because the array dimensions
    # are names), but use the fleet position to index catch and fishing mortality
    # data (because fleets are not explicitly named in these objects).

    ## loop over each fleet
    fleets <- FLFisheries(lapply(1:fleet_n, function(x){

      # -----------------------------------------------#
      # SECTION 3.1.1: Generate FLCatch for i'th Fleet
      # -----------------------------------------------#

      fleetstk_x <-  makeFLCatch(SAMfit, qnt, useSAMcatch, yearRange, variates, res_n,
                                 land_disc_idx = fleetnames[x],
                                 catch_idx = fleet_idx[x],
                                 fishing_idx = fleet_idx[x])

      # -----------------------------------------#
      # SECTION 3.1.2: Insert into FLFishery
      # -----------------------------------------#

      ## Create an FLFishery container to store FLCatches
      fleet <- FLFishery::FLFishery(FLFishery::FLCatches(list(fleetstk_x)))

      ## Define FLCatch stock name
      names(fleet) <- stkname

      ## Return fleets
      return(fleet)

    }))

    ## Define fleet names
    names(fleets) <- fleetnames

  ## Append to existing structure if add == TRUE
  } else if(add == TRUE){

    # Some fleets may already exist, others might need to be generated.
    # This is handled in two steps:
    #
    #   1. First identify relevant fleets that already exist within FLFisheries
    #      object. Loop over each of these and generate a new FLCatch object.
    #      Append this to an existing FLFishery.
    #   2. Loop over fleets that do not currently exist. Generate a new
    #      FLCatch and FLFishery and add these to the supplied FLFisheries object.

    existingfleets <- which(names(fleets) %in% fleetnames)
    newfleets      <- which(!(fleetnames %in% names(fleets)))

    # ============================================#
    # SECTION 3.2:   Append FLFisheries (existing)
    # ============================================#

    # To append to existing FLFisheries objects, the indexing of data is a little
    # more complex. For the landings, discards and landings fractions data,
    # we use the x'th existing fleet name. For catch data and fishing mortality
    # data, we use the position of the x'th existing fleet name in the vector
    # of multifleet SAM commercial fleet names to index a vector of
    # positions of multifleet SAM commercial fleets.

    if(length(existingfleets) > 0) {
      for(x in existingfleets) {

        ## how many stocks currently caught by fishery?
        nstkfishery <- length(fleets[[x]])

        # -----------------------------------------------#
        # SECTION 3.2.1: Generate FLCatch for i'th Fleet
        # -----------------------------------------------#

        fleetstk_x <-  makeFLCatch(SAMfit, qnt, useSAMcatch, yearRange, variates, res_n,
                                   land_disc_idx = names(fleets)[x],
                                   catch_idx = fleet_idx[fleetnames %in% names(fleets)[x]],
                                   fishing_idx = fleet_idx[fleetnames %in% names(fleets)[x]])

        # -----------------------------------------#
        # SECTION 3.2.2: Insert into FLFishery
        # -----------------------------------------#

        ## Add FLCatch and define stock name
        fleets[[x]][[nstkfishery + 1]] <- fleetstk_x
        names(fleets[[x]])[nstkfishery + 1] <- stkname

      } # end loop over existing fleets
    }

    # ============================================#
    # SECTION 3.3:   Append FLFisheries (new)
    # ============================================#

    # To append new FLFishery objects to existing FLFisheries objects, the
    # indexing of data is also a little more complex.
    #
    # For the landings, discards and landings fractions data, we use the
    # x'th new fleet name (within a vector of multifleet SAM commercial fleet names). For
    # catch data and fishing mortality data, we use the position of the x'th new fleet
    # in the vector of multifleet SAM commercial fleets.

    ## Add new fleets to FLFisheries if necessary
    if(length(newfleets) > 0) {
      for(x in newfleets){

        # -----------------------------------------------#
        # SECTION 3.3.1: Generate FLCatch for i'th Fleet
        # -----------------------------------------------#

        fleetstk_x <-  makeFLCatch(SAMfit, qnt, useSAMcatch, yearRange, variates, res_n,
                                   land_disc_idx = fleetnames[x],
                                   catch_idx = fleet_idx[x],
                                   fishing_idx = fleet_idx[x])

        # -----------------------------------------#
        # SECTION 3.3.2: Insert into FLFishery
        # -----------------------------------------#

        ## How many fleets currently exist?
        nfishery <- length(fleets)

        ## Add new fishery
        fleets[[nfishery + 1]] <- FLFishery::FLFishery(FLFishery::FLCatches(list(fleetstk_x)))

        ## Define FLCatch stock name
        names(fleets[[nfishery + 1]]) <- stkname

        ## Define new FLFishery name
        names(fleets)[nfishery + 1] <- fleetnames[x]

      } # end loop over new fleets
    }
  } else {

    stop("'add' must be TRUE or FALSE")

  }

  return(fleets)
}

#' Extract and process landings and discards numbers and weights, selectivity, and fishing mortality
#' -------------------------------------------------------------------------------------------------
#'
#' This function extracts the relevant catch information for a given fleet from
#' a fitted multifleet SAM model. This is called once per fleet within 'multiSAM2FLFishery'
#'
#' The outputted FLCatch object may be used to define a new FLFishery and
#' FLFisheries, define a new FLFishery within an existing FLFisheries, or be
#' appended to an existing FLFishery. Therefore the function may be called within
#' any of these three contexts for a given stock as multiSAM2FLFishery loops over
#' each fleet.
#'
#' Supplying the correct indices to the function is critical to appropriately
#' subsetting the data for the correct fleet.

makeFLCatch <- function(SAMfit,        # fitted SAM object
                        qnt,           # blank FLQuant
                        useSAMcatch,   # use predicted catches from SAM?
                        yearRange,     # (optional) min and max year to extend year dimension
                        variates,      # matrix of sample variates (replicates x variables)
                        res_n,         # array of sampled fleet catch numbers [age, year, fleet, iteration]
                        land_disc_idx, # index to subset landings, discards, landings fraction data
                        catch_idx,     # index to subset catch numbers data
                        fishing_idx){  # index to subset fishing mortality data

  ## Generate new FLCatch
  fleetstk_x <- FLFishery::FLCatch(qnt)

  ## extract age, year and iteration dimensions
  ages       <- SAMfit$conf$minAge:SAMfit$conf$maxAge
  years      <- SAMfit$data$years
  niter      <- dims(qnt)$iter

  # -------------------------------------#
  # 1. Catch retained-at-age
  # 2. Landings numbers-at-age
  #    Discards numbers-at-age
  # 3. Landings mean weight-at-age
  #    Discards mean weight-at-age
  # 4. Partial fishing mortality-at-age
  #    Catch selection at-age
  # 5. (Optional) expand year range
  # -------------------------------------#

  # Note that uncertainty is handled within each subsection rather than in a
  # single step.

  # -------------------------------------#
  # SECTION 1: catch retained-at-age
  # -------------------------------------#

  # This is not saved within the FLCatch
  # object but is calculated from landings and
  # discards numbers

  ## Extract landings fraction for xth fleet
  lf <- SAMfit$data$landFrac
  if(length(dim(lf) > 2)) {

    lf <- t(lf[,,land_disc_idx])
    lf_qnt <- qnt
    lf_age <- rownames(lf)
    lf_yrs <- colnames(lf)
    lf_qnt[FLCore::ac(lf_age), FLCore::ac(lf_yrs)] <- lf

  } else {

    stop("Landing fraction (landFrac) is not a 3D array")

  }

  # -------------------------------------#
  # SECTION 2: Landings numbers-at-age
  #            Discards numbers-at-age
  # -------------------------------------#

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

  ## select commercial fleets
  catchn <- catchn[SAMfit$data$aux[,"fleet"] %in% catch_idx,]

  ## Convert year vector to actual years
  if(!(min(catchn[,"year"]) %in% years)) {

    ## update year index to actual year
    catchn[,"year"] <- catchn[,"year"] + min(years) - 1

  }

  # Next re-organise the year index and create an empty matrix to cater for cases
  # where there are missing data

  ## Generate blank matrix which has full age and year dimensions
  Cmatrix <- matrix(nrow = length(ages),
                    ncol = length(years),
                    dimnames = list(age = ages,
                                    year = years))

  ## insert catches into blank matrix
  Cmatrix[cbind(ac(catchn[,"age"]), ac(catchn[,"year"]))] <- catchn[,"catch"]
  Cmatrix[is.na(Cmatrix)] <- 0

  ## insert catch matrix into FLQuant
  catch_qnt <- qnt
  catch_qnt[ac(ages),ac(years)] <- Cmatrix

  ## Insert catch variates if available (i.e. uncertainty == TRUE)
  if(!is.null(res_n)){
    res_n_ages  <- dimnames(res_n)$age
    res_n_years <- dimnames(res_n)$year
    catch_qnt[res_n_ages,res_n_years,1,1,1,-1] <- res_n[,,catch_idx,]
  }

  ## Calculate fisheries landings & discards
  FLFishery::landings.n(fleetstk_x)[ac(ages),ac(years)] <- catch_qnt * lf_qnt[ac(ages),ac(years)]
  FLFishery::discards.n(fleetstk_x)[ac(ages),ac(years)] <- catch_qnt * (1 - lf_qnt[ac(ages),ac(years)])

  # -----------------------------------------#
  # SECTION 3: Landings mean weight-at-age
  #            Discards mean weight-at-age
  # -----------------------------------------#

  landwt <- SAMfit$data$landMeanWeight
  if(length(dim(landwt) > 2)) {

    landwt <- t(landwt[,,land_disc_idx])
    landwt_qnt <- qnt
    landwt_age <- rownames(landwt)
    landwt_yrs <- colnames(landwt)
    landwt_qnt[FLCore::ac(landwt_age), FLCore::ac(landwt_yrs)] <- landwt

    FLFishery::landings.wt(fleetstk_x) <- landwt_qnt

  } else {

    stop("Landings mean weight-at-age (landMeanWeight) is not a 3D array")

  }

  discwt <- SAMfit$data$disMeanWeight
  if(length(dim(discwt) > 2)) {

    discwt <- t(discwt[,,land_disc_idx])
    discwt_qnt <- qnt
    discwt_age <- rownames(discwt)
    discwt_yrs <- colnames(discwt)
    discwt_qnt[FLCore::ac(discwt_age), FLCore::ac(discwt_yrs)] <- discwt

    FLFishery::discards.wt(fleetstk_x) <- discwt_qnt

  } else {

    stop("discards mean weight-at-age (disMeanWeight) is not a 3D array")

  }

  # -----------------------------------------------#
  # SECTION 4: Partial fishing mortality-at-age
  #            Catch selection-at-age
  # -----------------------------------------------#

  Fidx <- (SAMfit$conf$keyLogFsta + 1)[fishing_idx,] # index for fleet x
  Fidx <- Fidx[Fidx > 0] # remove ages without F-at-age
  F_matrix <- exp(SAMfit$pl$logF)[Fidx,]
  F_qnt <- qnt
  F_qnt[] <- 0 # default value is 0
  F_qnt[unique(FLCore::ac(catchn[,"age"])),FLCore::ac(years)] <- F_matrix

  ## Insert partial fishing mortality variates if available (i.e. uncertainty == TRUE)
  if(!is.null(variates)){

    ## Extract fishing mortality variates to a separate object
    Fvariates <- variates[, colnames(variates) == "logF"]

    # because I am subsetting sampled logF for a multifleet model, logF columns
    # are associated with both age and year.

    ## generate an index to handle ages and years
    idxy <- c(sapply(1:length(years), function(y) Fidx + max(SAMfit$conf$keyLogFsta + 1)*(y-1)))

    ## generate an array to hold age, year, iteration data
    F_uncertainty <- array(NA, dim = c(length(Fidx), length(years), niter-1))
    F_uncertainty[] <- c(exp(t(Fvariates[,idxy])))

    ## insert sampled partial F-at-age into FLQuant
    F_qnt[unique(FLCore::ac(catchn[,"age"])),,1,1,1,-1] <- F_uncertainty
  }

  ## Calculate selectivity-at-age as a proportion of
  ## summed partial fishing mortality-at-age
  catchSel <- sweep(F_qnt, c(2:6), apply(F_qnt, c(2:6), sum), "/")
  Sel_qnt <- qnt
  Sel_qnt[] <- catchSel

  FLFishery::catch.sel(fleetstk_x) <- Sel_qnt

  ## Store as an ad hoc attribute
  attr(fleetstk_x, "partF") <- F_qnt

  # --------------------------------------------#
  # SECTION 5: (Optional) expand year range
  # --------------------------------------------#

  if (!is.null(yearRange)) {
    fleetstk_x <- FLCore::expand(fleetstk_x, year = yearRange[1]:yearRange[2])
  }

  return(fleetstk_x)
}
