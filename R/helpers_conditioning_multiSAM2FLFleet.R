# ---
# title: 'Conditioning: multiSAM2FLFleets'
# author: 'Matthew Pace'
# date: 'July 2022'
# ---
#
#' Convert a SAM fitted object into an \code{FLFleets} object
#'
#' This function takes a SAM fitted stock assessment object as input and
#' returns a \code{FLFleets} object.
#'
#' Returned \code{FLFleet} object contains data for:
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
#'            \code{FLCatch} information is appended onto an existing \code{FLFleet}
#'            and \code{FLMetier} structure
#' @param fleets Optional argument. An object of class \code{FLFleets} to which
#'               fitted SAM object data are extracted and appended.
#' @param yearRange   Optional argument to extend the FLStock year dimension to
#'                    a user-supplied range. Consists of an integer vector
#'                    of two elements. First element is the minimum year. Second
#'                    element is the maximum year.
#'
#' @return An \code{FLFleets} object
#'
#' @section  Warning:
#' This function requires \code{FLCore} and \code{FLFleets} to operate.
#'
#' @export

multiSAM2FLFleet <- function(SAMfit,
                             stkname = NULL,
                             useSAMcatch = TRUE,
                             add = FALSE,
                             fleets = NULL,
                             yearRange = NULL) {

  # ==================================#
  # Check that inputs are correct
  # ==================================#

  ## Stock name must be provided
  if(is.null(stkname))
    stop("Stock name (stkname) must be provided")

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
  fleet_idx  <- which(SAMfit$data$fleetTypes %in% c(0,1))
  fleet_n    <- length(fleet_idx)
  fleetnames <- attr(SAMfit$data, "fleetNames")[fleet_idx]

  # =====================================#
  # SECTION 2: Create or Append FLFleets
  # =====================================#

  ## Generate new structure if add == FALSE
  if(add == FALSE) {

    ## loop over each fleet
    fleets <- FLFleets(lapply(1:fleet_n, function(x){

      ## Generate new FLCatch
      fleetstk_x <- FLCatch(qnt)

      # =====================================#
      # SECTION 2.1:   New FLFleets
      # =====================================#
      # SECTION 2.1.1: Proportion catch retained-at-age
      # ------------------------------------------------#

      ## Extract landings fraction for xth fleet
      lf <- SAMfit$data$landFrac
      if(length(dim(lf) > 2)) {

        lf <- t(lf[,,fleetnames[x]])
        lf_qnt <- qnt
        lf_age <- rownames(lf)
        lf_yrs <- colnames(lf)
        lf_qnt[FLCore::ac(lf_age), FLCore::ac(lf_yrs)] <- lf

        landings.sel(fleetstk_x) <- lf_qnt
        discards.sel(fleetstk_x) <- (1 - lf_qnt)

      } else {

        stop("Landing fraction (landFrac) is not a 3D array")

      }

      # -------------------------------------#
      # SECTION 2.1.2: Landings numbers-at-age
      #                Discards numbers-at-age
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
      catchn <- catchn[SAMfit$data$aux[,"fleet"] %in% fleet_idx[x],]

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


      ## Calculate fisheries landings & discards
      FLCore::landings.n(fleetstk_x)[ac(ages),ac(years)] <- catch_qnt * lf_qnt[ac(ages),ac(years)]
      FLCore::discards.n(fleetstk_x)[ac(ages),ac(years)] <- catch_qnt * (1 - lf_qnt[ac(ages),ac(years)])

      # -----------------------------------------#
      # SECTION 2.1.3: Landings mean weight-at-age
      #                Discards mean weight-at-age
      # -----------------------------------------#

      landwt <- SAMfit$data$landMeanWeight
      if(length(dim(landwt) > 2)) {

        landwt <- t(landwt[,,fleetnames[x]])
        landwt_qnt <- qnt
        landwt_age <- rownames(landwt)
        landwt_yrs <- colnames(landwt)
        landwt_qnt[FLCore::ac(landwt_age), FLCore::ac(landwt_yrs)] <- landwt

        FLCore::landings.wt(fleetstk_x) <- landwt_qnt

      } else {

        stop("Landings mean weight-at-age (landMeanWeight) is not a 3D array")

      }

      discwt <- SAMfit$data$disMeanWeight
      if(length(dim(discwt) > 2)) {

        discwt <- t(discwt[,,fleetnames[x]])
        discwt_qnt <- qnt
        discwt_age <- rownames(discwt)
        discwt_yrs <- colnames(discwt)
        discwt_qnt[FLCore::ac(discwt_age), FLCore::ac(discwt_yrs)] <- discwt

        FLCore::discards.wt(fleetstk_x) <- discwt_qnt

      } else {

        stop("discards mean weight-at-age (disMeanWeight) is not a 3D array")

      }

      # -----------------------------------------#
      # SECTION 2.1.4: Landings biomass
      #                Discards biomass
      # -----------------------------------------#

      FLCore::landings(fleetstk_x) <- computeLandings(fleetstk_x)
      FLCore::discards(fleetstk_x) <- computeDiscards(fleetstk_x)

      # -----------------------------------------------#
      # SECTION 2.1.5: Partial fishing mortality-at-age
      # -----------------------------------------------#

      Fidx <- (SAMfit$conf$keyLogFsta + 1)[fleet_idx[x],] # index for fleet x
      F_matrix <- exp(SAMfit$pl$logF)[Fidx,]
      F_qnt <- qnt
      F_qnt[FLCore::ac(ages),FLCore::ac(years)] <- F_matrix

      ## Calculate selectivity-at-age as a proportion of
      ## summed partial fishing mortality-at-age
      catchSel <- sweep(F_qnt, c(2:6), apply(F_qnt, c(2:6), sum), "/")

      # For some inscrutable reason catchSel is considered an array
      # whe the package is built (note that this does not happen
      # when the function is manually sourced...)
      #
      # We must therefore coerce it into an FLQuant
      Sel_qnt <- qnt
      Sel_qnt[] <- catchSel

      ## Scale retained catch proportions by catch selectivity
      landingsSel <- landings.sel(fleetstk_x) * Sel_qnt
      discardsSel <- discards.sel(fleetstk_x) * Sel_qnt

      landings.sel(fleetstk_x)[FLCore::ac(ages),FLCore::ac(years)] <- landingsSel[FLCore::ac(ages),FLCore::ac(years)]
      discards.sel(fleetstk_x)[FLCore::ac(ages),FLCore::ac(years)] <- discardsSel[FLCore::ac(ages),FLCore::ac(years)]

      ## Store as an ad hoc attribute
      attr(fleetstk_x, "partF") <- F_qnt

      # --------------------------------------------#
      # SECTION 2.1.6: (Optional) expand year range
      # --------------------------------------------#

      if (!is.null(yearRange)) {
        fleetstk_x <- expand(fleetstk_x, year = yearRange[1]:yearRange[2])
      }

      # -----------------------------------------#
      # SECTION 2.1.7: Insert into FLFleet
      # -----------------------------------------#

      ## Create a blank FLFleet structure containing FLMetier and FLCatches
      fleet <- FLFleet::FLFleet(FLFleet::FLMetier(name = "mtr", catches = FLFleet::FLCatches(fleetstk_x)))

      ## Define FLCatch stock name
      fleet@metiers$met@catches@names <- stkname

      ## Return fleets
      return(fleet)

    }))

    fleets@names <- fleetnames

  } else if(add == TRUE){

    # ============================================#
    # SECTION 2.2:   Append FLFleets (existing)
    # ============================================#

    # Some fleets may already exist, others might need
    # to be generated. First identify relevant fleets that
    # exist within FLFleets object, then generate any
    # additional FLFleet objects as required!

    existingfleets <- which(names(fleets) %in% fleetnames)
    newfleets      <- which(!(fleetnames %in% names(fleets)))

    if(length(existingfleets) > 0) {
      for(x in existingfleets) {

        ## how many stocks currently caught by fishery?
        nstkfleet <- length(fleets[[x]]@metiers$met@catches)

        ## Generate new FLCatch
        fleetstk_x <- FLFleet::FLCatch(qnt)

        # --------------------------------------------#
        # SECTION 2.2.1: catch selection-at-age
        # --------------------------------------------#

        ## Extract landings fraction for xth fleet
        lf <- SAMfit$data$landFrac
        if(length(dim(lf) > 2)) {

          lf <- t(lf[,,fleetnames %in% names(fleets)[x]])
          lf_qnt <- qnt
          lf_age <- rownames(lf)
          lf_yrs <- colnames(lf)
          lf_qnt[FLCore::ac(lf_age), FLCore::ac(lf_yrs)] <- lf

          landings.sel(fleetstk_x) <- lf_qnt
          discards.sel(fleetstk_x) <- (1 - lf_qnt)

        } else {

          stop("Landing fraction (landFrac) is not a 3D array")

        }

        # -------------------------------------#
        # SECTION 2.2.2: Landings numbers-at-age
        #                Discards numbers-at-age
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
        catchn <- catchn[SAMfit$data$aux[,"fleet"] %in% which(fleetnames %in% names(fleets))[x],]

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


        ## Calculate fisheries landings & discards
        FLCore::landings.n(fleetstk_x)[ac(ages),ac(years)] <- catch_qnt * lf_qnt[ac(ages),ac(years)]
        FLCore::discards.n(fleetstk_x)[ac(ages),ac(years)] <- catch_qnt * (1 - lf_qnt[ac(ages),ac(years)])

        # -----------------------------------------#
        # SECTION 2.2.3: Landings mean weight-at-age
        #                Discards mean weight-at-age
        # -----------------------------------------#

        landwt <- SAMfit$data$landMeanWeight
        if(length(dim(landwt) > 2)) {

          landwt <- t(landwt[,,fleetnames %in% names(fleets)[x]])
          landwt_qnt <- qnt
          landwt_age <- rownames(landwt)
          landwt_yrs <- colnames(landwt)
          landwt_qnt[FLCore::ac(landwt_age), FLCore::ac(landwt_yrs)] <- landwt

          FLCore::landings.wt(fleetstk_x) <- landwt_qnt

        } else {

          stop("Landings mean weight-at-age (landMeanWeight) is not a 3D array")

        }

        discwt <- SAMfit$data$disMeanWeight
        if(length(dim(discwt) > 2)) {

          discwt <- t(discwt[,,fleetnames %in% names(fleets)[x]])
          discwt_qnt <- qnt
          discwt_age <- rownames(discwt)
          discwt_yrs <- colnames(discwt)
          discwt_qnt[FLCore::ac(discwt_age), FLCore::ac(discwt_yrs)] <- discwt

          FLCore::discards.wt(fleetstk_x) <- discwt_qnt

        } else {

          stop("discards mean weight-at-age (disMeanWeight) is not a 3D array")

        }

        # -----------------------------------------#
        # SECTION 2.2.4: Landings biomass
        #                Discards biomass
        # -----------------------------------------#

        FLCore::landings(fleetstk_x) <- computeLandings(fleetstk_x)
        FLCore::discards(fleetstk_x) <- computeDiscards(fleetstk_x)

        # -----------------------------------------------#
        # SECTION 2.2.5: Partial fishing mortality-at-age
        # -----------------------------------------------#

        Fidx <- (SAMfit$conf$keyLogFsta + 1)[fleet_idx[fleetnames %in% names(fleets)[x]],] # index for fleet x
        F_matrix <- exp(SAMfit$pl$logF)[Fidx,]
        F_qnt <- qnt
        F_qnt[FLCore::ac(ages),FLCore::ac(years)] <- F_matrix

        ## Calculate selectivity-at-age as a proportion of
        ## summed partial fishing mortality-at-age
        catchSel <- sweep(F_qnt, c(2:6), apply(F_qnt, c(2:6), sum), "/")
        Sel_qnt <- qnt
        Sel_qnt[] <- catchSel

        ## Scale retained catch proportions by catch selectivity
        landingsSel <- landings.sel(fleetstk_x) * Sel_qnt
        discardsSel <- discards.sel(fleetstk_x) * Sel_qnt

        landings.sel(fleetstk_x) <- landingsSel
        discards.sel(fleetstk_x) <- discardsSel

        ## Store as an ad hoc attribute
        attr(fleetstk_x, "partF") <- F_qnt

        # --------------------------------------------#
        # SECTION 2.2.6: (Optional) expand year range
        # --------------------------------------------#

        if (!is.null(yearRange)) {
          fleetstk_x <- expand(fleetstk_x, year = yearRange[1]:yearRange[2])
        }

        # -----------------------------------------#
        # SECTION 2.2.7: Insert into FLFleet
        # -----------------------------------------#

        ## Add FLCatch and define stock name
        fleets[[x]]@metiers[[1]]@catches[[nstkfleet + 1]] <- fleetstk_x
        names(fleets[[x]]@metiers[[1]]@catches)[nstkfleet + 1] <- stkname

      } # end loop over existing fleets
    }

    # ============================================#
    # SECTION 2.3:   Append FLFleets (new)
    # ============================================#

    ## Add new fleets to FLFleets if necessary
    if(length(newfleets) > 0) {
      for(x in newfleets){

        ## Generate new FLCatch
        fleetstk_x <- FLFleet::FLCatch(qnt)

        # --------------------------------------------#
        # SECTION 2.3.1: catch selection-at-age
        # --------------------------------------------#

        ## Extract landings fraction for xth fleet
        lf <- SAMfit$data$landFrac
        if(length(dim(lf) > 2)) {

          lf <- t(lf[,,fleetnames[x]])
          lf_qnt <- qnt
          lf_age <- rownames(lf)
          lf_yrs <- colnames(lf)
          lf_qnt[FLCore::ac(lf_age), FLCore::ac(lf_yrs)] <- lf

          landings.sel(fleetstk_x) <- lf_qnt
          discards.sel(fleetstk_x) <- (1 - lf_qnt)

        } else {

          stop("Landing fraction (landFrac) is not a 3D array")

        }

        # -------------------------------------#
        # SECTION 2.3.2: Landings numbers-at-age
        #                Discards numbers-at-age
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
        catchn <- catchn[SAMfit$data$aux[,"fleet"] %in% x,]

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


        ## Calculate fisheries landings & discards
        FLCore::landings.n(fleetstk_x)[ac(ages),ac(years)] <- catch_qnt * lf_qnt[ac(ages),ac(years)]
        FLCore::discards.n(fleetstk_x)[ac(ages),ac(years)] <- catch_qnt * (1 - lf_qnt[ac(ages),ac(years)])

        # -----------------------------------------#
        # SECTION 2.3.3: Landings mean weight-at-age
        #                Discards mean weight-at-age
        # -----------------------------------------#

        landwt <- SAMfit$data$landMeanWeight
        if(length(dim(landwt) > 2)) {

          landwt <- t(landwt[,,fleetnames[x]])
          landwt_qnt <- qnt
          landwt_age <- rownames(landwt)
          landwt_yrs <- colnames(landwt)
          landwt_qnt[FLCore::ac(landwt_age), FLCore::ac(landwt_yrs)] <- landwt

          FLCore::landings.wt(fleetstk_x) <- landwt_qnt

        } else {

          stop("Landings mean weight-at-age (landMeanWeight) is not a 3D array")

        }

        discwt <- SAMfit$data$disMeanWeight
        if(length(dim(discwt) > 2)) {

          discwt <- t(discwt[,,fleetnames[x]])
          discwt_qnt <- qnt
          discwt_age <- rownames(discwt)
          discwt_yrs <- colnames(discwt)
          discwt_qnt[FLCore::ac(discwt_age), FLCore::ac(discwt_yrs)] <- discwt

          FLCore::discards.wt(fleetstk_x) <- discwt_qnt

        } else {

          stop("discards mean weight-at-age (disMeanWeight) is not a 3D array")

        }

        # -----------------------------------------#
        # SECTION 2.3.4: Landings biomass
        #                Discards biomass
        # -----------------------------------------#

        FLCore::landings(fleetstk_x) <- computeLandings(fleetstk_x)
        FLCore::discards(fleetstk_x) <- computeDiscards(fleetstk_x)

        # -----------------------------------------------#
        # SECTION 2.3.5: Partial fishing mortality-at-age
        # -----------------------------------------------#

        Fidx <- (SAMfit$conf$keyLogFsta + 1)[fleet_idx[fleetnames %in% fleetnames[x]],] # index for fleet x
        F_matrix <- exp(SAMfit$pl$logF)[Fidx,]
        F_qnt <- qnt
        F_qnt[FLCore::ac(ages),FLCore::ac(years)] <- F_matrix

        ## Calculate selectivity-at-age as a proportion of
        ## summed partial fishing mortality-at-age
        catchSel <- sweep(F_qnt, c(2:6), apply(F_qnt, c(2:6), sum), "/")
        Sel_qnt <- qnt
        Sel_qnt[] <- catchSel

        ## Scale retained catch proportions by catch selectivity
        landingsSel <- landings.sel(fleetstk_x) * Sel_qnt
        discardsSel <- discards.sel(fleetstk_x) * Sel_qnt

        landings.sel(fleetstk_x) <- landingsSel
        discards.sel(fleetstk_x) <- discardsSel

        ## Store as an ad hoc attribute
        attr(fleetstk_x, "partF") <- F_qnt

        # --------------------------------------------#
        # SECTION 2.3.6: (Optional) expand year range
        # --------------------------------------------#

        if (!is.null(yearRange)) {
          fleetstk_x <- expand(fleetstk_x, year = yearRange[1]:yearRange[2])
        }

        # -----------------------------------------#
        # SECTION 2.3.7: Insert into FLFleets
        # -----------------------------------------#

        ## How many fleets currently exist?
        nfishery <- length(fleets)

        ## Add new fishery
        fleets[[nfishery + 1]] <- FLFleet::FLFleet(FLFleet::FLMetier(name = "mtr", catches = FLFleet::FLCatches(fleetstk_x)))

        ## Define FLCatch stock name
        names(fleets[[nfishery + 1]]) <- stkname

        ## Define new FLFleet name
        names(fleets)[nfishery + 1] <- fleetnames[x]

      } # end loop over new fleets
    }
  } else {

    stop("'add' must be TRUE or FALSE")

  }
  return(fleets)
}
