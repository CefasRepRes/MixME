# ---
# title: 'Operating Model - Observation error model'
# author: 'Matthew Pace'
# date: 'September 2022'
# ---
#
#' Implementation of stock observation methods
#'
#' Function to generate stock observations (fleet catches and survey indices)
#' with (optional) uncertainty.
#'
#' @param om An operating model
#' @param deviances ...
#' @param observations ...
#' @param args List of additional arguments
#' @param tracking A named list of tracking objects to monitor emergent dynamic
#'                 properties
#' @param catch_timing A named list of catch timing relative to assessment year
#' @param idx_timing A named list of survey index timing relative to assessment year
#' @param use_stk_oem Logical vector. Length \code{n} stocks. Should a supplied
#'                    Observation Error Model structure be used?
#' @param use_catch_residuals Logical vector. Length \code{n} stocks. Should
#'                            catch residuals be used to generate uncertainty?
#' @param use_idx_residuals Logical vector. Length \code{n} stocks. Should
#'                          survey index residuals be used to generate uncertainty?
#'
#' @return A named list of stock observations, survey indices and updated tracking
#'         object
#'
#' @export

oemMixME <- function(om,
                    deviances,
                    observations,
                    args,
                    tracking,
                    catch_timing = NULL, # catch timing relative to ay
                    idx_timing   = NULL, # index timing relative to ay
                    use_stk_oem         = FALSE,
                    use_catch_residuals = FALSE,
                    use_idx_residuals   = FALSE) {

  # SHOULD I BE CLIPPING THE OBSERVATION TIMESERIES TO THE MOST RECENT DATA YEAR?

  # ===================================#
  # Error checking
  # ===================================#

  ## Check that correct structures supplied
  if(!all(names(observations) %in% c("stk","flt","idx")))
    stop("'observations' must be a named list. Names may be 'stk', 'flt' or 'idx'")
  if(!all(names(deviances)    %in% c("stk","flt","idx")))
    stop("'Deviances' must be a named list. Names may be 'stk', 'flt' or 'idx'")

  ## Check that the correct object classes are supplied
  if(!(class(observations$stk) %in% c("FLStocks","FLBiols")))
    stop("'stk' must be of class FLStocks or FLBiols")

  if(!is.null(observations$flt)) {
    if(class(observations$flt) != "FLFisheries")
      stop("'flt' must be of class FLFisheries")
  }

  # ===================================#
  # SECTION 1: Extract global arguments
  # ===================================#

  ## extract timings
  iy   <- args$iy             # initial projection year
  ay   <- args$ay             # current (assessment) year
  mlag <- args$management_lag # management lag

  ## Process logical arguments if required
  if(length(use_stk_oem) == 1 & is.logical(use_stk_oem)) {
    use_stk_oem <- sapply(observations$stk@names, function(x) use_stk_oem,
                          USE.NAMES = TRUE)
  }
  if(length(use_catch_residuals) == 1 & is.logical(use_catch_residuals)) {
    use_catch_residuals <- sapply(observations$stk@names, function(x) use_catch_residuals,
                                  USE.NAMES = TRUE)
  }
  if(length(use_idx_residuals) == 1 & is.logical(use_idx_residuals)) {
    use_idx_residuals <- sapply(observations$stk@names, function(x) use_idx_residuals,
                                USE.NAMES = TRUE)
  }

  ## Process catch and index timings
  if(is.null(catch_timing)) {
    catch_timing <- sapply(observations$stk@names, function(x) 0,
           USE.NAMES = TRUE, simplify = FALSE)
  }

  if(is.null(idx_timing)) {
    idx_timing <- sapply(observations$stk@names, function(x) 0,
           USE.NAMES = TRUE, simplify = FALSE)
  }

  # =======================================#
  # SECTION 2: Iterative stock observation
  # =======================================#

  ## Loop over each observed stock
  oemList <- lapply(observations$stk@names, function(x){

    # ------------------------------------#
    # SECTION 2.2: Observed stock catches #
    # ------------------------------------#

    # ---------------------------------------------------#
    # (OPTION I) Use oem observation structure (FLStock) #
    # ---------------------------------------------------#
    # This allows for different biological parameters
    # (weights, maturity, mortality)

    if(use_stk_oem[x] == TRUE & is.null(observations$flt)) {

      ## use observations object
      stk0 <- observations$stk[[x]]

      # If perfect observations
      # -----------------------#

      if(use_catch_residuals[x] == FALSE){

        ## Calculate overall landings and discards
        fltlandings <- sapply(1:length(om$flts), function(y){
          om$flts[[y]][[x]]@landings.n
        }, simplify = "array")
        fltdiscards <- sapply(1:length(om$flts), function(y){
          om$flts[[y]][[x]]@discards.n
        }, simplify = "array")

        ## Sum over fleets
        stk0landings <- apply(fltlandings, c(1:6), sum)
        stk0discards <- apply(fltdiscards, c(1:6), sum)

        ## update landings, discards and catch numbers in observed stock object
        stk0@landings.n[] <- stk0landings
        stk0@discards.n[] <- stk0discards
        stk0@catch.n      <- stk0@landings.n + stk0@discards.n
      }

      # If observations with uncertainty
      # --------------------------------#

      if(use_catch_residuals[x] == TRUE){

        ## Extract overall catch numbers-at-age
        fltcatches <- sapply(1:length(om$flts), function(y){
          catch.n(om$flts[[y]][[x]])
        }, simplify = "array")

        ## Extract landings numbers-at-age
        fltlandings <- sapply(1:length(om$flts), function(y){
          om$flts[[y]][[x]]@landings.n
        }, simplify = "array")

        ## Implement catch numbers-at-age uncertainty
        flt0catches <- fltcatches * deviances$stk[[x]]$catch.dev

        ## Calculate landings fraction
        catchSel <- sweep(fltlandings, c(1:7), fltcatches, "/")

        ### split catch into discards and landings, based on landing fraction
        flt0landings <- sweep(flt0catches, c(1:7), catchSel, "*")
        flt0discards <- sweep(flt0catches, c(1:7), (1 - catchSel), "*")

        ## Sum over fleets
        stk0landings <- apply(flt0landings, c(1:6), sum)
        stk0discards <- apply(flt0discards, c(1:6), sum)

        ## update landings, discards and catch numbers in observed stock object
        landings.n(stk0) <- stk0landings
        discards.n(stk0) <- stk0discards
        stk0@catch.n      <- stk0@landings.n + stk0@discards.n

      }

      ## update total landings, discards and catch weights
      landings(stk0)  <- computeLandings(stk0)
      discards(stk0)  <- computeDiscards(stk0)
      catch(stk0)     <- computeCatch(stk0)

      # Update overall mean weights based on catch-numbers-weighted mean
      # ----------------------------------------------------------------#
      # catch.wt(stk0)
      # landings.wt(stk0)
      # discards.wt(stk0)

      # Update tracking object
      # -------------------------#
      tracking[[x]]$stk["C.obs", ac(iy:ay)] <- catch(stk0)[,ac(iy:ay)]
      tracking[[x]]$stk["L.obs", ac(iy:ay)] <- landings(stk0)[,ac(iy:ay)]
      tracking[[x]]$stk["D.obs", ac(iy:ay)] <- discards(stk0)[,ac(iy:ay)]
    }

    # ---------------------------------------------------------------#
    # (OPTION II) Use oem observation structure (FLBiol & FLFishery) #
    # ---------------------------------------------------------------#

    if(use_stk_oem[x] == TRUE & !is.null(observations$flt)) {

      stop("fleet observations not currently implemented")

      ## use observations object
      stk0 <- observations$stk[[x]]
      flt0 <- observations$flt

    }

    # ----------------------------------------#
    # (OPTION III) Use OM structure (FLStock) #
    # ----------------------------------------#

    # For simplicity, if an observation error model structure is not supplied,
    # then I will return an FLStock for each stock supplied - this should be
    # valid for 99% of users

    if(use_stk_oem[x] == FALSE) {
      stop("Automatic definition of stock structure not yet implemented")

      if(class(om$stks[[x]]) == "FLStock") {

        stk0 <- om$stks[[x]]
        flt0 <- om$flts

      } else if(class(om$stks[[x]]) == "FLBiol") {

        # I DON'T WANT TO COERCE TO FLSTOCK... I SHOULD USE FLBIOLS AND FLFISHERIES
        # INSTEAD

        ## coerce to FLStock
        stk0 <- as(om$stks[[x]],"FLStock")

        ## populate missing slots with data from stock object in tracking
        units(stk0) <- units(tracking[[x]][["stk"]])

        ## Fill missing slots
        stk0@landings.n  <- tracking[[x]][["stk"]]@landings.n
        stk0@landings.wt <- tracking[[x]][["stk"]]@landings.wt
        stk0@discards.n  <- tracking[[x]][["stk"]]@discards.n
        stk0@discards.wt <- tracking[[x]][["stk"]]@discards.wt
        stk0@catch.n  <- tracking[[x]][["stk"]]@catch.n
        stk0@catch.wt <- tracking[[x]][["stk"]]@catch.wt
        stk0@harvest  <- tracking[[x]][["stk"]]@harvest

        ## update Fbar range
        range(stk0)["minfbar"] <- range(tracking[[x]][["stk"]])["minfbar"]
        range(stk0)["maxfbar"] <- range(tracking[[x]][["stk"]])["maxfbar"]

        ## compute properties
        stock(stk0)    <- computeStock(stk0)
        landings(stk0) <- computeLandings(stk0)
        discards(stk0) <- computeDiscards(stk0)
        catch(stk0)    <- computeCatch(stk0)

        ## truncate to min data year
        minyr <- dims(stk0@stock[!is.na(stk0@stock)])$minyear # min year where data exists
        stk0  <- window(stk0, start = minyr)

        ## Generate SR of correct dimensions
        sr0        <- as.FLSR(stk0, model = om1$stks[[x]]@rec@model)
        sr0@rec    <- rec(stk0)
        sr0@ssb    <- ssb(stk0)
        sr0@params <- om1$stks[[x]]@rec@params
      }
    }

    # --------------------#
    # Trim to data period #
    # --------------------#

    ## If survey data is more recent than catch data, then trim to survey year
    if(max(idx_timing[[x]]) > max(catch_timing[[x]])) {

      ## Trim stock object
      stk0 <- window(stk0, end = ay + max(idx_timing[[x]]))

      ## Remove data in years where no catch data is available
      yrs_remove <- (ay + catch_timing[[x]] + 1):ay

      catch(stk0)[, ac(yrs_remove)]       <- NA
      catch.n(stk0)[, ac(yrs_remove)]     <- NA
      catch.wt(stk0)[, ac(yrs_remove)]    <- NA
      landings(stk0)[, ac(yrs_remove)]    <- NA
      landings.n(stk0)[, ac(yrs_remove)]  <- NA
      landings.wt(stk0)[, ac(yrs_remove)] <- NA
      discards(stk0)[, ac(yrs_remove)]    <- NA
      discards.n(stk0)[, ac(yrs_remove)]  <- NA
      discards.wt(stk0)[, ac(yrs_remove)] <- NA

    } else {

      stk0 <- window(stk0, end = ay + max(catch_timing[[x]]))

    }

    # ------------------------#
    # Observed survey indices #
    # ------------------------#

    ## use observed survey indices
    idx0 <- observations$idx

    ## should index uncertainty be added?
    if(use_idx_residuals[x] == TRUE) {

      idx0 <- lapply(seq_along(idx0), function(idx_i) {
        idx_tmp <- idx0[[idx_i]]
        index(idx_tmp) <- index(idx_tmp) * deviances$idx[[idx_i]]
        return(idx_tmp)
      })
    }

    return(list(stk = stk0,
                idx = idx0,
                tracking = tracking[[x]]$stk))
  })

  ## Add names to list
  names(oemList) <- observations$stk@names

  ## Extract list elements
  for(x in observations$stk@names) {
    tracking[[x]]$stk <- oemList[[x]]$tracking
  }
  stk <- FLStocks(lapply(oemList, "[[", "stk"))
  idx <- lapply(oemList, "[[", "idx")


  ### return observations
  return(list(stk          = stk,
              idx          = idx,
              observations = observations,
              tracking     = tracking))
}

