# ---
# title: 'Operating Model - Observation error model'
# author: 'Matthew Pace'
# date: 'September 2022'
# ---
#
#' Implementation of stock observation methods
#'
#' Function ...
#'
#' Note that this function is intended to be looped over, and therefore is run
#' separately for each stock.
#'
#' @param stk
#' @param deviances
#' @param observations
#'
#' @return
#'
#' @export

oemMIME <- function(om,
                    deviances,
                    observations,
                    args,
                    tracking,
                    use_stk_oem         = FALSE,
                    use_catch_residuals = FALSE,
                    use_idx_residuals   = FALSE) {

  # ===================================#
  # SECTION 1: Exract global arguments
  # ===================================#

  ## extract stock name


  ## extract timings
  ay   <- args$ay             # current (assessment) year

  # =======================================#
  # SECTION 2: Iterative stock observation
  # =======================================#

  # -----------------------#
  # Observed stock catches #
  # -----------------------#

  ## Should an oem observation stock structure be used?
  if(use_stk_oem == TRUE) {

    ## use observations object
    stk0 <- observations$stk
    # flt0 <- observations$flt

    ## update fisheries catch, discards and landings information
    catch.n(stk0) <- catch.n(stk)
    catch(stk0)   <- catch(stk)

  } else {

    stk0 <- stk
    # flt0 <- flt
  }

  ## Should catch uncertainty be added?
  if(use_catch_residuals == TRUE) {

    ## implement catch deviances
    catch.n(stk0) <- catch.n(stk) * deviances$stk$catch.dev
  }

  ## split catch into discards and landings, based on landings fraction
  landings.n(stk0) <- catch.n(stk0) * (landings.n(stk) / catch.n(stk))
  discards.n(stk0) <- catch.n(stk0) * (1 - landings.n(stk) / catch.n(stk))

  # ------------------------#
  # Observed survey indices #
  # ------------------------#

  ## use observed survey indices
  idx0 <- observations$idx

  ## should index uncertainty be added?
  if(use_idx_residuals == TRUE) {

    idx0 <- lapply(seq_along(idx0), function(idx_i) {
      idx_tmp <- idx0[[idx_i]]
      index(idx_tmp) <- index(idx_tmp) * deviances$idx[[idx_i]]
      return(idx_tmp)
    })
  }

  ### return observations
  return(list(stk = stk0,
              idx = idx0,
              observations = observations,
              tracking     = tracking))
}


######################

stk <- lapply(om1$stks@names, function(x){

  ## coerce to FLStock
  stk0 <- as(om1$stks[[x]],"FLStock")

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

  return(stk0)
})
