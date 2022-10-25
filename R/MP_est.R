# ---
# title: 'Management Procedure - Stock estimation methods'
# author: 'Matthew Pace'
# date: 'August 2022'
# ---
#
#' Implementation of stock estimation methods
#'
#' Function takes a time-series of observed catch or index data
#' (\code{FLStock} and \code{FLIndex} respectively) and estimates current stock
#' status using user-supplied methods. Optionally, if a lag exists between the
#' assessment and management years, a short-term forecast may be carried out to
#' estimate stock status during the management year.
#'
#' The *fwdList* argument is a list that should contain the following elements:
#' \itemize{
#' \item *forecast*: Logical. Should a forecast be carried out?
#' \item *fwd_trgt*: Character. The short-term forecast target type. Either
#'                   'fsq' or 'TAC'.
#' \item *fwd_yrs*: Integer. The number of years to forecast
#' \item *fwd_yrs_average*: Integer vector. Index of historic years over which to
#'                          average biological data
#' \item *fwd_yrs_rec_start*: Integer vector. Index of historic years from which
#'                            sample recruitment.
#' \item *fwd_yrs_sel*: Integer vector. Index of historic years over which to average
#'                      selectivity-at-age
#' \item *fwd_yrs_lf_remove*: Integer vector. Index of historic years for which
#'                            landings fraction data are overwritten by the landings
#'                            fraction data in the most recent year.
#' \item *fwd_splitLD*: Separate landings and discards numbers in forecast outputs.
#' }
#'
#' @param stk a list of stocks, each of class \code{FLBiol} or \code{FLStock},
#'            or a single stock to be assessed
#' @param idx a list of indices ...
#' @param ctrl a forward control object of class \code{...}
#'             for each stock.
#' @param args list of additional arguments
#' @param estMethod named list of estimation methods for each stock.
#'                  List elements may be the name of an in-built estimation
#'                  method or a user-supplied function that is applied to the
#'                  stock. If \code{NULL}, defaults to 'perfectObs'.
#' @param tracking a named list of tracking objects to monitor emergent dynamic
#'                 properties. Must contain an \code{FLStock} object named 'stk'
#'                 containing landings, discards, catch and harvest information.
#' @param fitList (optional) a named list of length *n* stocks. Each list element is a sub-list
#'                containing the arguments needed to estimate stock status.
#'                Defaults to \code{NULL}.
#' @param fwdList (optional) a named list of length *n* stocks. Each list element is a sub-list
#'                containing the arguments needed to carry out a short-term
#'                forecast for the stock. See 'Details' for required list elements.
#'                Defaults to \code{NULL}.
#'
#' @return
#'
#' @export

estMixME <- function(stk,
                    idx,
                    ctrl = NULL, ## DO I REALLY NEED THIS OR CAN I GENERATE IN FUNCTION?
                    om   = NULL,
                    args,
                    estmethod = NULL,
                    tracking,
                    fitList = NULL,
                    fwdList = NULL) {

  # ===================================#
  # SECTION 1: Extract global arguments
  # ===================================#

  ## extract timings
  iy   <- args$iy             # initial projection year
  ay   <- args$ay             # current (assessment) year
  mlag <- args$management_lag # managment lag

  ## If no methods supplied, then assume perfect observation
  if(is.null(estmethod)) {

    estmethod <- sapply(stk@names, function(x) "perfectObs",
           USE.NAMES = TRUE, simplify = FALSE)
  }

  # ======================================#
  # SECTION 2: Iterative stock estimation
  # ======================================#

  ## If a list of stocks is given, loop over each stock
  if(is.list(stk)) {

    est_list <- lapply(stk@names, function(x){

      # ---------------------------------------------------------#
      # (Option 1) Apply user-supplied stock estimation method
      # ---------------------------------------------------------#

      ## If available, apply user-supplied estimation method
      if(is.function(estmethod[[x]])) {

        stk_est <- do.call(estmethod[[x]],
                           list(stk = stk[[x]],
                                idx = idx[[x]]))

        ## DO I WANT TO EXTRACT SOME GENERIC RECRUITMENT MODEL???
      }

      # ---------------------------------------------------------#
      # (Option 2) Apply perfect stock observation
      # ---------------------------------------------------------#

      ## Alternatively, simply populate OEM stock numbers and recruitment from OM
      if(estmethod[[x]] == "perfectObs") {

        ## Copy observed stock object
        stk0 <- stk[[x]]

        ## Extract data year vector
        yrs_oem <- (range(stk0)["minyear"]):(range(stk0)["maxyear"])

        ## If FLBiols
        if(class(stk[[x]]) == "FLBiol") {

          stop("Stock estimation using FLBiol class no yet supported!")

          ## insert stock numbers
          n(stk0)[,ac(yrs_oem)] <- n(om$stks[[x]])[,ac(yrs_oem)]

          ## insert stock recruitment information
          sr0 <- NULL

        }

        ## If FLStock
        if(class(stk[[x]]) == "FLStock") {

          ## insert stock numbers and calculate stock biomass
          stock.n(stk0)[,ac(yrs_oem)] <- n(om$stks[[x]])[,ac(yrs_oem)]
          stock(stk0)                 <- computeStock(stk0)

          ## insert fishing mortality
          fltFage <- sapply(om$flts@names,
                            function(y){
                              Fage <- attr(om$flts[[y]][[x]],"catchq")[,ac(yrs_oem)] %*%
                                om$flts[[y]]@effort[,ac(yrs_oem)] %*%
                                om$flts[[y]][[x]]@catch.sel[,ac(yrs_oem)]

                              Fage#[drop = TRUE]
                            }, simplify = "array")

          harvest(stk0)[,ac(yrs_oem)] <- apply(fltFage, c(1:6), sum)

          # Under perfect stock observations and zero management lag, the final
          # (current) year is also the advice year and no fishing has occurred yet. The
          # harvest data in the stock object for this year is the basis for selectivity
          # understood by the OM.

          # If projected values are means of the historical period, there will be
          # slight differences in the resulting values compared to the FLStock
          # generated when conditioning the OM. This is because we are calculated
          # using projected values at the fleet level, whereas the FLStock is simply
          # a mean of historic F.

          # This whole process is not needed if we intend to extend the stock
          # for a short-term forecast.

          ## Update intermediate year harvest selectivity
          if (mlag == 0) {
            if(ay == iy) {
              harvest(stk0)[,ac(ay)] <- yearMeans(harvest(stk0)[,ac((ay-3):(ay-1))])
            }
            if(ay > iy) {

              harvest(stk0)[,ac(ay)] <- harvest(stk0)[,ac(ay-1)]
              # harvest(stk0)[,ac(ay)] <- sweep(harvest(stk0)[,ac(ay-1)], c(2:6), fbar(stk0)[,ac(ay-1)], "/")

            }
          }

          ## extract stock recruitment information
          sr0        <- as.FLSR(stk0, model = om$stks[[x]]@rec@model)
          sr0@rec    <- rec(stk0)
          sr0@ssb    <- ssb(stk0)
          sr0@params <- om$stks[[x]]@rec@params

          # Update tracking object
          # -------------------------#
          tracking[[x]]$stk["F.est", ac(iy:ay)]  <- fbar(stk0)[,ac(iy:ay)]
          tracking[[x]]$stk["B.est", ac(iy:ay)]  <- stock(stk0)[,ac(iy:ay)]
          tracking[[x]]$stk["SB.est", ac(iy:ay)] <- ssb(stk0)[,ac(iy:ay)]

          tracking[[x]]$stk["C.est", ac(iy:ay)] <- catch(stk0)[,ac(iy:ay)]
          tracking[[x]]$stk["L.est", ac(iy:ay)] <- landings(stk0)[,ac(iy:ay)]
          tracking[[x]]$stk["D.est", ac(iy:ay)] <- discards(stk0)[,ac(iy:ay)]

          tracking[[x]]$sel_est[,ac(ay)] <- sweep(harvest(stk0)[,ac(ay)], c(2:6), fbar(stk0)[,ac(ay)], "/")

        }

        # ---------------------------------------------------------------------#
        # (Option 2.1) Apply short term forecast on perfect stock observation
        # ---------------------------------------------------------------------#

        # if(mlag > 0 & Forecast == FALSE)
        #   warning("Management lag > 0 but no short-term forecast defines. Errors may occur.")

        ## Combine outputs into list
        stk_est <- list(stk = stk0,
                        sr  = sr0,
                        tracking = tracking[[x]])

      }

      return(stk_est)
    })

    ## Add names to list of estimated stocks
    names(est_list) <- stk@names

    ## Update tracking object
    for(x in stk@names) {
      tracking[[x]] <- est_list[[x]]$tracking
    }

    stk0 <- FLStocks(lapply(est_list, "[[", "stk"))
    sr0  <- lapply(est_list, "[[", "sr")
  }

  return(list(stk      = stk0,
              sr       = sr0,
              tracking = tracking))
}

#' Short-term forecast to estimate stock in advice year
#'
#' Function runs a short-term forecast for a single stock to estimate stock
#' properties in the advice year.
#'
#' @param stk an object of class \code{FLBiol} or \code{FLStock}
#' @param ctrl a forward control object of class \code{...}
#'             for each stock.
#' @param tracking a named list of tracking objects to monitor emergent dynamic
#'                 properties. Must contain an \code{FLStock} object named 'stk'
#'                 containing landings, discards, catch and harvest information.
#' @param args list of additional arguments

estForecast <- function(stk,
                        ctrl,
                        args,
                        forecast,
                        fwd_trgt,
                        fwd_yrs,
                        fwd_yrs_average,
                        fwd_yrs_rec_start,
                        fwd_yrs_sel,
                        fwd_yrs_lf_remove,
                        fwd_splitLD) {

  ## extract timings
  ay   <- args$ay             # current (assessment) year
  mlag <- args$management_lag # lag between assessment year and advice year



  return(list())
}
