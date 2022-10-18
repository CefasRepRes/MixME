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

estMIME <- function(stk,
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
  ay   <- args$ay             # current (assessment) year

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

        ## If FLBiols
        if(class(stk[[x]]) == "FLBiol") {

          stop("Stock estimation using FLBiol class no yet supported!")

          ## Copy observed stock object
          stk0 <- stk[[x]]

          ## insert stock numbers
          n(stk0[[x]]) <- n(om$stks[[x]])

          ## insert stock recruitment information
          sr0 <- NULL


          ## Combine outputs into list
          stk_est <- list(stk = stk0,
                          sr  = sr0)
        }

        ## If FLStock
        if(class(stk[[x]]) == "FLStock") {

          ## Copy observed stock object
          stk0 <- stk[[x]]

          ## insert stock numbers
          stock.n(stk0) <- n(om$stks[[x]])

          ## extract stock recruitment information
          sr0        <- as.FLSR(stk0, model = om$stks[[x]]@rec@model)
          sr0@rec    <- rec(stk0)
          sr0@ssb    <- ssb(stk0)
          sr0@params <- om$stks[[x]]@rec@params

          ## Combine outputs into list
          stk_est <- list(stk = stk0,
                          sr  = sr0)
        }


      }

      return(stk_est)
    })

    names(est_list) <- stk@names

    # if()
    #
    #   } else if() {
    #
    #     stk_fwd <- estMethod(stk, ctrl, args)
    #
    #     if()
  }

  return(list(stkList  = est_list,
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
