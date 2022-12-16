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