# ---
# title: 'Functions to implement advice using SAM forecast'
# author: 'Various'
# date: 'December 2022'
# ---
# 
# Adapted from code by S.H. Fischer.
#
#' Advice implementation using the SAM state-space assessment model
#' 
#' Carry out a short-term forecast using the SAM state-space assessment model
#' to transform advised fishing mortality into an advised catch target.
#' 
#' @param stk Object of class \code{FLStock} containing observed stock 
#'            information including commercial catch data, individual mean 
#'            weights and biological parameters.
#' @param tracking Tracking object
#' @param args Additional arguments
#' @param forecast Logical. Should a short-term forecast be carried out? Defaults
#'                 to \code{TRUE}
#' @param fwd_trgt Character. Catch or effort target to use during forecast. 
#'                 Defaults to 'fsq'.
#' @param fwd_yrs Integer. The number of years to forecast. Defaults to 1.
#' @param fwd_yrs_average Integer vector. The historical data years over which
#'                        biological parameter are averaged for use during 
#'                        forecast. Defaults to -3:-1.
#' @param fwd_yrs_rec_start Integer. Starting historical year from which to sample
#'                          projection period recruitment during forecast.
#' @param fwd_yrs_sel Integer vector. The historical data years over which
#'                    catch selection-at-age is averaged for use during forecast.
#'                    Defaults to -3:-1.
#' @param fwd_yrs_lf_remove Integer Vector. ... Defaults to -2:-1.
#' @param fwd_splitLD Logical. Defaults to \code{TRUE}
#' 
#' @return 
#' 
#' @export

SAMimplementation <- function(stk, tracking, ctrl,
                              args,                       # contains ay (assessment year)
                              fwd_trgt = c("fsq", "hcr"), # target in forecast
                              fwd_yrs = 2,                # number of years to add
                              fwd_yrs_average = -3:0,     # years used for averages
                              fwd_yrs_rec_start = NULL,   # recruitment 
                              fwd_yrs_sel = -3:-1,        # selectivity
                              fwd_yrs_lf_remove = -2:-1,
                              fwd_splitLD = TRUE) {
  
  ## get current (assessment) year
  ay <- args$ay
  
  ## number of iterations
  niter <- dim(stk)[6]
  
  ## retrieve SAM model fit (list)
  fit <- attr(stk, "fit")
  
  ## check class of model fit(s)
  if (!class(fit) %in% c("sam", "sam_list")) 
    stop("attr(stk0, \"fit\") has to be class sam or sam_list")
  
  ## if single fit, turn into list
  if (is(fit, "sam")) fit <- list(fit)
  
  ## get recent TAC
  if (args$iy == ay) {
    
    ## in first year of simulation, use value from OM saved earlier in ay
    TAC_last <- tracking["C.om", ac(ay)]
  } else {
    
    ## in following years, use TAC advised the year before
    TAC_last <- tracking["metric.is", ac(ay - 1)]
  }
  
  ## go through all model fits
  fc <- foreach(fit_i = fit, iter_i = seq_along(fit), 
                .errorhandling = "pass") %do% {
                  
                  ## overwrite landing fraction with last year, if requested
                  if (!is.null(fwd_yrs_lf_remove)) {
                    
                    ## index for years to remove/overwrite
                    idx_remove <- nrow(fit_i$data$landFrac) + fwd_yrs_lf_remove
                    
                    ## overwrite
                    fit_i$data$landFrac[idx_remove, ] <- 
                      fit_i$data$landFrac[rep(nrow(fit_i$data$landFrac), length(idx_remove)), ]
                  }
                  
                  ## check how to do forecast
                  ## can handle F status quo, F target from ctrl object and TAC
                  
                  ## scaled F
                  fscale <- ifelse(fwd_trgt == "fsq", 1, NA)
                  
                  ## target F values
                  fval <- ifelse(fwd_trgt == "hcr", ctrl@trgtArray[, "val", iter_i], NA)
                  
                  ## target catch values
                  catchval <- ifelse(fwd_trgt == "TAC", c(TAC_last[,,,,, iter_i]), NA)
                  
                  ## years for average values
                  ave.years <- max(fit_i$data$years) + fwd_yrs_average
                  
                  ## years for sampling of recruitment years
                  if (is.null(fwd_yrs_rec_start)) {
                    rec.years <- fit_i$data$years ### use all years, if not defined
                  } else {
                    rec.years <- seq(from = fwd_yrs_rec_start, max(fit_i$data$years))
                  }
                  
                  ## years where selectivity is not used for mean in forecast
                  overwriteSelYears <- max(fit_i$data$years) + fwd_yrs_sel
                  
                  ## arguments for forecast
                  fc_args <- list(fit = fit_i, fscale = fscale, fval = fval, 
                                  catchval = catchval,
                                  ave.years = ave.years, rec.years = rec.years,
                                  overwriteSelYears = overwriteSelYears, 
                                  splitLD = fwd_splitLD)
                  
                  ## for compatibility of stockassessment's commit a882a11 and later:
                  if ("savesim" %in% names(formals(base::args(stockassessment::forecast)))) {
                    fc_args$savesim <- TRUE
                  }
                  
                  ## run forecast
                  fc_i <- do.call(stockassessment::forecast, fc_args)
                  
                  ## return forecast table
                  return(attr(fc_i, "tab"))
                  
                }
  
  ## if forecast fails, error message returned
  ## replace error message with NA
  ## otherwise, extract catch target
  catch_target <- sapply(fc, function(x) {
    if (is(x, "error")) {
      return(NA)
    } else {
      return(x[ac(ay + 1), "catch:median"])
    }
  })
  
  ## create ctrl object
  ctrl <- getCtrl(values = catch_target, 
                  quantity = "catch", 
                  years = ctrl@target$year, it = niter)
  
  ## return catch target and tracking
  return(list(ctrl = ctrl, tracking = tracking))
  
}

