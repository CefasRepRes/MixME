# ---
# title: 'Helper functions to plot outputs'
# author: 'Matthew Pace'
# date: 'December 2022'
# ---
#
#' Plot time-series of Operating Model properties
#' 
#' Function takes an Operating model as input and plots a time-series of
#' a specified property for one or more stocks or fleets. Annual distributions 
#' are summarised to the median and up to two user-specified quantile intervals.
#' 
#' @export

plot_timeseries_MixME <- function(object,
                                  quantity,
                                  minyr = NULL,
                                  maxyr = NULL,
                                  stknames = NULL,
                                  fltnames = NULL,
                                  trajectories = NULL,
                                  quantiles = c(0.05, 0.25, 0.75, 0.95),
                                  addRefpts = TRUE,
                                  keepFailedIters = TRUE) {
  
  # =====================================#
  # extract elements and define arguments
  # =====================================#
  
  ## extract object elements
  om       <- object$om
  tracking <- object$tracking
  
  ## define quantiles if needed
  if(isTRUE(quantiles))
    quantiles <- c(0.05, 0.25, 0.75, 0.95)
  
  ## define min and max year if null
  SSBmaxyr <- maxyr
  if(is.null(maxyr)) {
    maxyr    <- object$args$fy - object$args$management_lag
    SSBmaxyr <- object$args$fy
  }
  
  if(addRefpts == TRUE & !is.null(object$ctrl_obj$phcr)){
    Refpts <- as.data.frame(do.call(rbind, object$ctrl_obj$phcr@args$hcrpars))
    Refpts$stk <- rownames(Refpts)
  } else {
    Refpts <- NULL
  }
  
  ## starting projection year
  iy <- object$args$iy
  
  # =====================================#
  # calculate requested quantity
  # =====================================#
  
  if(quantity == "ssb") {
    res <- summary_ssb_MixME(object = object, minyr = minyr, maxyr = SSBmaxyr,
                             stknames = stknames)
  }
  if(quantity == "effort"){
    res <- summary_effort_MixME(object = object, minyr = minyr, maxyr = maxyr,
                                fltnames = fltnames)
  }
  if(quantity == "catch") {
    res <- summary_catch_MixME(object = object, minyr = minyr, maxyr = maxyr,
                               stknames = stknames)
  }
  if(quantity == "uptake") {
    res <- summary_uptake_MixME(object = object, minyr = minyr, maxyr = maxyr,
                                stknames = stknames)
  }
  if(quantity == "fbar"){
    res <- summary_fbar_MixME(object = object, minyr = minyr, maxyr = maxyr,
                              stknames = stknames)
  }
  if(quantity == "f") {
    res <- summary_f_MixME(object = object, minyr = minyr, maxyr = maxyr,
                           fltnames = stknames)
  }
  if(quantity == "risk") {
    if(is.null(Refpts)) stop("No reference points are available")

    res <- summary_risk_MixME(object = object, minyr = minyr, maxyr = maxyr,
                              Refpts = Refpts,
                              stknames = stknames)
  }
  
  # =====================================#
  # (Optional) removed failed iterations
  # =====================================#
  
  if(!keepFailedIters) {
    res <- merge(res, aggregate(Freq ~ iter, data = as.data.frame.table(tracking$iterfail), FUN = sum))
    res <- res[res$Freq == 0,]
  }
  
  # =====================================#
  # Plot requested quantity
  # =====================================#
  
  if(quantity == "ssb") {
    out <- plot_ssb_MixME(res = res, trajectories = trajectories, 
                          quantiles = quantiles,
                          Refpts = Refpts,
                          iy = iy)
  }
  if(quantity == "effort"){
    out <- plot_effort_MixME(res = res, trajectories = trajectories, 
                             quantiles = quantiles,
                             iy = iy)
  }
  if(quantity == "catch") {
    out <- plot_catch_MixME(res = res, trajectories = trajectories, 
                            quantiles = quantiles,
                            iy = iy)
  }
  if(quantity == "uptake") {
    out <- plot_uptake_MixME(res = res, trajectories = trajectories, 
                             quantiles = quantiles,
                             iy = iy)
  }
  if(quantity == "fbar"){
    out <- plot_fbar_MixME(res = res, trajectories = trajectories,
                           quantiles = quantiles,
                           Refpts = Refpts,
                           iy = iy)
  }
  if(quantity == "f") {

  }
  if(quantity == "risk") {
    out <- plot_risk_MixME(res = res, iy = iy)
  }
  
  return(out)
}








