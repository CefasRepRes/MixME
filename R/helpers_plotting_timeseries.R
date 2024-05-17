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
#' @param object The output from a `MixME` simulation.
#' @param alt    (Optional) The output from an alternative `MixME` simulation to 
#'               be compared
#' @param quantity `Character`. The operating model property to be visualised.
#'                 Options are "ssb", "effort", "catch", "uptake","fbar", and "f". 
#' @param minyr (Optional) `numeric`. The minimum year to be plotted.
#' @param maxyr (Optional) `numeric`. The maximum year to be plotted.
#' @param stknames (Optional) `character` vector. The names of stocks to be included
#'                 in the plot. Applies to "ssb", "catch", "uptake", "fbar" and "f".
#' @param fltnames (Optional) `character` vector. The names of fleets to be included
#'                 in the plot. Applies to "effort" only. 
#' @param trajectories (Optional)`numeric`. The number of randomly selected trajectories
#'                     to be plotted. Defaults to \code{NULL}.
#' @param quantiles `Numeric` vector. The summary quantiles to be calculated for 
#'                  the operating model quantity. The argument must be a vector of 
#'                  either 2 or 4 values between 0 and 1. 
#'                  Defaults to 5%, 25%, 75% and 95% quantiles.
#' @param addRefpts `Logical`. Should biological and fishing mortality reference points
#'                  be included in the plot? Defaults to \code{TRUE}
#' @param keepFailedIters `Logical`. Should trajectories, also known as iterations or
#'                        replicates, containing instances of failed advice generation
#'                        be retained in the plot. Defaults to \code{TRUE}.
#'                        
#' @returns A `ggplot` object
#' 
#' @export
#' @examples
#' \donttest{
#' ## load example data
#' data("mixedfishery_MixME_input")
#'
#' ## run MixME simulation
#' res <- runMixME(om  = mixedfishery_MixME_input$om, 
#'                 oem = mixedfishery_MixME_input$oem,
#'                 ctrl_obj = mixedfishery_MixME_input$ctrl_obj,
#'                 args     = mixedfishery_MixME_input$args)
#' 
#' ## plot individual simulation trajectories
#' plot_timeseries_MixME(res, quantity = "ssb")
#' plot_timeseries_MixME(res, quantity = "fbar")
#' plot_timeseries_MixME(res, quantity = "catch")
#' plot_timeseries_MixME(res, quantity = "uptake")
#' }

setGeneric("plot_timeseries_MixME", function(object,
                                             alt,
                                             quantity,
                                             minyr = NULL,
                                             maxyr = NULL,
                                             stknames = NULL,
                                             fltnames = NULL,
                                             trajectories = NULL,
                                             quantiles = c(0.05, 0.25, 0.75, 0.95),
                                             addRefpts = TRUE,
                                             keepFailedIters = TRUE,
                                             ...) {
  
  standardGeneric("plot_timeseries_MixME")
})

## If object == "list" ; alt == NULL
#' @rdname plot_timeseries_MixME
setMethod(f = "plot_timeseries_MixME",
          signature = signature(object = "list", alt = "missing"),
          definition = function(object,
                                alt,
                                quantity,
                                minyr = NULL,
                                maxyr = NULL,
                                stknames = NULL,
                                fltnames = NULL,
                                trajectories = NULL,
                                quantiles = c(0.05, 0.25, 0.75, 0.95),
                                addRefpts = TRUE,
                                keepFailedIters = TRUE,
                                ...) {
            
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
              maxyr    <- object$args$fy
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
                                    iy = iy, 
                                    ...)
            }
            if(quantity == "effort"){
              out <- plot_effort_MixME(res = res, trajectories = trajectories, 
                                       quantiles = quantiles,
                                       iy = iy, 
                                       ...)
            }
            if(quantity == "catch") {
              out <- plot_catch_MixME(res = res, trajectories = trajectories, 
                                      quantiles = quantiles,
                                      iy = iy, 
                                      ...)
            }
            if(quantity == "uptake") {
              out <- plot_uptake_MixME(res = res, trajectories = trajectories, 
                                       quantiles = quantiles,
                                       iy = iy, 
                                       ...)
            }
            if(quantity == "fbar"){
              out <- plot_fbar_MixME(res = res, trajectories = trajectories,
                                     quantiles = quantiles,
                                     Refpts = Refpts,
                                     iy = iy, 
                                     ...)
            }
            if(quantity == "f") {
              
            }
            if(quantity == "risk") {
              out <- plot_risk_MixME(res = res, iy = iy, 
                                     ...)
            }
            
            return(out)
            
          })

## If object == "list" ; alt == "list"
#' @rdname plot_timeseries_MixME
setMethod(f = "plot_timeseries_MixME",
          signature = signature(object = "list", alt = "list"),
          definition = function(object,
                                alt,
                                quantity,
                                minyr = NULL,
                                maxyr = NULL,
                                stknames = NULL,
                                fltnames = NULL,
                                trajectories = NULL,
                                quantiles = c(0.05, 0.25, 0.75, 0.95),
                                addRefpts = TRUE,
                                keepFailedIters = TRUE,
                                ...) {
            
            out1 <- plot_timeseries_MixME(object       = object,
                                          quantity     = quantity,
                                          minyr        = minyr,
                                          maxyr        = maxyr,
                                          stknames     = stknames,
                                          fltnames     = fltnames,
                                          trajectories = trajectories,
                                          quantiles    = quantiles,
                                          addRefpts    = addRefpts,
                                          keepFailedIters = keepFailedIters)
            
            out2 <- plot_timeseries_MixME(object       = alt,
                                          quantity     = quantity,
                                          minyr        = minyr,
                                          maxyr        = maxyr,
                                          stknames     = stknames,
                                          fltnames     = fltnames,
                                          trajectories = trajectories,
                                          quantiles    = quantiles,
                                          addRefpts    = addRefpts,
                                          keepFailedIters = keepFailedIters, 
                                          add = out1,
                                          fill   = "red4",
                                          colour = "red4")
            return(out2)
          })
