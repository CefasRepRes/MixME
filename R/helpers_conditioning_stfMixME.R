# ---
# title: 'Functions to extend year dimensions'
# author: 'Matthew Pace'
# date: 'December 2022 (updated March 2026)'
# ---
#
#' Function to extend a MixME Operating Model to future years for forecasting
#' 
#' This function extends the \code{FLBiols} and \code{FLFisheries} comprising
#' the operating model by a user-specified number of years and populates the
#' data slots for biological and fishery parameters using estimates from recent
#' data years.
#' 
#' \code{stfMixME} contains two methods to estimate future biological and fishery parameters.
#' 
#' - *'yearMeans'*: values in each projection year are the mean value from a reference year range.
#' - *'resample'*: values in each projection year are resampled with replacement from a reference year range in the same iteration.
#' 
#' In both cases, operations are performed on the same strata of 'quant', 'unit',
#' 'season', 'area' and 'iteration'. This means that *'yearMeans'* will
#' reflect any variation in these dimensions and *'resample'* will yield
#' a consistent set of values over the 'quant' dimension for any combination of 
#' 'unit', 'season', 'area' and 'iteration'.
#' 
#' @param om A list of \code{FLBiols} and \code{FLFisheries} containing the relevant stock and fleet information in the Operating Model.
#' @param method Character. The method to estimate future biological and fishery parameters. See details.
#' @param nyears Integer. The number of projection years to extend each fleet and stock slot.
#' @param wts.nyears Integer. The number of recent data years from which future 
#'                   biological (stock weights \code{wt}, natural mortality \code{m}, 
#'                   maturity \code{mat}, fecundity \code{fec}), fishery
#'                   (landings weight \code{landings.wt}, discards weight \code{discards.wt},
#'                   landed fraction \code{landings.n}, discarded fraction \code{discards.n})
#'                   parameters are estimated.
#' @param sel.nyears Integer. The number of recent data years from which future fishery
#'                   (catch selectivity \code{catch.sel}, catchability \code{catch.q}, 
#'                   \code{capacity}, \code{effort}) and survey (survey catchability \code{index.q})
#'                   parameters are estimated.
#' @param qs.nyears Integer. The number of recent data years from which future 
#'                  quota-shares (\code{quotashare}) are estimated.
#' @param verbose Logical. Should progress be reported? Defaults to \code{TRUE}.
#' @param seed (Optional) Integer. Random seed for reproducibility if using RNG.
#' 
#' @returns A list containing the \code{FLBiols} and \code{FLFisheries} objects with projection
#'          years and estimated biological and fishery parameters. 
#' 
#' @examples
#' ## load data
#' data("singlestock_MixME_om")
#' 
#' ## check year dimensions for stock and fleet
#' dimnames(singlestock_MixME_om$stks$had)$year
#' dimnames(singlestock_MixME_om$flts$fleet)$year
#' 
#' ## prepare operating model for a 3-year forecast 
#' om1 <- stfMixME(singlestock_MixME_om, method = "yearMeans", 
#'                 nyears = 3,
#'                 wts.nyears = 3,
#'                 sel.nyears = 5,
#'                 qs.nyears = 5,
#'                 verbose = FALSE)
#' 
#' ## historical weights in black. projected weights in red
#' matplot(t(om1$stks$had@wt[drop=TRUE]), lty=1,col="red",type="l",ylab="weight-at-age (kg)")
#' matplot(t(singlestock_MixME_om$stks$had@wt[drop=TRUE]), lty=1,col="black",type="l",add=TRUE)
#' 
#' @export

stfMixME <- function(om, 
                     method     = "yearMeans",
                     nyears     = 3, 
                     wts.nyears = 3,
                     sel.nyears = 3,
                     qs.nyears  = 3,
                     verbose = TRUE,
                     seed    = NULL) {
  
  # ----------------------------------------------------------------------------
  # Prepare arguments
  # ----------------------------------------------------------------------------
  
  args0 <- stf_args(om,
                    method,
                    nyears,
                    wts.nyears,
                    sel.nyears,
                    qs.nyears,
                    seed)
  
  ydata       = args0$ydata
  year_proj   = args0$year_proj
  wts_year    = args0$year_wts
  sel_year    = args0$year_sel
  qs_year     = args0$year_qs
  wts_samples = args0$wts_samples
  sel_samples = args0$sel_samples
  qs_samples  = args0$qs_samples
  ni          = args0$ni
  
  # ----------------------------------------------------------------------------
  # Process FLStocks/FLBiols
  # ----------------------------------------------------------------------------
  
  om$stks <- stf_stocks(om$stks,
                        stocks_ref = NULL,
                        method,
                        nyears,
                        wts.nyears,
                        wts_samples,
                        wts_year,
                        year_proj,
                        ni,
                        verbose)
  
  # ----------------------------------------------------------------------------
  # Process FLFisheries
  # ----------------------------------------------------------------------------
  
  om$flts <- stf_fisheries(om$flts,
                           fisheries_ref = NULL,
                           method,
                           basis = NULL,
                           fleets = NULL,
                           nyears,
                           ydata,
                           wts_samples,
                           wts_year,
                           sel_samples,
                           sel_year,
                           qs_samples,
                           qs_year,
                           year_proj,
                           ni,
                           verbose)
  
  # ----------------------------------------------------------------------------
  # (OPTIONAL) Process FLIndices
  # ----------------------------------------------------------------------------
  #
  # The stf function within FLasher also does not work with FLIndex objects. We 
  # will need to extend these manually. I should use the same reference years as 
  # commercial catch selectivity.
  # 
  # slots to be extended are: index.q
  
  if (!is.null(om$idxs)) {
    for(x in 1:length(om$idxs)) {
      
      om$idxs[[x]] <- window(om$idxs[[x]], end = ydata+nyears)
      
      for(y in 1:length(om$idxs[[x]])) {
        om$idxs[[x]][[y]]@index.q[,year_proj,,,,] <- stfQuant(om$idxs[[x]][[y]]@index.q[,year_sel], 
                                                              method  = method, 
                                                              samples = sel_samples,
                                                              ni      = ni)
      }
    }
  }
  
  return(om)
}

#' Function to handle different methods of extending stock/fleet properties to future years
#' ----------------------------------------------------------------------------------------

stfQuant <- function(object, method, samples = NULL, ni = NULL, ignoreZero = FALSE, ignoreAllZero = FALSE) {
  
  if (method == "yearMeans") {
    
    # IgnoreZero handles cases where a zero denotes a missing observation for
    # a given age. We would want to ignore this zero when calculating an annual average.
    
    if (ignoreZero & !all(object==0,na.rm = TRUE)) {
      object[object==0] <- NA
    }
    
    # IgnoreAllZero handles cases where an individual zero is possible (and we
    # want to include this in our calculations), but we want to ignore cases where
    # all ages contain a zero (because this represents missing data)
    
    if (ignoreAllZero) {
      object <- apply(object,2:6,function(x) {
        if(sum(x) ==0) x[] <- NA
        return(x)
      })
    }
    res <- yearMeans(object)
    res[is.na(res)] <- 0
    return(res)
  }
  if (method == "resample") {
    vec <- lapply(1:ni, function(x) {
      
      xx <- ((x-1)* (length(samples)/ni)) + (1:(length(samples)/ni))
      return(c(object[, samples[xx], , , , x]))
    })
    return(unlist(vec))
  }
}