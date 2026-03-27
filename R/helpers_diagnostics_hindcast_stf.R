# ---
# title: 'Functions to carry out reconditioning of historical years'
# author: Matthew Pace
# date: 'March 2026'
# ---
#
#' Function to recondition a MixME operating Model for hindcasting
#' 
#' This function reconditions the \code{FLBiols} and \code{FLFisheries} comprising
#' the operating model in preparation of a hindcast. Objects are extended by a
#' single year and the data slots for biological and fishery parameters are
#' populated using estimates from recent data years.
#' 
#' \code{stf_hindcast} contains two methods to estimate future biological and fishery parameters.
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
#' @param om A list of \code{FLBiols} and \code{FLFisheries} with year 'y' removed.
#' @param om_ref A list of \code{FLBiols} and \code{FLFisheries} with year 'y' present
#' @param basis Character vector. The fleet parameters to be reconditioned.
#' @param fleets Character vector. The fleets that should be reconditioned.
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

hindcast_stf <- function(om, 
                         om_ref,
                         basis,
                         fleets,
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
                        stocks_ref = om_ref$stks,
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
                           fisheries_ref = om_ref$flts,
                           method,
                           basis = basis,
                           fleets = fleets,
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
  
  return(om)
}