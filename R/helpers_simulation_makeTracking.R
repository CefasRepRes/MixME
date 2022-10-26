# ---
# title: 'Functions to help prepare objects needed for MSE simulation loop'
# author: 'Matthew Pace'
# date: 'October 2022'
# ---
#
#' Generate a tracker to store emergent simulation properties
#'
#' This function generates a tracking object to store diagnostics and emergent
#' properties generated during the MSE simulation.
#'
#' The tracking object is a named list with the following hierarchical structure:
#'
#' \itemize{
#' \item Named list for the i'th stock
#' \itemize{
#' \item Summary statistics (\code{stk})
#' \itemize{
#' \item Operating model mean fishing mortality (\code{F.om})
#' \item Operating model total stock biomass (\code{B.om})
#' \item Operating model spawning stock biomass (\code{SB.om})
#' \item Operating model total catch weight (\code{C.om})
#' \item Operating model total landings weight (\code{L.om})
#' \item Operating model total discards weight (\code{D.om})
#' \item Observed total catch weight (\code{C.obs})
#' \item Observed total landings weight (\code{L.obs})
#' \item Observed total discards weight (\code{D.obs})
#' \item Estimated mean fishing mortality (\code{F.est})
#' \item Estimated total stock biomass (\code{B.est})
#' \item Estimated spawning stock biomass (\code{SB.est})
#' \item Estimated total catch weight (\code{C.est})
#' \item Estimated total landings weight (\code{L.est})
#' \item Estimated total discards weight (\code{D.est})
#' \item (Optional) additional user-supplied metrics
#' }
#' \item Stock advice prior to implementation (\code{advice})
#' \item Operating model catch selection pattern aggregated across fleets (\code{sel_om})
#' \item Estimated catch selection pattern aggregated across fleets (\code{sel_est})
#' }
#' \item Stock quota allocated to each fleet (\code{quota})
#' \item Optimised objective function value and convergence code (\code{optim})
#' \item Optimisation message outputs (\code{message})
#' \item Stock quota uptake by fleet (\code{uptake})
#' \item Stock over-quota discards by fleet (\code{overquota})}
#'
#' @param om An operating model
#' @param projyrs Character vector. Names of projection years over which the simulation
#'                will take place.
#' @param addmetric Character vector. Names of additional stock metrics to be added
#'                  to the tracking object.
#'
#' @return List. An empty tracking object.
#'
#' @export

makeTracking <- function(om,
                         projyrs,
                         addmetrics = NULL) {

  ## Extract dimensions
  ns <- length(om$stks)
  nf <- length(om$flts)
  ny <- length(projyrs)
  ni <- dims(om$stks[[1]])$iter

  ## Generate tracker for optimisation and warnings
  tracking <- lapply(om1$stks@names, function(x){

    ## track stock advice
    track_advice_stk <- array(NA, dim = c(1,ny, ni),
                              dimnames = list(advice = 1,
                                              year = projyrs,
                                              iter = 1:ni))

    ## FLQuant to store summary statistics
    track_stk <- FLQuant(NA, dimnames=list(
      metric=c ("F.om", # OM Mean Fishing mortality
                "B.om", # OM Total Stock Biomass
               "SB.om", # OM Spawning Stock Biomass
                "C.om", # OM Catch (biomass)
                "L.om", # OM Landings (biomass)
                "D.om", # OM Discards (biomass)

               "C.obs", # OEM Catch (biomass)
               "L.obs", # OEM Landings (biomass)
               "D.obs", # OEM Discards (biomass)

               "F.est", # Estimated Mean Fishing mortality
               "B.est", # Estimated Total Stock Biomass
              "SB.est", # Estimated Spawning Stock Biomass
               "C.est", # Estimated Catch (biomass)
               "L.est", # Estimated Landings (biomass)
               "D.est", # Estimated Discards (biomass)
              addmetrics),
      year=projyrs,
      unit="unique",
      season="unique",
      iter=1:ni))

    track_sel_stk <- FLQuant(NA, dimnames = list(age    = dimnames(om$stks[[x]])$age,
                                                 year   = projyrs,
                                                 iter   = 1:ni))

    return(list(advice  = track_advice_stk,
                sel_om  = track_sel_stk,
                sel_est = track_sel_stk,
                stk     = track_stk))

  })
  names(tracking) <- om$stks@names

  ## track fleet advice
  tracking$quota <- array(NA, dim = c(ns, nf, ny, ni),
                          dimnames = list(stk = om$stks@names,
                                          flt = om$flts@names,
                                          year = projyrs,
                                          iter = 1:ni))

  ## track optimisation
  tracking$optim <- array(NA,
                          dim = c(2,ny, ni),
                          dimnames = list(metrics = c("objective","convergence"),
                                          year   = projyrs,
                                          iter   = 1:ni))
  tracking$message <- array(NA,
                            dim = c(2,ny, ni),
                            dimnames = list(metrics = c("message","rescale"),
                                            year   = projyrs,
                                            iter   = 1:ni))

  ## track fleet quota uptake
  tracking$uptake <- array(NA, dim = c(ns, nf, ny, ni),
                           dimnames = list(stk    = om$stks@names,
                                           flt    = om$flts@names,
                                           year  = projyrs,
                                           iter  = 1:ni))

  ## track fleet overquota discards
  tracking$overquota <- array(NA, dim = c(ns, nf, ny, ni),
                              dimnames = list(stk    = om$stks@names,
                                              flt    = om$flts@names,
                                              year  = projyrs,
                                              iter  = 1:ni))

  return(tracking)
}
