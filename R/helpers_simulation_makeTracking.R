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
#' By default, containers for the following properies are generated. Additional
#' containers are generated for user-supplied...

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
                                              years = projyrs,
                                              iters = 1:ni))

    ## track FLstock objects
    #track_stk <- stkfltAll$stks[[x]] ## I think this is profoundly inefficient...

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
                                          years = projyrs,
                                          iters = 1:ni))

  ## track optimisation
  tracking$optim <- array(NA,
                          dim = c(2,ny, ni),
                          dimnames = list(metrics = c("objective","convergence"),
                                          years   = projyrs,
                                          iters   = 1:ni))
  tracking$message <- array(NA,
                            dim = c(2,ny, ni),
                            dimnames = list(metrics = c("message","rescale"),
                                            years   = projyrs,
                                            iters   = 1:ni))

  ## track fleet quota uptake
  tracking$uptake <- array(NA, dim = c(ns, nf, ny, ni),
                           dimnames = list(stk    = om$stks@names,
                                           flt    = om$flts@names,
                                           years  = projyrs,
                                           iters  = 1:ni))

  ## track fleet overquota discards
  tracking$overquota <- array(NA, dim = c(ns, nf, ny, ni),
                              dimnames = list(stk    = om$stks@names,
                                              flt    = om$flts@names,
                                              years  = projyrs,
                                              iters  = 1:ni))

  return(tracking)
}
