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

    ## track stock advice (as implemented)
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

               "conv.est", # Stock estimation model convergence
               
               "F.est", # Estimated Mean Fishing mortality
               "B.est", # Estimated Total Stock Biomass
              "SB.est", # Estimated Spawning Stock Biomass
               "C.est", # Estimated Catch (biomass)
               "L.est", # Estimated Landings (biomass)
               "D.est", # Estimated Discards (biomass)
              
              "hcr.adv", # HCR advice output (fbar / catch)
              
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
  
  ## track failed management procedure iterations
  tracking$iterfail <- matrix(0, nrow = ny, ncol = ni,
                              dimnames = list(year = projyrs,
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
  
  ## track fleet choke stock
  tracking$choke <- array(NA, dim = c(nf,ny,ni),
                          dimnames = list(flt = om$flts@names,
                                          year = projyrs,
                                          iter = 1:ni))

  return(tracking)
}

# Update tracking object with OM properties for a given year
# ===========================================================#
# 
# Function updates total stock biomass (B.om), spawning stock biomass (SB.om),
# Landings (L.om), Discards (D.om), Catch (C.om), mean fishing mortality (F.om)
# and fishing selectivity at age
#' @export

updateTrackingOM <- function(om, tracking, args, yr) {
  
  ## Update tracking object - True Stock Properties
  for(x in om$stks@names) {
    
    ## Update stock numbers
    tracking[[x]]$stk["B.om",  ac(yr)] <- 
      quantSums(om$stks[[x]]@n[,ac(yr)] * om$stks[[x]]@wt[,ac(yr)])
    tracking[[x]]$stk["SB.om", ac(yr)] <- 
      quantSums(om$stks[[x]]@n[,ac(yr)] * 
                  om$stks[[x]]@wt[,ac(yr)] * 
                  om$stks[[x]]@mat$mat[,ac(yr)])
    
    ## Extract catches for stock
    fltcatches <- lapply(om$flts, "[[", x)

    ## Calculate overall landings and discards
    fltlandings <- sapply(1:length(fltcatches), function(y){
      if(!is.null(fltcatches[[y]])) {
        landings(fltcatches[[y]])[,ac(yr)]
      } else {
        FLQuant(0, dimnames = list(year = ac(yr), iter = dimnames(om$stks[[x]])$iter))
      }
    }, simplify = "array")
    
    fltdiscards <- sapply(1:length(fltcatches), function(y){
      if(!is.null(fltcatches[[y]])) {
        discards(fltcatches[[y]])[,ac(yr)]
      } else {
        FLQuant(0, dimnames = list(year = ac(yr), iter = dimnames(om$stks[[x]])$iter))
      }
    }, simplify = "array")
    
    if(is.array(fltlandings)) {
      fltlandings <- apply(fltlandings, c(1:6), sum)
      fltdiscards <- apply(fltdiscards, c(1:6), sum)
    } else {
      fltlandings <- sum(fltlandings)
      fltdiscards <- sum(fltdiscards)
    }


    
    ## update landings, discards and catch numbers in tracking object
    tracking[[x]]$stk["L.om",  ac(yr)] <- fltlandings
    tracking[[x]]$stk["D.om",  ac(yr)] <- fltdiscards
    tracking[[x]]$stk["C.om",  ac(yr)] <- fltlandings + fltdiscards
    
    ## Update harvest
    fltFage <- sapply(1:length(om$flts), function(y){
      
      if(!is.null(om$flts[[y]][[x]])) {
        om$flts[[y]][[x]]@catch.q[1,] %*% 
          om$flts[[y]]@effort[,ac(yr)] %*% 
          om$flts[[y]][[x]]@catch.sel[,ac(yr)]
      } else {
        
        ## A bit of a hacky way to retrieve correct dimensions
        FLQuant(0, dimnames = list(age = dimnames(om$stk[[x]])$age, 
                                   year = yr, 
                                   iter = dimnames(om$stk[[x]])$iter))
      }
    }, simplify = "array")
    
    Fage <- apply(fltFage, c(1:6), sum)
    tracking[[x]]$stk["F.om",  ac(yr)] <- 
      apply(Fage[ac(args$frange[[x]][1]:args$frange[[x]][2]),,,,,,drop = FALSE], c(2:6), mean)
    
    ## Save Selectivity
    tracking[[x]]$sel_om[,ac(yr)] <-  sweep(Fage, c(2:6), tracking[[x]]$stk["F.om",  ac(yr)], "/")
  }
  
  return(tracking)
}
