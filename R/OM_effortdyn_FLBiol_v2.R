# ---
# title: 'Fleet effort dynamics methods'
# author: 'Matthew Pace'
# date: 'August 2022'
# ---
#
# Summary
# =======

#' Convert object into simple nested list structure
#'
#' This function converts the linked \code{FLBiols} and \code{FLFisheries}
#' objects into a set of nested lists containing the data required by
#' \code{effortBaranov} to optimise fleet efforts to fully catch their first quota.
#'
#' Outputs take the following structure:
#'
#' \itemize{
#'   \item \code{iterations}: a named list
#'   \itemize{
#'     \item \code{n}: a named list
#'     \itemize{
#'       \item vector of numbers-at-age for ith stock
#'     }
#'     \item \code{m}: a named list
#'     \itemize{
#'       \item vector of natural mortality-at-age for ith stock
#'     }
#'     \item \code{landwt}: a named list
#'     \itemize{
#'       \item matrix of landed individual mean weight-at-age for ith stock (age x fleet)
#'     }
#'     \item \code{discwt}: a named list
#'     \itemize{
#'       \item matrix of discarded individual mean weight-at-age for ith stock (age x fleet)
#'     }
#'     \item \code{landfrac}: a named list
#'     \itemize{
#'       \item matrix of proportion of size-selective-retained catch-at-age for
#'             ith stock (age x fleet)
#'     }
#'     \item \code{catchsel}: a named list
#'     \itemize{
#'       \item matrix of catch selectivity-at-age for ith stock (age x fleet)
#'     }
#'     \item \code{catchq}: a matrix of catchability (stock x fleet)
#'     \item \code{quotashare}: a matrix of proportional quota-share (stock x fleet)
#'   }
#' }
#'
#' @param om A list of \code{FLBiols} and \code{FLFisheries} containing the relevant
#'           stock and fleet information in the Operating Model.
#' @param year Integer. Year in which advice is implemented.
#' @param advice A list of the total allowable catch or landings advice for each
#'               stock.
#' @param useCpp (Optional) Logical. Should data be processed using faster C++ code?
#'               Defaults to \code{TRUE}.
#'
#' @export

FLBiols2List <- function(om, year, advice, useCpp = TRUE) {

  # NOTE! CURRENTLY ASSUMES THAT CATCH.Q IS FLPAR... MIGHT WANT TO CHANGE THIS...
  # NOTE! R version CURRENTLY ASSUMES THAT ALL FLEETS CATCH ALL STOCKS ... FIX THIS!

  # ---------------------------------#
  # Extract some summary information
  # ---------------------------------#

  ## How many stocks and fleets?
  nstk <- length(om$stks)
  nflt <- length(om$flts)

  ## How many iterations?
  ## (use the first stock for this -- all dimensions should be identical across stocks and fleets)
  ni <- dims(om$stks[[1]])$iter

  # -------------------#
  # Run checks on data
  # -------------------#

  ## Is year a numeric element?
  if(!is.numeric(year) | length(year) > 1) stop("'year' must be a single numeric value")

  ## Is advice a list with elements for each stock?
  if(!is.list(advice) | length(advice) != nstk) stop("'advice' must be a list with a TAC for each stock")

  ## Do stocks and fisheries have the same number of iterations?
  if(dims(om$flts[[1]])$iter != ni) stop("FLBiols and FLFisheries must have the same number of iterations")

  ## Is there area, season, unit information? (these are not yet supported)
  if(length(dimnames(om$stks[[1]])$unit) > 1 |
     length(dimnames(om$stks[[1]])$season) > 1|
     length(dimnames(om$stks[[1]])$area) >1) stop("Multiple unit, season and area divisions not yet supported")

  # ---------------------------#
  # Run Calculations using C++
  # ---------------------------#
  #
  # Using C++ should be substantially faster at extracting and processing data with
  # large numbers of iterations

  if(useCpp == TRUE) {

    ## Call c++ function
    omList <- flr_to_list(om = om, advice = advice, year = year,
                          nstock = nstk, nfleet = nflt, niter = ni)

  }

  # ---------------------------#
  # Run Calculations using R
  # ---------------------------#
  #
  # Using R is useful to debug issues in the inputs or potential bugs in code.
  # Much slower than C++.

  if(useCpp == FALSE){

    ## loop over each iteration
    omList <- lapply(1:ni, function(x){

      ## extract numbers-at-age for each stock
      ## (I'm guessing that FLasher considers numbers in year x to be numbers at beginning of year x+1)
      n_age <- lapply(1:nstk, function(y){
        n(iter(om$stks[[y]],x))[,ac(year), drop = TRUE]
      })
      names(n_age) <- names(om$stks)

      ## extract natural mortality-at-age
      m_age <- lapply(1:nstk, function(y){
        m(iter(om$stks[[y]], x))[,ac(year), drop = TRUE]
      })
      names(m_age) <- names(om$stks)

      ## extract landings individual mean weight-at-age
      landwt_age <- lapply(1:nstk, function(y){
        wt_y <- sapply(1:nflt, function(z){
          landings.wt(iter(om$flts[[z]][[y]], x))[,ac(year), drop = TRUE]
        })
        colnames(wt_y) <- names(om$flts)
        return(wt_y)
      })
      names(landwt_age) <- names(om$stks)

      ## extract discards individual mean weight-at-age
      discwt_age <- lapply(1:nstk, function(y){
        wt_y <- sapply(1:nflt, function(z){
          discards.wt(iter(om$flts[[z]][[y]], x))[,ac(year), drop = TRUE]
        })
        colnames(wt_y) <- names(om$flts)
        return(wt_y)
      })
      names(discwt_age) <- names(om$stks)

      ## extract proportion catch retained-at-age
      landfrac_age <- lapply(1:nstk, function(y){
        lf_y <- sapply(1:nflt, function(z){
          ## This assumes that the proportions are correct and don't need
          ## normalising
          landings.n(iter(om$flts[[z]][[y]], x))[,ac(year), drop = TRUE]
        })
        colnames(lf_y) <- names(om$flts)
        return(lf_y)
      })
      names(landfrac_age) <- names(om$stks)

      ## extract selection-at-age
      sel_age <- lapply(1:nstk, function(y){
        sel_y <- sapply(1:nflt, function(z){
          catch.sel(iter(om$flts[[z]][[y]], x))[,ac(year), drop = TRUE]
        })
        colnames(sel_y) <- names(om$flts)
        return(sel_y)
      })
      names(sel_age) <- names(om$stks)

      ## Catchability by fleet (matrix stock x fleet)
      q_age <- sapply(1:nstk, function(y){
        q_y <- sapply(1:nflt, function(z){
          catch.q(iter(om$flts[[z]][[y]], x))["alpha", drop = TRUE]
        })
        names(q_y) <- names(om$flts)
        return(q_y)
      })
      colnames(q_age) <- names(om$stks)

      ## Quota-share by fleet (matrix stock x fleet)
      quota_f <- sapply(1:nstk, function(y){
        quota_y <- sapply(1:nflt, function(z){
          iter(attr(om$flts[[z]][[y]],"quotashare"), x)[,ac(year), drop = TRUE] * advice[[y]]
        })
        names(quota_y) <- names(om$flts)
        return(quota_y)
      })
      colnames(quota_f) <- names(om$stks)

      ## return list of objects
      list(n          = n_age,
           m          = m_age,
           landwt     = landwt_age,
           discwt     = discwt_age,
           landfrac   = landfrac_age,
           catchsel   = sel_age,
           catchq     = t(q_age),
           quota = t(quota_f))
    })

  }

  return(omList)
}

#' Calculation of fleet-stock total weight catches or landings given fleet efforts
#'
#' This function calculates the total weight of catches or landings per stock
#' for each fleet given the inputted effort.
#' The function requires input data to be structured following the
#' format given by \code{FLBiols2List}.
#'
#' Input data should contain the following variables listed within each iteration:
#'
#'   \itemize{
#'     \item \code{n}: a named list
#'     \itemize{
#'       \item vector of numbers-at-age for ith stock
#'     }
#'     \item \code{m}: a named list
#'     \itemize{
#'       \item vector of natural mortality-at-age for ith stock
#'     }
#'     \item \code{landwt}: a named list
#'     \itemize{
#'       \item matrix of landed individual mean weight-at-age for ith stock (age x fleet)
#'     }
#'     \item \code{discwt}: a named list
#'     \itemize{
#'       \item matrix of discarded individual mean weight-at-age for ith stock (age x fleet)
#'     }
#'     \item \code{landfrac}: a named list
#'     \itemize{
#'       \item matrix of proportion of size-selective-retained catch-at-age for
#'             ith stock (age x fleet)
#'     }
#'     \item \code{catchsel}: a named list
#'     \itemize{
#'       \item matrix of catch selectivity-at-age for ith stock (age x fleet)
#'     }
#'     \item \code{catchq}: a matrix of catchability (stock x fleet)
#'     \item \code{quotashare}: a matrix of proportional quota-share (stock x fleet)
#'   }
#'
#' @param par Numeric vector. A vector of fleet effort on a log-scale or natural-scale
#' @param dat List. Input data containing numbers-at-age and natural mortality-at-age
#'            for each stock, and landed and discarded mean weights-at-age,
#'            fraction-landed-at-age, catch-selection-at-age, catchability-at-age
#'            and quota-share for each combination of stock and fleet.
#' @param adviceType Character. Values may be 'catch' or 'landings'. Determines
#'                   whether values returned by the function are based on catch
#'                   or landings.
#' @param islog (Optional) Boolean. Is supplied effort (\code{par}) on a log-scale?
#'              Defaults to \code{FALSE}.
#'
#' @return A matrix of the total weight of catch or landings with
#'         fleets on columns and stocks on rows
#'
#' @export

catchBaranov <- function(par, dat, adviceType, islog = FALSE) {

  ## exponentiate to normal-scale effort (if necessary)
  if(islog == TRUE){
    E <- exp(par)
  }
  if(islog == FALSE){
    E <- par
  }

  ## Calculate partial F
  dat$partF <- t(t(dat$catchq) * E) # rows = stocks, cols = fleets

  ## Calculate partial F-at-age using fleet selectivity
  partFage <- sapply(1:nrow(dat$partF),
                     function(x){
                       t(dat$partF[x,] * t(dat$catchsel[[x]]))
                     })

  ## Overall Stock fishing mortality
  Fm   <- rowSums(dat$partF, na.rm = TRUE)

  ## Overall Stock fishing mortality-at-age
  Fage <- sapply(1:length(partFage),
                 function(x) {
                   rowSums(partFage[[x]])
                 })

  ## calculate catch for a given F
  Cage <- sapply(1:nrow(dat$partF),
                 function(x) {

                   (Fage[[x]]/(Fage[[x]] + dat$m[[x]])) * (1-exp(-(Fage[[x]] + dat$m[[x]]))) * dat$n[[x]]

                 })

  # Note that we need to use an ifelse statement to catch instances
  # where Ftot = 0 and avaid NaN issues.

  ## calculate fleet-level catches by weight
  #  - first split Catch number-at-age by fleet partial F-at-age
  #    and calculate Catch weight-at-age
  #  - calculate the landing and discards
  #  - if advice is landings based, find total weight of landings
  #  - if adbice is catch based, find total weight of landings and discards
  #  - then sum over ages to calculate total Catch in weight per stock
  Cfleet <- sapply(1:nrow(dat$partF),
                   function(x){

                     ## to avoid divide by zro issues
                     Fa_x <- ifelse(Fage[[x]] == 0, 1, Fage[[x]])

                     ## Calculate Catch in numbers at age per fleet
                     partCage <- Cage[[x]] * (partFage[[x]] / Fa_x)

                     ## If advice is landings-based, account for
                     ## size-selective discarding
                     if(adviceType == "landings"){
                       
                       ## Calculate landings in weight at age per fleet
                       partLWage <- partCage * dat$landfrac[[x]] * dat$landwt[[x]]
                       
                       ## Handle possibility of NAs if landings weight is NA
                       partLWage[partCage * dat$landfrac[[x]] == 0] <- 0 
                       
                       return(colSums(partLWage))

                     } else if(adviceType == "catch"){
                       
                       ## Calculate landings and discards in weight at age per fleet
                       partLWage <- partCage * dat$landfrac[[x]] * dat$landwt[[x]]
                       partDWage <- partCage * (1-dat$landfrac[[x]]) * dat$discwt[[x]]
                       
                       ## Handle possibility of NAs if landings weight is NA
                       partLWage[partCage * dat$landfrac[[x]] == 0]     <- 0
                       partDWage[partCage * (1-dat$landfrac[[x]]) == 0] <- 0
                       
                       ## Calculate Catch in weight per fleet
                       return(colSums(partLWage + partDWage))
                     }
                   })
  ## Transpose because output has fleets on rows
  Cfleet <- t(Cfleet)

  return(Cfleet)
}

#' Calculation of fleet efforts given mixed fisheries technical interactions
#'
#' This function calculates the effort for each fleet required to catch their
#' first quota.
#'
#' Outputs are a list of the form:
#'
#' \itemize{
#'   \item A list of iterations
#'   \itemize{
#'      \item \code{par}: A vector of log-scale effort per fleet
#'      \item \code{objective}: The final optimisation objective function value.
#'      \item \code{convergence}: The final optimisation convergence code value.
#'                                0 indicates successful convergence.
#'      \item \code{iterations}: The final optimisation number of iterations.
#'      \item \code{message}: The final optimisation convergence message.
#'   }
#' }
#'
#' @param omList List. Input data following the structure returned by \code{FLBiols2List}
#'               containing numbers-at-age and natural mortality-at-age
#'               for each stock, and landed and discarded mean weights-at-age,
#'               fraction-landed-at-age, catch-selection-at-age, catchability-at-age
#'               and quota-share for each combination of stock and fleet.
#' @param adviceType Character. Values may be 'catch' or 'landings'. Determines
#'                   whether inputted advice values correspond to catch or
#'                   landings. Defaults to 'catch'.
#' @param par (Optional) Numeric vector. A vector of fleet effort on a log-scale.
#' @param method (Optional) Character. Determines the optimisation algorithm that is used.
#'               Values may be 'nlminb' or 'Nelder-Mead'. Default is 'nlminb'.
#'               HIGHLY recommended that the default is used.
#' @param maxRetry (Optional) Integer. How many times to re-run optimisation if
#'                 identification of choke-stocks is unsuccessful? Default is 1.
#' @param useEffortAsInit (Optional) Boolean. Should the fleet effort from the 
#'                        previous year be used as initial values. Default is
#'                        \code{FALSE}
#' @param useTMB (Optional) Boolean. Should TMB be used to estimate fleet effort?
#'               Default is \code{TRUE}. Highly recommended that the default is
#'               used.
#' @param correctResid (Optional) Boolean. Should fleet efforts be scaled down
#'                     to bring residual overquota catch to zero if residual
#'                     over-quota catch following optimisation exceeds 0.001?
#'                     Defaults to \code{FALSE}
#' @param parallel (Optional) Boolean. Should function be run in parallel?
#'
#' @return A list containing outputs for the final optimisation.
#'
#' @export

effortBaranov <- function(omList,
                          adviceType = "catch",
                          par = NULL,
                          maxRetry = 1,
                          useEffortAsInit = FALSE,
                          useTMB          = TRUE,
                          correctResid    = FALSE,
                          parallel        = FALSE){
  
  # ======================================================#
  # Define dimensions
  # ======================================================#
  
  ## How many stocks and fleets? - use first iteration for reference
  nstk <- nrow(omList[[1]]$quota)
  nflt <- ncol(omList[[1]]$quota)

  ## How many iterations?
  ni <- length(omList)
  
  # ======================================================#
  # Define objective function
  # ======================================================#
  
  if (useTMB == FALSE){
    
    # ------------------------------------------------------#
    # Define a function to globally optimise effort
    # ------------------------------------------------------#
    
    Gobj <- function(par, dat, adviceType) {
      
      ## Calculate Catch given effort
      Cfleet <- catchBaranov(par = par, dat = dat, adviceType = adviceType, islog = TRUE)
      
      ## Check that fleet catches are within quota limits
      overundershoot <- dat$quota - Cfleet
      
      ## Add an extra penalty to the overshoot
      overshoot <- overundershoot[which(overundershoot < 0)]
      undershoot <- overundershoot[which(overundershoot > 0)]
      
      return(sum(overshoot^2) + sum(undershoot))
      
    }
    
    # ------------------------------------------------------#
    # Define a function to optimise effort for selected stocks
    # ------------------------------------------------------#
    
    Fobj <- function(par, dat, adviceType) {
      
      ## Calculate Catch given effort
      Cfleet <- catchBaranov(par = par, dat = dat, adviceType = adviceType, islog = TRUE)
      
      # extract discrepancy between fleet catches and quota
      quotaResid <- dat$quota - Cfleet
      
      ## replace NAs with 0 (cases where no catchability)
      quotaResid[is.na(quotaResid)] <- 0
      
      ## extract discrepancy for effort limiting stocks
      quotaResid_lim <- sapply(1:length(dat$stkLim), function(x){
        quotaResid[dat$stkLim[x],x]
      })
      
      return(sum(quotaResid_lim^2))
    }
    
  }
  
  # ======================================================#
  # Loop over each iteration
  # ======================================================#

  ## if parallel = FALSE, run iteratively
  if (parallel == FALSE){
    out <- lapply(1:ni, function(it) {

      ## define default initial log-effort values if not defined
      if(is.null(par) & useEffortAsInit == FALSE){
        par <- rep(log(0.5), nflt)
      }
      
      if(is.null(par) & useEffortAsInit == TRUE){
        par <- log(omList[[it]]$effort)
      }

      # ------------------------------------------------------#
      # Identify choke stocks for each fleet
      # ------------------------------------------------------#
      #
      # If useTMB is FALSE, then we use a previously defined objective function
      # to estimate fleet efforts. We're not expecting to achieve effort targets.
      # Instead, we intend to identify the stock for each fleet that chokes
      # effort.
      
      if(useTMB == FALSE){
        
        eff <- nlminb(start = par, objective = Gobj, dat = omList[[it]],
                      adviceType = adviceType,
                      control = list("iter.max" = 1000,
                                     "eval.max" = 1000))
        
        ## Calculate resulting stock catches for each fleet and find choke stocks
        stkEff <- omList[[it]]$quota - catchBaranov(par = exp(eff$par),
                                                    dat = omList[[it]],
                                                    adviceType = adviceType)
        
        ## Find effort-limiting stock for each fleet
        ## NOTE: I apply over fleets (cols)
        ## NOTE: we need to select from only stocks the fleet has catching power for!
        stkLim <- sapply(1:ncol(stkEff), function(x) { 
          xx <- stkEff[,x]
          xx[omList[[it]]$catchq[,x] == 0] <- NA
          which.min(xx)
        })
        
        ## Add vector of effort-limiting stock per fleet to omList
        omList[[it]]$stkLim <- stkLim
      }
      
      # If useTMB is TRUE, then we use a TMB to generate an objective function
      # and a gradient function that we pass to nlminb. The process is a little
      # more complicated because we re-organise the way we pass the parameters
      # to the function.
      
      if(useTMB == TRUE){
        
        ## Redefine parameters
        parm <- list()
        parm$logE <- par
        
        ## add advice type to data
        omList[[it]]$adviceType <- adviceType
        omList[[it]]$objType    <- "global"
        omList[[it]]$stkLim     <- rep(1, nflt)
        
        Gobj <- TMB::MakeADFun(data = omList[[it]],
                          parameters = parm,
                          DLL="TMBobj", silent=TRUE)
        
        eff <- nlminb(start = Gobj$par, objective = Gobj$fn, gradient = Gobj$gr,
                      adviceType = adviceType,
                      control = list("iter.max" = 10000,
                                     "eval.max" = 10000))
        
        ## Calculate resulting stock catches for each fleet and find choke stocks
        stkEff <- omList[[it]]$quota - Gobj$report()$Cfleet
        
        ## Find effort-limiting stock for each fleet
        ## NOTE: I apply over fleets (cols)
        ## NOTE: we need to select from only stocks the fleet has catching power for!
        stkLim <- sapply(1:ncol(stkEff), function(x) { 
          xx <- stkEff[,x]
          xx[omList[[it]]$catchq[,x] == 0] <- NA
          which.min(xx)
        })
        
        ## Add vector of effort-limiting stock per fleet to omList
        omList[[it]]$stkLim <- stkLim - 1
      }
      
      # ------------------------------------------------------#
      # Optimise fleet efforts to consume choke stock quota
      # ------------------------------------------------------#
      
      ## use last year effort or global optimisation output as initial values
      if(useEffortAsInit == TRUE){
        par <- log(omList[[it]]$effort)
      } else {
        par <- eff$par
      }
      
      # If useTMB is FALSE, then we use a previously defined objective function
      # to estimate the effort required to consume choke stock quota.
      # We check that we have correctly identified the choke stocks by comparing
      # the inputted choke stocks with the emergent vector of highest stock quota
      # uptake
      
      if(useTMB == FALSE) {
        
        out <- nlminb(start = par, objective = Fobj,
                      dat = omList[[it]], adviceType = adviceType,
                      control = list("iter.max" = 1000,
                                     "eval.max" = 1000))
        
        ## Calculate resulting stock catches for each fleet and find choke stocks
        stkEff <- omList[[it]]$quota - catchBaranov(par = exp(out$par),
                                                    dat = omList[[it]],
                                                    adviceType = adviceType)
        
        ## Check for mismatch in choke stocks
        stkLimMismatch <- !all(omList[[it]]$stkLim == sapply(1:ncol(stkEff), function(x) {
          xx <- stkEff[,x]
          xx[omList[[it]]$catchq[,x] == 0] <- NA
          which.min(xx)
        }))
        
      }
      
      # If useTMB is TRUE, there is some reorganisation of how parameters are 
      # passed, and the TMB objective and gradient functions are built. These
      # estimate the effort required to consume choke stock quota.
      # We check that we have correctly identified the choke stocks by comparing
      # the inputted choke stocks with the emergent vector of highest stock quota
      # uptake
      
      if(useTMB == TRUE) {
        
        parm$logE <- par
        omList[[it]]$objType    <- "choke"
        
        Fobj <- TMB::MakeADFun(data = omList[[it]],
                          parameters = parm,
                          DLL="TMBobj", silent=TRUE)
        
        out <- nlminb(start = Fobj$par, objective = Fobj$fn, gradient = Fobj$gr,
                      control = list("iter.max" = 10000,
                                     "eval.max" = 10000))
        
        ## Calculate resulting stock catches for each fleet and find choke stocks
        stkEff <- omList[[it]]$quota - Fobj$report()$Cfleet
        
        ## Check for mismatch in choke stocks
        stkLimMismatch <- !all(omList[[it]]$stkLim == sapply(1:ncol(stkEff), function(x) {
          xx <- stkEff[,x]
          xx[omList[[it]]$catchq[,x] == 0] <- NA
          which.min(xx)-1
        }))
      }

      # ----------------------------------------------------------------#
      # (OPTIONAL) Re-run optimisation using updated choke stock vector
      # ----------------------------------------------------------------#

      ## If mismatch, re-run optimisation
      while(stkLimMismatch == TRUE & maxRetry > 0) {
        
        cat("Choke stock mis-match detected - rerunning optimisation \n")
        
        ## use last year effort or global optimisation output as initial values
        if(useEffortAsInit == TRUE){
          par <- log(omList[[it]]$effort)
        } else {
          par <- out$par
        }
        
        if(useTMB == FALSE) {
          
          ## Update effort-limiting stock vector
          omList[[it]]$stkLim <- sapply(1:ncol(stkEff), function(x) { 
            xx <- stkEff[,x]
            xx[omList[[it]]$catchq[,x] == 0] <- NA
            which.min(xx)
          })
          
          ## optimise efforts
          out <- nlminb(start = par, objective = Fobj,
                        dat = omList[[it]],
                        adviceType = adviceType,
                        control = list("iter.max" = 1000,
                                       "eval.max" = 1000))
          
          ## recalculate stock overshoots
          stkEff <- omList[[it]]$quota - catchBaranov(par = exp(out$par),
                                                      dat = omList[[it]],
                                                      adviceType = adviceType)
          
          ## Check for mismatch in choke stocks
          stkLimMismatch <- !all(omList[[it]]$stkLim == sapply(1:ncol(stkEff), function(x) { 
            xx <- stkEff[,x]
            xx[omList[[it]]$catchq[,x] == 0] <- NA
            which.min(xx)
          }))
          
          ## Save vector of fleet choke stocks
          out$stkLim <- omList[[it]]$stkLim
        }
        
        if(useTMB == TRUE) {
          
          ## Update effort-limiting stock vector
          omList[[it]]$stkLim <- sapply(1:ncol(stkEff), function(x) { 
            xx <- stkEff[,x]
            xx[omList[[it]]$catchq[,x] == 0] <- NA
            which.min(xx)-1
          })
          
          ## update starting parameters
          parm$logE <- par
          
          ## Make TMB function
          Fobj <- TMB::MakeADFun(data = omList[[it]],
                            parameters = parm,
                            DLL="TMBobj", silent=TRUE)
          
          ## optimise efforts
          out <- nlminb(start = Fobj$par, objective = Fobj$fn, gradient = Fobj$gr,
                        control = list("iter.max" = 10000,
                                       "eval.max" = 10000))
          
          ## recalculate stock overshoots
          stkEff <- omList[[it]]$quota - Fobj$report()$Cfleet
          
          ## Check for mismatch in choke stocks
          stkLimMismatch <- !all(omList[[it]]$stkLim == sapply(1:ncol(stkEff), function(x) { 
            xx <- stkEff[,x]
            xx[omList[[it]]$catchq[,x] == 0] <- NA
            which.min(xx)-1
          }))
          
          ## Save vector of fleet choke stocks
          out$stkLim <- omList[[it]]$stkLim + 1
          
          ## attach catch matrix to out
          out$Cfleet <- Fobj$report()$Cfleet
          
          ## attach TMB 
          sdr<- TMB::sdreport(Fobj)
          pl   <- as.list(sdr,"Est")
          plsd <- as.list(sdr,"Std")
          
          out$pl <- pl
          out$plsd <- plsd
          
        }
        
        ## incrementally decrease number of tries
        maxRetry <- maxRetry - 1
      }

      # -------------------------------------------------------------#
      # (Optional) Rescale effort down if residual overshoot remains
      # -------------------------------------------------------------#
      
      ## Does not currently work with TMB
      if(correctResid == TRUE & useTMB == TRUE) 
        stop("correctResid does not currently work with TMB")

      ## re-run if overshoot exceeds some value
      if(min(stkEff, na.rm =  TRUE) < -0.001 & correctResid == TRUE) {
        cat("Overquota catches > 0.001 - scaling effort \n")

        ## scale all effort to bring overshoot to zero
        effmult <- optimise(function(x) {
          abs(min(omList[[it]]$quota - catchBaranov(par = exp(out$par)*x,
                                                    dat = omList[[it]],
                                                    adviceType = adviceType),
                  na.rm = TRUE))},
          c(0,1))

        out$par     <- log(exp(out$par)*effmult$minimum)
        out$message <- c(out$message,paste0("effort re-scaled: ",effmult$minimum))

        return(out)

      } else {

        return(out)
      }
    })
  } else if(parallel == TRUE){

    stop("parallelisation not yet implemented")
  }

  return(out)
}
