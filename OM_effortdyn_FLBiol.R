# ---
# title: 'Fleet effort dynamics methods'
# ---
#
# Summary
# =======

#' Convert object into simple nested list structure
#'
#' This function converts the linked \code{FLBiols} and \code{FLFisheries}
#' objects into a set of nested lists with the following structure:
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
#' @param om Operating model
#' @param year Integer. Year in which advice is implemented.
#' @param adviceType Character. Takes either 'landings' or 'catch'. If 'landings',
#'                   quota-limits apply to landed weight. If 'catch', quota-limits
#'                   apply to overall catch weight (comprising both landings and discards).
#'
#' NOTE! CURRENTLY ASSUMES THAT CATCH.Q IS FLPAR... MIGHT WANT TO CHANGE THIS...
#'
#' @export

FLBiols2List <- function(om, year, advice) {

  ## How many stocks and fleets?
  nstk <- length(om$stks)
  nflt <- length(om$flts)

  ## How many iterations?
  ## (use the first stock for this -- all dimensions should be identical across stocks and fleets)
  ni <- dimnames(om$stks[[1]])$iter

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

    #-------------------------------------------------#
    # I WILL WANT TO CALCULATE FLEET-STOCK QUOTA HERE #
    #-------------------------------------------------#

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

  ## Call c++ function
  # flr_to_list()

  return(omList)
}

#' Calculation of fleet-stock catches given fleet efforts
#'
#' This function



#' Calculation of fleet efforts given mixed fisheries technical interactions
#'
#' This function calculates the effort for each fleet required to catch their
#' first quota.

effortBaranov <- function(omList,
                          adviceType = "catch",
                          par = NULL,
                          method = "nlminb",
                          correctResid = FALSE,
                          parallel = FALSE){

  ## How many stocks and fleets? - use first iteration for reference
  nstk <- nrow(omList[[1]]$quota)
  nflt <- ncol(omList[[1]]$quota)

  ## How many iterations?
  ni <- length(omList)

  ## if parallel = FALSE, run iteratively
  if (parallel == FALSE){
    out <- lapply(1:ni, function(it) {

      ## define default initial log-effort values if not defined
      if(is.null(par)){
        par <- rep(log(0.5), nflt)
      }

      # ------------------------------------------------------#
      # Define a function to globally optimise effort
      # ------------------------------------------------------#

      Gobj <- function(par, dat, adviceType) {

        ## exponentiate to normal-scale effort
        E <- exp(par)

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
                             partLWage <- partCage * dat$landfrac[[x]] * dat$landwt[[x]]
                             return(colSums(partLWage))

                           } else if(adviceType == "catch"){
                             ## Calculate landings and discards in weight at age per fleet
                             partLWage <- partCage * dat$landfrac[[x]] * dat$landwt[[x]]
                             partDWage <- partCage * (1-dat$landfrac[[x]]) * dat$discwt[[x]]
                             ## Calculate Catch in weight per fleet
                             return(colSums(partLWage + partDWage))
                           }
                         })
        ## Transpose because output has fleets on rows
        Cfleet <- t(Cfleet)

        # Check that fleet catches are within quota limits
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

        ## exponentiate to normal-scale effort
        E <- exp(par)

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
                             partLWage <- partCage * dat$landfrac[[x]] * dat$landwt[[x]]
                             return(colSums(partLWage))

                           } else if(adviceType == "catch"){
                             ## Calculate landings and discards in weight at age per fleet
                             partLWage <- partCage * dat$landfrac[[x]] * dat$landwt[[x]]
                             partDWage <- partCage * (1-dat$landfrac[[x]]) * dat$discwt[[x]]
                             ## Calculate Catch in weight per fleet
                             return(colSums(partLWage + partDWage))
                           }
                         })
        ## Transpose because output has fleets on rows
        Cfleet <- t(Cfleet)

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

      # ------------------------------------------------#
      # Function to estimate fleet catches given effort
      # ------------------------------------------------#

      BaranovCatch <- function(par, dat, adviceType) {

        ## Effort
        E <- par

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
                             partLWage <- partCage * dat$landfrac[[x]] * dat$landwt[[x]]
                             return(colSums(partLWage))

                           } else if(adviceType == "catch"){
                             ## Calculate landings and discards in weight at age per fleet
                             partLWage <- partCage * dat$landfrac[[x]] * dat$landwt[[x]]
                             partDWage <- partCage * (1-dat$landfrac[[x]]) * dat$discwt[[x]]
                             ## Calculate Catch in weight per fleet
                             return(colSums(partLWage + partDWage))
                           }
                         })
        ## Transpose because output has fleets on rows
        Cfleet <- t(Cfleet)

        return(Cfleet)
      }

      # ------------------------------------------------------#
      # Identify choke stocks for each fleet
      # ------------------------------------------------------#

      if(method == "nlminb") {

        eff <- nlminb(start = par, objective = Gobj, dat = omList[[it]],
                      adviceType = adviceType,
                      control = list("iter.max" = 1000,
                                     "eval.max" = 1000))

      } else {

        eff <- optim(par = par, fn = Gobj,
                     dat = omList[[it]],
                     adviceType = adviceType,
                     method = "Nelder-Mead")

      }

      ## Calculate resulting stock catches for each fleet and find
      ## choke stocks

      stkEff <- omList[[it]]$quota - BaranovCatch(par = exp(eff$par),
                                                  dat = omList[[it]],
                                                  adviceType = adviceType)

      ## note that I apply over fleets (cols)
      stkLim <- sapply(1:ncol(stkEff), function(x) { which.min(stkEff[,x])})

      ## Add vector of effort-limiting stock per fleet to omList
      omList[[it]]$stkLim <- stkLim

      # ------------------------------------------------------#
      # Optimise using selected optimisation routine
      # ------------------------------------------------------#

      if(method == "nlminb") {

        out <- nlminb(start = eff$par, objective = Fobj,
                      dat = omList[[it]], adviceType = adviceType,
                      control = list("iter.max" = 1000,
                                     "eval.max" = 1000))

      } else {

        out <- optim(par = eff$par, fn = Fobj,
                     dat = omList[[it]], adviceType = adviceType,
                     method = "Nelder-Mead")

      }

      # ------------------------------------------------------#
      # Check that we have correctly identified choke stocks
      # ------------------------------------------------------#
      #
      # ...If not, re-run optimisation using new vector

      ## Calculate resulting stock catches for each fleet and find choke stocks
      stkEff <- omList[[it]]$quota - BaranovCatch(par = exp(out$par),
                                                  dat = omList[[it]],
                                                  adviceType = adviceType)

      ## Check for mismatch in choke stocks
      stkLimMismatch <- !all(omList[[it]]$stkLim == sapply(1:ncol(stkEff), function(x) { which.min(stkEff[,x])}))

      ## If mismatch, re-run optimisation
      if(stkLimMismatch) {
        cat("Choke stock mis-match detected - rerunning optimisation \n")
        ## Update effort-limiting stock vector
        omList[[it]]$stkLim <- sapply(1:ncol(stkEff), function(x) { which.min(stkEff[,x])})

        ## Re-run optimisation
        if(method == "nlminb") {

          out <- nlminb(start = out$par, objective = Fobj,
                        dat = omList[[it]],
                        adviceType = adviceType,
                        control = list("iter.max" = 1000,
                                       "eval.max" = 1000))

        } else {

          out <- optim(par = out$par, fn = Fobj,
                       dat = omList[[it]],
                       adviceType = adviceType,
                       method = "Nelder-Mead")

        }
        ## recalculate stock overshoots
        stkEff <- omList[[it]]$quota - BaranovCatch(par = exp(out$par),
                                                    dat = omList[[it]],
                                                    adviceType = adviceType)
      }

      # -------------------------------------------------------------#
      # (Optional) Rescale effort down if residual overshoot remains
      # -------------------------------------------------------------#

      ## re-run if overshoot exceeds some value
      if(min(stkEff, na.rm =  TRUE) < -0.001 & correctResid == TRUE) {
        cat("Overquota catches > 0.001 - scaling effort \n")

        ## scale all effort to bring overshoot to zero
        effmult <- optimise(function(x) {
          abs(min(omList[[it]]$quota - BaranovCatch(par = exp(out$par)*x,
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
