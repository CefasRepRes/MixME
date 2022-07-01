#' ---
#' title: 'Functions to help condition Operating Models'
#' author: 'Matthew Pace'
#' date: 'June 2022'
#' ---
#'
#' Summary
#' =======
#'
#' This script contains several function to help condition multi-stock operating
#' models.
#'
#' The functions include:
#'
#'  - multiSAM2FLR
#'
#' Functions
#' =========

## Define a function to calculate fleet F-at-age
ffaytable <- function(fit) {

  ## row index of fleets
  fleetIndex <- which(fit$data$fleetTypes == 0)
  fleetList  <- list()

  ## perform operation for each fleet
  for(ii in fleetIndex) {

    ## Extract index of F-at-age
    fayindex <- fit$conf$keyLogFsta[ii,] + 1
    names(fayindex) <- fit$conf$minAge:fit$conf$maxAge

    ffay_fit <- t(fit$pl$logF[fayindex,])

    # matrix(0, nrow = length(fit1$data$year),
    #        ncol = length(fit1$conf$minAge:fit1$conf$maxAge),
    #        dimnames = list(year = fit1$data$year,
    #                        age  = fit1$conf$minAge:fit1$conf$maxAge))

    rownames(ffay_fit) <- fit$data$years
    colnames(ffay_fit) <- names(fayindex)[fayindex > 0]

    fleetList[ii] <- list(as.data.frame(ffay_fit) %>% mutate(Fleet = ii) %>% rownames_to_column("year"))
  }

  return(fleetList)
}


## Define a function to extract observation variance
ObsVartable <- function(fit, nfleet) {

  ObsVarKey <- fit$conf$keyVarObs[1:nfleet,]
  ObsVar    <- fit$plsd$logSdLogObs

  ObsVarData <- ObsVarKey
  ObsVarData[] <- ObsVar[ObsVarKey + 1]

  if(is.null(dim(ObsVarData))) {
    names(ObsVarData) <- fit$conf$minAge : fit$conf$maxAge
  } else {
    colnames(ObsVarData) <- fit$conf$minAge : fit$conf$maxAge
    rownames(ObsVarData) <- attr(fit$data, "fleetNames")[1:nfleet]
  }

  return(ObsVarData)
}



### function to extract logObs for a fleet
fleetLogObs <- function(fit, fleet = NULL) {

  ## Extract index for fleet observations
  idx<-fit$data$aux[,"fleet"]%in%fleet

  ## Extract fitted index-at-age for fleet
  fitted_Obs   <- exp(fit$obj$report(c(fit$sdrep$par.fixed,fit$sdrep$par.random))$predObs[idx])

  ## Not confident about this... I have 3 different logSdLogObs in parameter table... but not separate
  ## by age... however, is this better than what I have below?
  # fitted_ObsSd <- exp(sqrt(rowSums(fit$obj$report(c(fit$sdrep$par.fixed,fit$sdrep$par.random))$obsCov[[fleet]])))

  ## Extract observed index-at-age for fleet
  data_Obs <- exp(fit$data$logobs[idx])

  ## Extract ancilliary data
  ages  <- fit$data$aux[idx,"age"]
  years <- fit$data$aux[idx,"year"]

  # ages_lookup <- data.frame(Sd  = fitted_ObsSd,
  #                           age = ages)

  return(data.frame(fitted   = fitted_Obs,
                    observed = data_Obs,
                    age      = ages,
                    year     = years) # %>%
         #left_join(ages_lookup, by = "age")
  )
}
