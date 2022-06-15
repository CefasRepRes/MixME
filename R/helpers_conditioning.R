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
#'  - ...
#'
#' Functions
#' =========
#' 
#' multiSAM2FLR
#' ------------
#' 
#' This function takes a SAM fitted stock assessment object as input and 
#' returns an FLStock object.
#' 
#' @param SAMfit A SAM fitted stock assessment model object
#' @return A list containing an FLStock and FLFleets object
#' @export

multiSAM2FLR <- function(SAMfit) {
  
  ## Extract dimensions
  ages       <- SAMfit$conf$minAge:SAMcod$conf$maxAge
  years      <- SAMfit$data$years
  fbar_range <- SAMfit$conf$fbarRange
  
  ## Generate empty FLQuant object
  qnt <- FLQuant(NA, dimnames = list(year = years,
                                            age  = ages,
                                            iter = 1))
  ## Generate empty FLStock object
  stk <- FLStock(qnt)
  
  ## Update Fbar-range
  fbar(stk)
  
  ## Fill stock numbers
  
  
  ## Extract fishing mortality at age 
  idx <- (SAMfit$conf$keyLogFsta + 1)[1,] # index for fleet 1
  SAMfit$pl$logF[idx,]
  
  ## Fill stock weights
  stock.wt(stk) <- c(t(SAMfit$data$stockMeanWeight))

  ## Fill natural mortality
  m(stk) <- c(t(SAMfit$data$natMor))
  
  ## Fill maturity
  mat(stk) <- c(t(SAMfit$data$propMat))
  
  ## Fill harvest (this will be the total fishing mortality at age across fleets)
  
  ## Fill proportion fishing mortality before spawning
  harvest.spwn(stk) <- c(t(SAMfit$data$propF))
  
  ## Fill proportion natural mortality before spawning
  m.spwn(stk) <- c(t(SAMfit$data$propM))
  
  
  SAMfit$conf$
    stock.n(emptyFLStock) <- 
  
  return()
}



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