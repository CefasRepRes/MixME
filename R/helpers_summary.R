# ---
# title: 'Helper functions to generate summary outputs'
# author: 'Matthew Pace'
# date: 'December 2022'
# ---
#
#' Generate summaries of Operating Model spawning stock biomass
#' 
#' Function takes an Operating model as input and generate a summary dataframe
#' of spawning stock biomass for one or more stocks.
#' 
#' @export

summary_ssb_MixME <- function(object, 
                              minyr = NULL,
                              maxyr = NULL,
                              stknames = NULL) {
  
  # ----------------
  # extract elements
  # ----------------
  
  ## extract object elements
  om       <- object$om
  tracking <- object$tracking
  
  ## handle null cases
  if(is.null(minyr)) minyr <- max(unlist(lapply(om$stks, function(x) dims(x)$minyear)))
  if(is.null(maxyr)) maxyr <- min(unlist(lapply(om$stks, function(x) dims(x)$maxyear)))
  
  # -------------------------------------------------
  # calculate summary quantity and correct dimensions
  # -------------------------------------------------
  
  ## Calculate summary array
  res <- sapply(1:length(om$stks), function(x){
    ssb(om$stks[[x]])[,ac(minyr:maxyr), drop = FALSE]
  }, simplify = "array")
  
  ## define dimension names
  names(dimnames(res))[7] <- "stk"
  dimnames(res)$stk       <- names(om$stks)
  
  ## transform array to dataframe
  res <- as.data.frame.table(res)
  names(res)[names(res) == "Freq"] <- "SSB"
  
  ## (optional) filter for specific stocks
  if(!is.null(stknames)) {
    res <- res[res$stk %in% stknames,]
  }
  
  ## coerce "year" and "iter" to numeric
  res$year <- as.numeric(as.character(res$year))
  res$iter <- as.numeric(as.character(res$iter))
  
  return(res)
}

#' Generate summaries of Operating Model fleet efforts
#' 
#' Function takes an Operating model as input and generate a summary dataframe
#' of effort for one or more fleets
#' 
#' @export

summary_effort_MixME <- function(object, 
                                 minyr = NULL,
                                 maxyr = NULL,
                                 fltnames = NULL) {
  
  # ----------------
  # extract elements
  # ----------------
  
  ## extract object elements
  om       <- object$om
  tracking <- object$tracking
  
  ## handle null cases
  if(is.null(minyr)) minyr <- max(unlist(lapply(om$stks, function(x) dims(x)$minyear)))
  if(is.null(maxyr)) maxyr <- min(unlist(lapply(om$stks, function(x) dims(x)$maxyear)))
  
  # -------------------------------------------------
  # calculate summary quantity and correct dimensions
  # -------------------------------------------------
  
  ## Extract effort
  res <- sapply(names(om$flts), function(x) {
    effort(om$flts[[x]])[,ac(minyr:maxyr), drop = FALSE]
  }, simplify = "array")
  
  ## aggregate over metiers
  if(is.list(res)) {
    res <- sapply(res, function(x) {
      x <- areaSums(x)
      return(x)
    }, simplify = "array")
  }
  
  ## define dimension names
  names(dimnames(res))[7] <- "flt"
  dimnames(res)$flt      <- names(om$flts)
  
  ## transform array to dataframe
  res <- as.data.frame.table(res)
  names(res)[names(res) == "Freq"] <- "effort"
  
  ## (optional) filter for specific stocks
  if(!is.null(fltnames)) {
    res <- res[res$flt %in% fltnames,]
  }
  
  ## coerce "year" and "iter" to numeric
  res$year <- as.numeric(as.character(res$year))
  res$iter <- as.numeric(as.character(res$iter))
  
  return(res)
  
}

#' Generate summaries of Operating Model fleet-stock catches
#' 
#' Function takes an Operating model as input and generate a summary dataframe
#' of fleet-stock catches for one or more fleets
#' 
#' @export

summary_catch_MixME <- function(object, 
                                minyr = NULL,
                                maxyr = NULL,
                                fltnames = NULL,
                                stknames = NULL,
                                byfleet = FALSE) {
  
  # ----------------
  # extract elements
  # ----------------
  
  ## extract object elements
  om       <- object$om
  tracking <- object$tracking
  
  ## handle null cases
  if(is.null(minyr)) minyr <- max(unlist(lapply(om$stks, function(x) dims(x)$minyear)))
  if(is.null(maxyr)) maxyr <- min(unlist(lapply(om$stks, function(x) dims(x)$maxyear)))
  
  # -------------------------------------------------
  # calculate summary quantity and correct dimensions
  # -------------------------------------------------
  
  ## leverage c++ summary functions
  res <- lapply(names(om$stks), function(x) {
    xx <- MixME:::getCW(om$flts, x, sl = "landings", summarise = !byfleet) +
      MixME:::getCW(om$flts, x, sl = "discards", summarise = !byfleet)
    xx <- as.data.frame.table(xx)
    xx$stk = x
    return(xx)
  })
  
  res <- do.call(rbind, res)
  
  ## (Optional) handle aggregation by fleet
  if (byfleet == TRUE) {
    res <- res[,c("age","year","unit","season","iter","fleet","stk","Freq")]
    names(res)[names(res) == "fleet"] <- "flt"
  }
  
  if (byfleet == FALSE) {
    res <- res[,c("age","year","unit","season","iter","stk","Freq")]
  }
  
  ## rename variable
  names(res)[names(res) == "Freq"]  <- "catch"
  
  ## (optional) filter for specific stocks
  if(!is.null(stknames)) {
    res <- res[res$stk %in% stknames,]
  }
  
  ## (optional) filter for specific fleets
  if(!is.null(fltnames)) {
    res <- res[res$flt %in% fltnames,]
  }
  
  ## coerce "year" and "iter" to numeric
  res$year <- as.numeric(as.character(res$year))
  res$iter <- as.numeric(as.character(res$iter))
  
  return(res)
  
}

#' Generate summaries of Operating Model stock quota uptake
#' 
#' Function takes an Operating model as input and generate a summary dataframe
#' of quota uptake for one or more stocks
#' 
#' @export

summary_uptake_MixME <- function(object, 
                                 minyr = NULL,
                                 maxyr = NULL,
                                 fltnames = NULL,
                                 stknames = NULL,
                                 byfleet = FALSE) {
  
  # ----------------
  # extract elements
  # ----------------
  
  ## extract object elements
  om       <- object$om
  tracking <- object$tracking
  
  # -------------------------------------------------
  # extract summary quantity and correct dimensions
  # -------------------------------------------------
  
  res  <- tracking$uptake
  res2 <- tracking$quota
  
  ## transform array to dataframe
  res <- as.data.frame.table(res)
  names(res)[names(res) == "Freq"] <- "uptake"
  
  res2 <- as.data.frame.table(res2)
  names(res2)[names(res2) == "Freq"] <- "quota"

  res <- merge(res, res2, c("stk","flt","year","iter"))
  
  ## coerce "year" and "iter" to numeric
  res$year <- as.numeric(as.character(res$year))
  res$iter <- as.numeric(as.character(res$iter))
  
  ## (optional) filter for specific stocks
  if(!is.null(stknames)) {
    res <- res[res$stk %in% stknames,]
  }
  
  ## (optional) filter for specific fleets
  if(!is.null(fltnames)) {
    res <- res[res$flt %in% fltnames,]
  }
  
  ## (optional) filter for specific years
  if(!is.null(minyr)) {
    res <- res[res$year >= minyr,]
  }
  if(!is.null(maxyr)) {
    res <- res[res$year <= maxyr,]
  }
  
  if(byfleet == TRUE) {
    res$uptake_percentage <- ((res$quota - res$uptake)/res$quota) * 100
    
    ## if quota = 0 and uptake = 0, % uptake = 100
    res$uptake_percentage[res$quota == 0 & res$uptake == 0] <- 100
    
    return(res[,c("stk","flt","year","iter","uptake_percentage")])
    
  } else {
    summary_uptake <- aggregate(res, cbind(quota, uptake) ~ year + stk + iter, sum)
    summary_uptake$uptake_percentage <- ((summary_uptake$quota - summary_uptake$uptake)/summary_uptake$quota) * 100
    
    ## if quota = 0 and uptake = 0, % uptake = 100
    summary_uptake$uptake_percentage[summary_uptake$quota == 0 & summary_uptake$uptake == 0] <- 100
    
    return(summary_uptake[,c("stk","year","iter","uptake_percentage")])
  }

}

#' Generate summaries of Operating Model mean fishing mortality
#' 
#' Function takes an Operating model as input and generate a summary dataframe
#' of mean fishing mortaltiy for one or more stocks
#' 
#' @export

summary_fbar_MixME <- function(object, 
                               minyr = NULL,
                               maxyr = NULL,
                               stknames = NULL) {
  
  # -------------------------------------
  # extract elements and define functions
  # -------------------------------------
  
  ## extract object elements
  om       <- object$om
  tracking <- object$tracking
  args     <- object$args
  
  ## handle null cases
  if(is.null(minyr)) minyr <- max(unlist(lapply(om$stks, function(x) dims(x)$minyear)))
  if(is.null(maxyr)) maxyr <- min(unlist(lapply(om$stks, function(x) dims(x)$maxyear)))
  if(is.null(stknames)) stknames <- unique(unlist(sapply(om$flts, names)))

  # -------------------------------------------------
  # calculate summary quantity and correct dimensions
  # -------------------------------------------------
  
  res <- lapply(stknames, function(x) {
    
    ## get Fbar range
    frange <- object$args$frange[[x]]
    
    ## get fishing mortality at age
    xx <- MixME:::getFage(object$om$stks, object$om$flts, x)
    
    ## get Fbar and coerce to dataframe
    xx <- FLCore::quantMeans(xx[as.character(frange[1]:frange[2]),])
    xx <- as.data.frame.table(xx)
    xx$stk = x
    return(xx)
  })
  
  res <- do.call(rbind, res)[,c("age","year","unit","season","iter","stk","Freq")]
  names(res)[names(res) == "Freq"] <- "fbar"
  
  # -------------------------------------------------
  # optional filter and return result
  # -------------------------------------------------
  
  ## coerce "year" and "iter" to numeric
  res$year <- as.numeric(as.character(res$year))
  res$iter <- as.numeric(as.character(res$iter))
  
  ## (optional) filter for specific years
  if(!is.null(minyr)) {
    res <- res[res$year >= minyr,]
  }
  if(!is.null(maxyr)) {
    res <- res[res$year <= maxyr,]
  }
  
  return(res)
  
}

#' Generate summaries of Operating Model fishing mortality at age
#' 
#' Function takes an Operating model as input and generate a summary dataframe
#' of mean fishing mortality at age for one or more stocks
#' 
#' @export

summary_f_MixME <- function(object, 
                               minyr = NULL,
                               maxyr = NULL,
                               stknames = NULL) {
  
  # -------------------------------------
  # extract elements and define functions
  # -------------------------------------
  
  ## extract object elements
  om       <- object$om
  tracking <- object$tracking
  
  ## handle null cases
  if(is.null(minyr)) minyr <- max(unlist(lapply(om$stks, function(x) dims(x)$minyear)))
  if(is.null(maxyr)) maxyr <- min(unlist(lapply(om$stks, function(x) dims(x)$maxyear)))
  if(is.null(stknames)) stknames <- unique(unlist(sapply(om$flts, names)))
  
  # -------------------------------------------------
  # calculate summary quantity and correct dimensions
  # -------------------------------------------------
  
  res <- lapply(stknames, function(x) {
    
    ## get fishing mortality at age
    xx <- MixME:::getFage(object$om$stks, object$om$flts, x)
    
    ## coerce to dataframe
    xx <- as.data.frame.table(xx)
    xx$stk = x
    return(xx)
  })
  
  res <- do.call(rbind, res)[,c("age","year","unit","season","iter","stk","Freq")]
  names(res)[names(res) == "Freq"] <- "f"
  
  # -------------------------------------------------
  # return result
  # -------------------------------------------------
  
  ## coerce "year" and "iter" to numeric
  res$year <- as.numeric(as.character(res$year))
  res$iter <- as.numeric(as.character(res$iter))
  
  return(res)
  
}

#' Generate summaries of Operating Model risk
#' 
#' Function takes an Operating model as input and generate a summary dataframe
#' of risk (the annual probability of SSB falling below Blim) for one or more stocks
#' 
#' @export

summary_risk_MixME <- function(object, 
                               minyr = NULL,
                               maxyr = NULL,
                               stknames = NULL,
                               Refpts) {
  
  # ----------------
  # extract elements
  # ----------------
  
  ## extract object elements
  om       <- object$om
  tracking <- object$tracking
  
  ## handle null cases
  if(is.null(minyr)) minyr <- max(unlist(lapply(om$stks, function(x) dims(x)$minyear)))
  if(is.null(maxyr)) maxyr <- min(unlist(lapply(om$stks, function(x) dims(x)$maxyear)))
  
  # -------------------------------------------------
  # calculate summary quantity and correct dimensions
  # -------------------------------------------------
  
  ## Calculate summary array
  res <- sapply(names(om$stks), function(x){
    ssb(om$stks[[x]])[,ac(minyr:maxyr), drop = FALSE] < Refpts[Refpts$stk == x,"Blim"]
  }, simplify = "array")
  
  ## define dimension names
  names(dimnames(res))[7] <- "stk"
  dimnames(res)$stk       <- names(om$stks)
  
  ## transform array to dataframe
  res <- as.data.frame.table(res)
  names(res)[names(res) == "Freq"] <- "risk"
  
  ## (optional) filter for specific stocks
  if(!is.null(stknames)) {
    res <- res[res$stk %in% stknames,]
  }
  
  ## coerce "year" and "iter" to numeric
  res$year <- as.numeric(as.character(res$year))
  res$iter <- as.numeric(as.character(res$iter))
  
  ## summarise to a single value
  temp  <- aggregate(res, risk ~ age + year + unit + season + area + stk, FUN = "sum")
  temp2 <- aggregate(res, risk ~ age + year + unit + season + area + stk, FUN = "length")
  temp[,"risk"] <- temp[,"risk"]/temp2[,"risk"]
  
  return(temp)
  
}
