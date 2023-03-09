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
  
  res <- sapply(om$flts@names, function(x) {
    effort(om$flts[[x]])[,ac(minyr:maxyr), drop = FALSE]
  }, simplify = "array")
  
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
  
  ## Generate 8D array
  res <- sapply(names(om$stks), function(x){
    catch_fleets <- sapply(names(om$flts), function(y){
      if(!is.null(om$flts[[y]][[x]])) {
        catch(om$flts[[y]][[x]])[,ac(minyr:maxyr),drop = FALSE]
      } else {
        FLQuant(0, dimnames = list(year = ac(minyr:maxyr),
                                   iter = dimnames(om$flts[[y]])$iter))
      }
    }, simplify = "array")}, 
    simplify = "array")
  
  ## (Optional) aggregate over fleets
  if(byfleet == TRUE) {
    names(dimnames(res))[7] <- "flt"
    dimnames(res)$flt       <- names(om$flts)

    names(dimnames(res))[8] <- "stk"
    dimnames(res)$stk       <- names(om$stks)
  }
  
  if(byfleet == FALSE) {
    res <- apply(res, c(1:6,8), sum)
    
    names(dimnames(res))[7] <- "stk"
    dimnames(res)$stk       <- names(om$stks)
  }
  
  ## transform array to dataframe
  res <- as.data.frame.table(res)
  names(res)[names(res) == "Freq"] <- "catch"
  
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
    res$uptake_percentage[res$quota == 0 & res$uptake == 0] <- 100
    
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
  
  ## define function to calculate fishing mortality
  getf <- function(op, fn = 1, cn = 1, bn = 1) {
    # f = alpha * sel * effort
    flf <- op[["flts"]][[fn]]
    flc <- flf[[cn]]
    b <- op[["stks"]][[bn]]
    f <- ((flc@catch.q[1, ] * quantSums(b@n * b@wt)^(-1 * flc@catch.q[2,])) %*% flf@effort) %*% flc@catch.sel
    return(f)
  }
  
  # -------------------------------------------------
  # calculate summary quantity and correct dimensions
  # -------------------------------------------------
  
  ## get vector of stock names and fleets catching these
  stklist <- sapply(om$flts, function(y) {
    names(y)
  },simplify = FALSE)
  
  fltlist <- sapply(names(om$flts), function(y) {
    rep(y, length(om$flts[[y]]))
  },simplify = FALSE)
  
  stkvector <- unlist(stklist, use.names = FALSE)
  fltvector <- unlist(fltlist, use.names = FALSE)
  
  res <- sapply(unique(stkvector), function(y){
    fage <- Reduce("+", Map(getf, 
                            fn = fltvector[stkvector == y], 
                            cn = stkvector[stkvector == y],
                            bn = stkvector[stkvector == y],
                            op = list(om)))
    
    age_range <- args$frange[[y]]["minfbar"]:args$frange[[y]]["maxfbar"]
    fbar <- FLCore::quantMeans(fage[ac(age_range), ])
    return(fbar[,ac(minyr:maxyr),,,,])
  }, simplify = "array")
  
  ## define dimension names
  names(dimnames(res))[7] <- "stk"
  dimnames(res)$stk       <- names(om$stks)
  
  # -------------------------------------------------
  # optional filter and return result
  # -------------------------------------------------
  
  ## transform array to dataframe
  res <- as.data.frame.table(res)
  names(res)[names(res) == "Freq"] <- "fbar"
  
  ## (optional) filter for specific stocks
  if(!is.null(stknames)) {
    res <- res[res$stk %in% stknames,]
  }

  ## coerce "year" and "iter" to numeric
  res$year <- as.numeric(as.character(res$year))
  res$iter <- as.numeric(as.character(res$iter))
  
  ## (optional) filter for specific years
  if(!is.null(minyr)) {
    res <- res[res$year > minyr,]
  }
  if(!is.null(maxyr)) {
    res <- res[res$year < maxyr,]
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
  
  ## define function to calculate fishing mortality
  getf <- function(op, fn = 1, cn = 1, bn = 1) {
    # f = alpha * sel * effort
    flf <- op[["flts"]][[fn]]
    flc <- flf[[cn]]
    b <- op[["stks"]][[bn]]
    f <- ((flc@catch.q[1, ] * quantSums(b@n * b@wt)^(-1 * flc@catch.q[2,])) %*% flf@effort) %*% flc@catch.sel
    return(f)
  }
  
  # -------------------------------------------------
  # calculate summary quantity and correct dimensions
  # -------------------------------------------------
  
  ## get vector of stock names and fleets catching these
  stklist <- sapply(om$flts, function(y) {
    names(y)
  },simplify = FALSE)
  
  fltlist <- sapply(names(om$flts), function(y) {
    rep(y, length(om$flts[[y]]))
  },simplify = FALSE)
  
  stkvector <- unlist(stklist, use.names = FALSE)
  fltvector <- unlist(fltlist, use.names = FALSE)
  
  res <- lapply(unique(stkvector), function(y){
    fage <- Reduce("+", Map(getf, 
                            fn = fltvector[stkvector == y], 
                            cn = stkvector[stkvector == y],
                            bn = stkvector[stkvector == y],
                            op = list(om)))
    
    ## transform array to dataframe
    out <- as.data.frame.table(fage[,ac(minyr:maxyr)])
    out$stk <- y
    
    return(out)
  })
  
  res <- do.call(rbind, res)
  
  # -------------------------------------------------
  # optional filter and return result
  # -------------------------------------------------
  
  ## rename result
  names(res)[names(res) == "Freq"] <- "f"
  
  ## (optional) filter for specific stocks
  if(!is.null(stknames)) {
    res <- res[res$stk %in% stknames,]
  }
  
  ## coerce "year" and "iter" to numeric
  res$year <- as.numeric(as.character(res$year))
  res$iter <- as.numeric(as.character(res$iter))
  
  return(res)
  
}