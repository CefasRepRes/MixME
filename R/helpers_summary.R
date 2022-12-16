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
  
  res <- sapply(names(om$stks), function(x){
    catch_fleets <- sapply(names(om$flts), function(y){
      if(!is.null(om$flts[[y]][[x]])) {
        catch(om$flts[[y]][[x]])[,ac(minyr:maxyr),drop = FALSE]
      } else {
        FLQuant(0, dimnames = list(year = ac(minyr:maxyr),
                                   iter = dimnames(om$flts[[y]])$iter))
      }
    }, simplify = "array")
    apply(catch_fleets, c(1:6), sum)
  }, simplify = "array")
  
  ## define dimension names
  names(dimnames(res))[7] <- "stk"
  dimnames(res)$stk       <- names(om$stks)
  
  ## transform array to dataframe
  res <- as.data.frame.table(res)
  names(res)[names(res) == "Freq"] <- "catch"
  
  ## (optional) filter for specific stocks
  if(!is.null(stknames)) {
    res <- res[res$stk %in% stknames,]
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
                                 stknames = NULL) {
  
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
  
  ## (optional) filter for specific years
  if(!is.null(minyr)) {
    res <- res[res$year >= minyr,]
  }
  if(!is.null(maxyr)) {
    res <- res[res$year <= maxyr,]
  }
  
  summary_uptake <- aggregate(res, cbind(quota, uptake) ~ year + stk + iter, sum)
  summary_uptake$uptake_percentage <- ((summary_uptake$quota - summary_uptake$uptake)/summary_uptake$quota) * 100
  
  return(summary_uptake[,c("stk","year","iter","uptake_percentage")])
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
    f <- ((flc@catch.q[1, ] %*% quantSums(b@n * b@wt)^(-1 * flc@catch.q[2,])) %*% flf@effort) %*% flc@catch.sel
    return(f)
  }
  
  # -------------------------------------------------
  # calculate summary quantity and correct dimensions
  # -------------------------------------------------
  
  ## get vector of stock names and fleets catching these
  stklist <- sapply(om$flts, function(y) {
    names(y)
  },simplify = TRUE)
  
  fltlist <- sapply(names(om$flts), function(y) {
    rep(y, length(om$flts[[y]]))
  },simplify = TRUE)
  
  stkvector <- unlist(stklist, use.names = FALSE)
  fltvector <- unlist(fltlist, use.names = FALSE)
  
  fage <- Reduce("+", Map(getf, 
                          fn = fltvector[stkvector == y][1], 
                          cn = stkvector[stkvector == y][1],
                          bn = stkvector[stkvector == y][1],
                          op = om))
  
  res <- sapply(1:length(om$stks), function(x){
    ## First generate a blank object to store results
    fa <- getf(op = om, fn = 1, cn = x, bn = x)
    fa[] <- 0
    ## Loop over fleets and calculate partial F for stock
    for(y in 1:length(om$flts)){
      fx <- getf(op = om, fn = y, cn = x, bn = x)
      fa <- fx + fa
    }
    
    age_range <- args$frange[[x]]["minfbar"]:args$frange[[x]]["maxfbar"]
    fbar <- apply(fa[ac(age_range), ], 2:6, mean)
    return(fbar[,ac(minyr:maxyr),drop = TRUE])
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
    f <- ((flc@catch.q[1, ] %*% quantSums(b@n * b@wt)^(-1 * flc@catch.q[2,])) %*% flf@effort) %*% flc@catch.sel
    return(f)
  }
  
  # -------------------------------------------------
  # calculate summary quantity and correct dimensions
  # -------------------------------------------------
  
  ## Calculate summary array
  res <- sapply(om$stks@names, function(x){
    ff <- sapply(om$flts@names, function(y){
      ## First generate a blank object to store results
      fa <- getf(op = om, fn = y, cn = x, bn = x)
      fa[drop = TRUE]
    }, simplify = "array", USE.NAMES = TRUE)
    apply(ff, c(1:3), sum)
  }, simplify = "array", USE.NAMES = TRUE)
  
  ## define dimension names
  names(dimnames(res))[7] <- "stk"
  dimnames(res)$stk       <- names(om$stks)
  
  # -------------------------------------------------
  # optional filter and return result
  # -------------------------------------------------
  
  ## transform array to dataframe
  res <- as.data.frame.table(res)
  names(res)[names(res) == "Freq"] <- "f"
  
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