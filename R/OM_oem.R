# ---
# title: 'Operating Model - Observation error model'
# author: 'Matthew Pace'
# date: 'September 2022'
# ---
#
#' Implementation of stock observation methods
#'
#' Function to generate stock observations (fleet catches and survey indices)
#' with (optional) uncertainty.
#'
#' @param om The operating model representing the true state and dynamics of 
#'           the system. A named list comprising stock information (\code{FLStocks}) 
#'           and fishing fleet information (\code{FLFisheries}).
#' @param deviances Environmental noise in stock catch and survey data for the 
#'                  historical and projection periods. A nested named list 
#'                  with the elements \code{stk} and \code{idx}. See details.
#' @param observations The structure of stock catch and survey data used to
#'                     estimate stock status.
#'                     A nested named list with the elements \code{stk} 
#'                     and \code{idx}.
#' @param args List of additional arguments
#' @param tracking A named list of tracking objects to monitor emergent dynamic
#'                 properties
#' @param catch_timing A named list of catch timing relative to assessment year
#' @param idx_timing A named list of survey index timing relative to assessment year
#' @param use_stk_oem Logical vector. Length \code{n} stocks. Should a supplied
#'                    Observation Error Model structure be used?
#' @param use_catch_residuals Logical vector. Length \code{n} stocks. Should
#'                            catch residuals be used to generate uncertainty?
#' @param use_idx_residuals Logical vector. Length \code{n} stocks. Should
#'                          survey index residuals be used to generate uncertainty?
#' @param use_om_weights Logical vector. Length \code{n} stocks. Should stock
#'                       catch, landings and discard individual mean weights be 
#'                       updated with data from OM? Over-quota discards may 
#'                       result in changes to discard and overall catch weights. 
#'                       Recommend \code{TRUE} if attempting perfect stock 
#'                       observations.
#'
#' @return A named list of stock observations, survey indices and updated tracking
#'         object
#'
#' @export

oemMixME <- function(x,
                     om,
                     deviances,
                     observations,
                     args,
                     tracking,
                     catch_timing = NULL, # catch timing relative to ay
                     idx_timing   = NULL, # index timing relative to ay
                     use_stk_oem         = FALSE,
                     use_catch_residuals = FALSE,
                     use_idx_residuals   = FALSE,
                     use_om_weights      = FALSE,
                     ...) {
  
  # --------------------------------#
  # 2. Iterative stock observation
  # 2.1 ... Observed stock catches
  #   I ...... Use oem observation structure (FLStock) 
  #     ......... If perfect observations
  #     ......... If observations with uncertainty
  #  II ...... Use oem observation structure (FLBiol & FLFishery)
  # III ...... Use OM structure (FLStock)
  # 2.2 ... Trim to data period
  # 2.3 ... Observed survey indices
  # --------------------------------#
  
  ## extract timings
  iy   <- args$iy             # initial projection year
  ay   <- args$ay             # current (assessment) year
  mlag <- args$management_lag # management lag
  
  # ------------------------------------#
  # SECTION 2.1: Observed stock catches #
  # ---------------------------------------------------#
  # (OPTION I) Use oem observation structure (FLStock) #
  # ---------------------------------------------------#
  # This allows for different biological parameters
  # (weights, maturity, mortality)
  
  if(use_stk_oem[x] == TRUE & is.null(observations$flt)) {
    
    ## use observations object
    stk0 <- observations$stk[[x]]
    
    ## Extract all catches for this stock
    fltcatches <- lapply(om$flts, "[[", x)
    
    ## Extract names with catches
    fltcatchesnames <- names(fltcatches)[!sapply(fltcatches, is.null)]
    
    ## Calculate overall landings and discards
    fltlandings <- sapply(fltcatchesnames, function(y){
      fltcatches[[y]]@landings.n           
    }, simplify = "array", USE.NAMES = TRUE)
    
    fltdiscards <- sapply(fltcatchesnames, function(y){
      fltcatches[[y]]@discards.n
    }, simplify = "array")
    
    ## Extract overall catch numbers-at-age
    fltcatchn <- sapply(fltcatchesnames, function(y){
      catch.n(fltcatches[[y]])
    }, simplify = "array", USE.NAMES = TRUE)
    
    # If perfect observations
    # -----------------------#
    
    if(use_om_weights[x] == TRUE) {
      
      ## Find total discards
      stk0discards <- apply(fltdiscards, c(1:6), sum)
      
      ## calculate updated weighted mean discards weights based on operating model
      fltdiscardwts <- sapply(fltcatchesnames, function(y){
        
        ## calculate the proportional fleet contribution to overall discards
        fltdiscardprop <- sweep(fltcatches[[y]]@discards.n, 
                                c(1:6), 
                                stk0discards, 
                                "/") 
        ## handle cases where no fleets discard
        fltdiscardprop[is.nan(fltdiscardprop)] <- 0
        
        ## calculate weighted contribution of fleet to discards weights
        fltdiscardprop * fltcatches[[y]]@discards.wt
      }, simplify = "array")
      
      stk0@discards.wt[] <- apply(fltdiscardwts, c(1:6), sum, na.rm = TRUE)
      
      ## Find updated discards fraction
      stk0discfrac <- stk0discards / apply(fltcatchn, c(1:6), sum)
      
      ## calculate updated weighted mean catch weights
      stk0@catch.wt[] <- (stk0@discards.wt * stk0discfrac) + (stk0@landings.wt * (1 - stk0discfrac))
      
    }
    
    if(use_catch_residuals[x] == FALSE){
      
      ## Sum over fleets
      stk0landings <- apply(fltlandings, c(1:6), sum)
      stk0discards <- apply(fltdiscards, c(1:6), sum)
      
      ## update landings, discards and catch numbers in observed stock object
      stk0@landings.n[] <- stk0landings
      stk0@discards.n[] <- stk0discards
      stk0@catch.n      <- stk0@landings.n + stk0@discards.n
    }
    
    # If observations with uncertainty
    # --------------------------------#
    
    if(use_catch_residuals[x] == TRUE){
      
      # The catch residuals are assumed to be available for each fleet
      # in the form of a 7-dimensional matrix [age, year, ..., iter, fleet]
      
      if(length(dim(deviances$stk[[x]])) != 7)
        stop(paste0("In 'oemMixME': For stock ",x,", catch residuals must be a 7-D array with the dimensions: age, year, unit, season, area, iter, and fleet"))
      
      ## Implement catch numbers-at-age uncertainty
      flt0catchn <- fltcatchn * deviances$stk[[x]]
      
      ## Calculate landings fraction
      flt0landfrac <- sweep(fltlandings, c(1:7), fltcatchn, "/")
      
      ### split catch into discards and landings, based on landing fraction
      flt0landings <- sweep(flt0catchn, c(1:7), flt0landfrac, "*")
      flt0discards <- sweep(flt0catchn, c(1:7), (1 - flt0landfrac), "*")
      
      ## Sum over fleets
      stk0landings <- apply(flt0landings, c(1:6), sum)
      stk0discards <- apply(flt0discards, c(1:6), sum)
      
      ## update landings, discards and catch numbers in observed stock object
      landings.n(stk0)[] <- stk0landings
      discards.n(stk0)[] <- stk0discards
      stk0@catch.n      <- stk0@landings.n + stk0@discards.n
      
    }
    
    ## update total landings, discards and catch weights
    landings(stk0)  <- computeLandings(stk0)
    discards(stk0)  <- computeDiscards(stk0)
    catch(stk0)     <- computeCatch(stk0)
    
    # Update overall mean weights based on catch-numbers-weighted mean
    # ----------------------------------------------------------------#
    # catch.wt(stk0)
    # landings.wt(stk0)
    # discards.wt(stk0)
    
    # Update tracking object
    # -------------------------#
    tracking[[x]]$stk["C.obs", ac(iy:ay)] <- catch(stk0)[,ac(iy:ay)]
    tracking[[x]]$stk["L.obs", ac(iy:ay)] <- landings(stk0)[,ac(iy:ay)]
    tracking[[x]]$stk["D.obs", ac(iy:ay)] <- discards(stk0)[,ac(iy:ay)]
  }
  
  # ---------------------------------------------------------------#
  # (OPTION II) Use oem observation structure (FLBiol & FLFishery) #
  # ---------------------------------------------------------------#
  
  if(use_stk_oem[x] == TRUE & !is.null(observations$flt)) {
    
    stop("In 'oemMixME': fleet observations not currently implemented")
    
    ## use observations object
    stk0 <- observations$stk[[x]]
    flt0 <- lapply(observations$flt, "[[", x)
    
  }
  
  # ----------------------------------------#
  # (OPTION III) Use OM structure (FLStock) #
  # ----------------------------------------#
  
  # For simplicity, if an observation error model structure is not supplied,
  # then I will return an FLStock for each stock supplied - this should be
  # valid for 99% of users
  
  if(use_stk_oem[x] == FALSE) {
    stop("In 'oemMixME': Automatic definition of stock structure not yet implemented")
    
    if(class(om$stks[[x]]) == "FLStock") {
      
      stk0 <- om$stks[[x]]
      flt0 <- om$flts
      
    } else if(class(om$stks[[x]]) == "FLBiol") {
      
      # I DON'T WANT TO COERCE TO FLSTOCK... I SHOULD USE FLBIOLS AND FLFISHERIES
      # INSTEAD
      
      ## coerce to FLStock
      stk0 <- as(om$stks[[x]],"FLStock")
      
      ## populate missing slots with data from stock object in tracking
      units(stk0) <- units(tracking[[x]][["stk"]])
      
      ## Fill missing slots
      stk0@landings.n  <- tracking[[x]][["stk"]]@landings.n
      stk0@landings.wt <- tracking[[x]][["stk"]]@landings.wt
      stk0@discards.n  <- tracking[[x]][["stk"]]@discards.n
      stk0@discards.wt <- tracking[[x]][["stk"]]@discards.wt
      stk0@catch.n  <- tracking[[x]][["stk"]]@catch.n
      stk0@catch.wt <- tracking[[x]][["stk"]]@catch.wt
      stk0@harvest  <- tracking[[x]][["stk"]]@harvest
      
      ## update Fbar range
      range(stk0)["minfbar"] <- range(tracking[[x]][["stk"]])["minfbar"]
      range(stk0)["maxfbar"] <- range(tracking[[x]][["stk"]])["maxfbar"]
      
      ## compute properties
      stock(stk0)    <- computeStock(stk0)
      landings(stk0) <- computeLandings(stk0)
      discards(stk0) <- computeDiscards(stk0)
      catch(stk0)    <- computeCatch(stk0)
      
      ## truncate to min data year
      minyr <- dims(stk0@stock[!is.na(stk0@stock)])$minyear # min year where data exists
      stk0  <- window(stk0, start = minyr)
      
      ## Generate SR of correct dimensions
      sr0        <- as.FLSR(stk0, model = om1$stks[[x]]@rec@model)
      sr0@rec    <- rec(stk0)
      sr0@ssb    <- ssb(stk0)
      sr0@params <- om1$stks[[x]]@rec@params
    }
  }
  
  # ---------------------------------#
  # SECTION 2.2: Trim to data period #
  # ---------------------------------#
  
  ## We probably want to remove years preceding the data period - find mimumum data year
  checkCatch <- iterSums(catch(stk0) > 0)
  checkCatch[is.na(checkCatch)] <- 0
  mindatayr <- dims(stk0[,checkCatch > 0])$minyear
  
  ## If survey data is more recent than catch data, then trim to survey year
  if(max(idx_timing[[x]]) > max(catch_timing[[x]])) {
    
    ## Trim stock object
    stk0 <- window(stk0, start = mindatayr, end = ay + max(idx_timing[[x]]))
    
    ## Remove data in years where no catch data is available
    yrs_remove <- (ay + catch_timing[[x]] + 1):ay
    
    catch(stk0)[, ac(yrs_remove)]       <- NA
    catch.n(stk0)[, ac(yrs_remove)]     <- NA
    catch.wt(stk0)[, ac(yrs_remove)]    <- NA
    landings(stk0)[, ac(yrs_remove)]    <- NA
    landings.n(stk0)[, ac(yrs_remove)]  <- NA
    landings.wt(stk0)[, ac(yrs_remove)] <- NA
    discards(stk0)[, ac(yrs_remove)]    <- NA
    discards.n(stk0)[, ac(yrs_remove)]  <- NA
    discards.wt(stk0)[, ac(yrs_remove)] <- NA
    
  } else {
    
    stk0 <- window(stk0, start = mindatayr, end = ay + max(catch_timing[[x]]))
    
  }
  
  # -------------------------------------#
  # SECTION 2.3: Observed survey indices #
  # -------------------------------------#
  
  if(!is.null(observations$idx[[x]])) {
    
    ## calculate index observations based on updated values in OM
    observations$idx[[x]] <- calculateSurvey(stk = om$stk[[x]], 
                                             flt = om$flt,
                                             idx = observations$idx[[x]])
    
    ## use observed survey indices
    idx0 <- observations$idx[[x]]
    
    ## should index uncertainty be added?
    if(use_idx_residuals[x] == TRUE) {
      
      ## loop over each survey index
      idx0 <- FLCore::FLIndices(lapply(seq_along(idx0), function(idx_i) {
        idx_tmp <- idx0[[idx_i]]
        index(idx_tmp) <- index(idx_tmp) * deviances$idx[[x]][[idx_i]]
        return(idx_tmp)
      }))
    }
    
    ## Trim index to match stock object
    idx0 <- window(idx0, end = ay + max(max(idx_timing[[x]]), max(catch_timing[[x]])))
    
  } else {
    
    idx0 <- NULL
  }
  
  return(list(stk = stk0,
              idx = idx0,
              tracking = tracking[[x]]$stk))
}

