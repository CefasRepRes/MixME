# ---
# title: 'Operating Model - Observation error model'
# author: 'Matthew Pace'
# date: 'September 2022 (modified January 2024)'
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
  #  II ...... Use oem observation structure (FLBiol & FLFishery) [NOT YET IMPLEMENTED]
  # III ...... Use OM structure (FLStock)
  #  IV ...... Use OM structure (FLBiols & FLFisheries)           [OBSOLETE]
  # 2.2 ... Populate observation slots
  #   I ...... FLStock 
  #     ............ perfect observations
  #     ............ observations with uncertainty 
  #  II ...... FLBiols & FLFisheries
  #     ............ perfect observations
  #     ............ observations with uncertainty 
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
  
  if(!is.null(observations$stk) & is.null(observations$flt)) {
    
    ## use observations object
    stk0 <- observations$stk[[x]]
    
    ## Extract overall landings and discards for this stock
    fltlandings <- getC(om$flts, x, sl = "landings.n", FALSE)
    fltdiscards <- getC(om$flts, x, sl = "discards.n", FALSE)
    
    ## Extract overall catch numbers-at-age
    fltcatchn <- fltlandings + fltdiscards
    
  } ## END if use oem FLStock
  
  # ---------------------------------------------------------------#
  # (OPTION II) Use oem observation structure (FLBiol & FLFishery) #
  # ---------------------------------------------------------------#
  
  if(!is.null(observations$stk) & !is.null(observations$flt)) {
    
    stop("In 'oemMixME': fleet observations not currently implemented")
    
    ## use observations object
    stk0 <- observations$stk[[x]]
    flt0 <- lapply(observations$flt, "[[", x)
    
  } ## END if use oem FLBiols 
  
  # ----------------------------------------#
  # (OPTION III) Use OM structure (FLStock) #
  # ----------------------------------------#
  
  # For simplicity, if an observation error model structure is not supplied,
  # then I will return an FLStock for each stock supplied - this should be
  # valid for 99% of users
  
  ## N.B. MixME does not allow stk to be FLStocks. Only FLBiols allowed
  ## N.B. Option III is now replaced by Option IV
  ## N.B. Option IV has issues. Reverting to Option III.
  
  if(is.null(observations$stk)) {
    # stop("In 'oemMixME': Automatic definition of stock structure not yet implemented")
    
    ## coerce to FLStock
    stk0 <- as(om$stks[[x]],"FLStock")
    units(stk0)$harvest <- "f"
    
    ## Extract fleet landings and discards for this stock
    fltlandings <- getC(om$flts, x, sl = "landings.n", FALSE)
    fltdiscards <- getC(om$flts, x, sl = "discards.n", FALSE)
    
    ## Extract overall catch numbers-at-age
    fltcatchn <- fltlandings + fltdiscards
    
    ## Extract fleet landings and discards weights for this stock 
    fltlandingswts <- getC(om$flts, x, sl = "landings.wt", FALSE)
    fltdiscardswts <- getC(om$flts, x, sl = "discards.wt", FALSE)
    
    ## Calculate fleet-weighted average for landings and discards weights
    LF <- sweep(fltlandings, c(1:6), apply(fltlandings, c(1:6), sum), "/")
    fltlandingswts0 <- apply(fltlandingswts * LF, c(1:6), sum)
    
    DF <- sweep(fltdiscards, c(1:6), apply(fltdiscards, c(1:6), sum), "/")
    fltdiscardswts0 <- apply(fltdiscardswts * DF, c(1:6), sum)
    
    CF <- sweep(apply(fltlandings, c(1:6), sum), c(1:6), apply(fltcatchn, c(1:6), sum), "/")
    LW0 <- fltlandingswts0 * CF
    LW0[is.na(LW0)] <- 0
    DW0 <- fltdiscardswts0 * (1-CF)
    DW0[is.na(DW0)] <- 0
    fltcatchwts0 <- LW0+DW0
    
    stk0@landings.wt[] <- fltlandingswts0 
    stk0@discards.wt[] <- fltdiscardswts0
    stk0@catch.wt[]    <- fltcatchwts0
    
    ## update Fbar range
    range(stk0)["minfbar"] <- args$frange[[x]]["minfbar"]
    range(stk0)["maxfbar"] <- args$frange[[x]]["maxfbar"]
    
  } ## END if use om FLStock
  
  # -----------------------------------------------------#
  # (OPTION IV) Use OM structure (FLBiols & FLFisheries) #
  # -----------------------------------------------------#
  
  # This replaces Option III. If an observation error model structure is not
  # supplied, then I return an FLBiols and FLFisheries. This allows for much better
  # flexibility when evaluating mixed fisheries management. It is then up to the
  # user to define functions that handle these objects.
  
  # if(use_stk_oem[x] == FALSE) {
  #   
  #   stk0 <- om$stks[[x]]
  #   flt0 <- om$flts
  #   
  #   ## remove stock numbers
  #   stk0@n[] <- NA
  # }
  
  # ----------------------------------------#
  # SECTION 2.2: Populate observation slots #
  # ----------------------------------------#
  #
  # In this next section, we populate the missing observation slots in the
  # stock structure. This differs depending on whether the observation
  # structure is an FLStock or an FLBiol/FLFisheries.
  #
  # ------------------- #
  # (Option I) FLStock  #
  # ------------------- #
  
  if (class(stk0)[1] == "FLStock") {
    
    # If prefect observation
    # --------------------------------#
    
    if(use_om_weights[x] == TRUE) {
      
      ## Find total discards
      stk0discards <- apply(fltdiscards, c(1:6), sum)
      
      ## calculate updated weighted mean discards weights based on operating model
      # fltdiscardwts <- sapply(fltcatchesnames, function(y){
      #   
      #   ## calculate the proportional fleet contribution to overall discards
      #   fltdiscardprop <- sweep(fltcatches[[y]]@discards.n, 
      #                           c(1:6), 
      #                           stk0discards, 
      #                           "/") 
      #   ## handle cases where no fleets discard
      #   fltdiscardprop[is.nan(fltdiscardprop)] <- 0
      #   
      #   ## calculate weighted contribution of fleet to discards weights
      #   fltdiscardprop * fltcatches[[y]]@discards.wt
      # }, simplify = "array")
      fltdiscardwts_all <- getC(om$flts, x,"discards.wt", FALSE)
      fltdiscardprop    <- sweep(fltdiscards, 1:6, apply(fltdiscards,1:6,sum),"/")
      fltdiscardwts_all[is.nan(fltdiscardwts_all)] <- 0
      fltdiscardprop[is.nan(fltdiscardprop)]       <- 0
      fltdiscardwts <- apply(fltdiscardprop * fltdiscardwts_all, 1:6, sum)
      
      stk0@discards.wt[] <- apply(fltdiscardwts, c(1:6), sum, na.rm = TRUE)
      
      ## Find updated discards fraction
      stk0discfrac <- stk0discards / apply(fltcatchn, c(1:6), sum)
      
      ## calculate updated weighted mean catch weights
      stk0@catch.wt[] <- (stk0@discards.wt * stk0discfrac) + (stk0@landings.wt * (1 - stk0discfrac))
      
    } ## END if use OM weights
    
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
      flt0landfrac[fltcatchn == 0] <- 0
      
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
  } ## END if FLStock
  
  # --------------------------------- #
  # (Option II) FLBiol / FLFisheries  #
  # --------------------------------- #
  
  if (class(stk0)[1] == "FLBiol") {
  }
  
  # ---------------------------------#
  # SECTION 2.3: Trim to data period #
  # ---------------------------------#
  
  # In this next section, we trim the data objects to the period for which we have
  # catch or survey data. This is important because subsequent modules will take
  # the final year in the object to mean the latest data year.
  
  # We need to separate the methods applied to (1) FLStock and (2) FLBiol and FLFisheries.
  # For simplicity, I define separate functions that handle these two approaches.
  
  if(class(stk0) == "FLStock") {
    stk0 <- oemTrimFLStock(stk0, x, ay, idx_timing, catch_timing)
    flt0 <- NULL
  }
  
  if(class(stk0) == "FLBiol") {
    stkflt0 <- oemTrimFLBiol(stk0, flt0, x, ay, idx_timing, catch_timing)
    stk0 <- stkflt0$stk
    flt0 <- stkflt0$flt
  }
  
  # -------------------------------------#
  # SECTION 2.4: Observed survey indices #
  # -------------------------------------#
  
  if(!is.null(observations$idx[[x]])) {
    
    ## calculate index observations based on updated values in OM
    observations$idx[[x]] <- calculateSurvey(stk = om$stk[[x]], 
                                             flt = om$flt,
                                             idx = observations$idx[[x]],
                                             use_fastF = args$use_fastF)
    
    ## use observed survey indices
    idx0 <- observations$idx[[x]]
    
    ## should index uncertainty be added?
    if(use_idx_residuals[x] == TRUE) {
      
      ## loop over each survey index
      idx0 <- lapply(seq_along(idx0), function(idx_i) {
        idx_tmp <- idx0[[idx_i]]
        index(idx_tmp) <- index(idx_tmp) * deviances$idx[[x]][[idx_i]]
        return(idx_tmp)
      })
      
      idx0 <- FLCore::FLIndices(idx0)
      names(idx0) <- names(observations$idx[[x]])
    }
    
    ### Check if the fix below causes problems during stock estimation
    
    ## Trim index to match stock object
    idx0 <- FLCore::FLIndices(lapply(seq_along(idx0), function(y) {
      window(idx0[[y]], end = ay + idx_timing[[x]][y])
    }))
    names(idx0) <- names(observations$idx[[x]])
    
    # idx0 <- window(idx0, end = ay + max(max(idx_timing[[x]]), max(catch_timing[[x]])))
    
  } else {
    
    idx0 <- NULL
  }
  
  return(list(stk = stk0,
              flt = flt0,
              idx = idx0,
              tracking = tracking[[x]]$stk))
}

