# ---
# title: 'Convert FLFleets object into FLFisheries object'
# author: 'Matthew Pace'
# date: 'November 2022'
# ---
#
#' Convert \code{FLFleets} class object into \code{FLFisheries} class object
#'
#' Function converts \code{FLFleets} objects with a nested \code{FLMetiers} and
#' \code{FLCatches} hierarchy into \code{FLFisheries} objects with nested
#' \code{FLCatches} by averaging across metiers.
#'
#' @param fleets An object of class \code{FLFleets}
#' @param na_replace Value used to replace instances of \code{NA}. Defaults to
#'                   0.
#' @param verbose Should progress be printed to screen? Defaults to \code{TRUE}.
#' @param verbose If the métier dimension should be kept and transfered to the area dimension (5th dim) in
#' in the FLQuants.
#'
#' @return object of class \code{FLFisheries}
#' 
#' @export

FLFleet2FLFishery <- function(fleets, na_replace = 0, verbose = TRUE, keep_metier = FALSE) {
  
  # HOW TO HANDLE THIS WHEN FLFISHERY::FLCATCH CONTAINS ONLY NUMBERS AND WEIGHTS
  # INFORMATION?
  
  # HOW TO HANDLE NA CASES FOR WTS, PRICE, CATCH.SEL?? THESE ARE NOT TRUE ZEROES
  # BY DEFAULT, I SHOULD REMOVE NAs FROM THE AVERAGING.
  
  # ---------------------------------#
  # 1. Some checks
  #
  # 2.  Process data (R version)
  # 2.1 ... Loop over each unique catch
  # 2.2 ... Age-structured stocks
  # 2.3 ... Biomass-structured stocks
  # ---------------------------------#
  
  # ===========================================================================#
  # SECTION 1: Some checks
  # ===========================================================================#
  
  ## is class FLFleets?
  
  ## is class FLFLeetsExt (FLBEIA)
  
  # ===========================================================================#
  # SECTION 2: Process data (R version)
  # ===========================================================================#
  
  # suppress following message:
  # Found more than one class "FLCatch" in cache; using the first, from namespace 'FLFleet'
  # Also defined by ‘FLFishery’
  
  ## Slow R-version
  fisheries <- suppressMessages(FLFishery::FLFisheries(lapply(fleets@names, function(x){
    
    ## print fleet name
    if(verbose) cat("\n", x, "\n")
    
    ## extract x'th fleet
    fleet_x <- fleets[[x]]
    
    ## extract list of harvested stocks over all metiers
    stk_list <- sapply(fleet_x@metiers@names, function(y) {
      fleet_x@metiers[[y]]@catches@names
    },simplify = FALSE)
    
    ## extract list of corresponding metier names for each harvested stock
    mtr_list <- sapply(fleet_x@metiers@names, function(y) {
      rep(y, length(fleet_x@metiers[[y]]@catches@names))
    },simplify = FALSE)
    
    stk_vector <- unlist(stk_list, use.names = FALSE)
    mtr_vector <- unlist(mtr_list, use.names = FALSE)
    
    # -------------------------------------------------------------------------#
    # SECTION 2.1: Loop over each unique catch
    # -------------------------------------------------------------------------#
    
    # loop over each stock and aggregate corresponding information from
    # each metier
    
    # suppress the following message:
    # Found more than one class "FLCatch" in cache; using the first, from namespace 'FLFleet'
    # Also defined by ‘FLFishery’

    catches <- suppressMessages(FLFishery::FLCatches(lapply(unique(stk_vector), function(y){
      
      ## Extract dimensions of all catches for the y'th stock
      
      
      # -------------------------------------------------------------------#
      # SECTION 2.2: Age-structured stocks
      # -------------------------------------------------------------------#
      
      ## define functions to sum over landings and discards numbers
      sumland <- function(x, y) {
        Lxy <- landings.n(x@catches[[y]])
        Lxy[is.na(Lxy)] <- na_replace
        return(Lxy)
      }
      
      sumdisc <- function(x, y) {
        Dxy <- discards.n(x@catches[[y]])
        Dxy[is.na(Dxy)] <- na_replace
        return(Dxy)
      }
      
      ## define functions to calculate weighted average of landings/discards
      ## weights and prices
      
      avglandwt <- function(x, y) {
        Lnxy <- landings.n(x@catches[[y]])
        Lwxy <- landings.wt(x@catches[[y]])
        
        Lnxy[is.na(Lnxy)] <- na_replace
        Lwxy[is.na(Lwxy)] <- na_replace
        
        if(any(Lnxy > 0 & Lwxy == 0))
          stop(paste0("For metier ", name(x), ", and stock ", y,
                      ": 'landings.wt' is NA or 0 when 'landings.n' > 0"))
        
        return(Lwxy * Lnxy / Ln)
      }
      
      avgdiscwt <- function(x, y) {
        Dnxy <- discards.n(x@catches[[y]])
        Dwxy <- discards.wt(x@catches[[y]])
        
        Dnxy[is.na(Dnxy)] <- na_replace
        Dwxy[is.na(Dwxy)] <- na_replace
        
        if(any(Dnxy > 0 & Dwxy == 0))
          stop(paste0("For metier ", name(x), ", and stock ", y,
                      ": 'discards.wt' is NA or 0 when 'discards.n' > 0"))
        
        return(Dwxy * Dnxy / Dn)
      }
      
      avgprice  <- function(x, y) {
        Lnxy  <- landings.n(x@catches[[y]])
        prxy <- price(x@catches[[y]])
        
        Lnxy[is.na(Lnxy)] <- na_replace
        prxy[is.na(prxy)] <- na_replace
        
        if(any(Lnxy > 0 & prxy == 0))
          warning(paste0("For metier ", name(x), ", and stock ", y,
                      ": 'price' is NA or 0 when 'landings.n' > 0"))
        
        return(prxy * Lnxy / Ln)
      }
      
      ## define functions to calculate weighted average of catch selection
      avgsel <- function(x, y) {
        csxy <- FLCore::catch.sel(x@catches[[y]])
        cqxy <- FLCore::catch.q(x@catches[[y]])
        effx <- x@effshare
        
        cqxy[is.na(cqxy)] <- na_replace
        
        if(dims(csxy)$age == dims(cqxy)$age) {
          
          ## If catch.q is age-disaggregated, can assume that selection is 1.
          if(any(is.na(csxy))) {
            csxy[is.na(csxy)] <- 1
          }
          ## calculate proportional catchability... replace retulting div by zero 
          ## NAs with zero
          cqprop <- sweep(cqxy, c(2:6), apply(cqxy, c(2:6), sum), "/")
          cqprop[is.na(cqprop)] <- 0
          
          ## update selection-at-age
          csxy <- csxy * cqprop
          return(FLCore::FLQuant(sweep(csxy, c(2:6), effx, "*")))
        }
        
        if(dims(csxy)$age != dims(cqxy)$age) {
          if(any(is.na(csxy))) {
            stop(paste0("For metier ", name(x), ", and stock ", y,
                        ": NA in 'landings.sel' or 'discards.sel'"))
          }
          
          return(FLCore::FLQuant(sweep(csxy, c(2:6), effx, "*")))
        }
      }
      
      ## define function to calculate weighted average of catchability
      avgcq <- function(x, y) {
        csxy <- FLCore::catch.sel(x@catches[[y]])
        cqxy <- FLCore::catch.q(x@catches[[y]])
        effx <- x@effshare
        
        cqxy[is.na(cqxy)] <- na_replace
        
        if(dims(csxy)$age == dims(cqxy)$age & dims(cqxy)$age > 1) {
          cqxy <- FLCore::quantSums(cqxy)
        }
        return(cqxy * effx)
      }
      
      if(!isTRUE(keep_metier)) {
      ## Sum catches, landings and discards numbers
      Ln <- Reduce("+", Map(sumland, 
                            fleet_x@metiers[mtr_vector[stk_vector == y]], 
                            stk_vector[stk_vector == y]))
      Dn <- Reduce("+", Map(sumdisc, 
                            fleet_x@metiers[mtr_vector[stk_vector == y]],
                            stk_vector[stk_vector == y]))
      
      ## Weighted-average landings and discards weights and prices
      Lw <- Reduce("+", Map(avglandwt,
                            fleet_x@metiers[mtr_vector[stk_vector == y]],
                            stk_vector[stk_vector == y]))
      Dw <- Reduce("+", Map(avgdiscwt,
                            fleet_x@metiers[mtr_vector[stk_vector == y]],
                            stk_vector[stk_vector == y]))
      Price <- Reduce("+", Map(avgprice,
                               fleet_x@metiers[mtr_vector[stk_vector == y]],
                               stk_vector[stk_vector == y]))
      
      ## weighted-average catch selection-at-age
      Cs <- Reduce("+", Map(avgsel,
                            fleet_x@metiers[mtr_vector[stk_vector == y]],
                            stk_vector[stk_vector == y]))
      
      ## weighted-average catchability
      Cq <- Reduce("+", Map(avgcq,
                            fleet_x@metiers[mtr_vector[stk_vector == y]],
                            stk_vector[stk_vector == y]))
      
      }
      
      ## Here we want to convert the métier dimension into an area dimension
      if(isTRUE(keep_metier)) {
        
        ## Extract the dimnames for the catch object, and add the area dimnames
        new_dimnames     <- dimnames(fleet_x@metiers[mtr_vector[stk_vector == y]][[1]]@catches[[y]])
        new_dimnames$area <- fleet_x@metiers@names
        
        ## Landings numbers
        Ln <- FLQuant(NA, dimnames = new_dimnames)
        Ln_met <- lapply(fleet_x@metiers[mtr_vector[stk_vector == y]], function(x) x@catches[[y]]@landings.n)
        for(i in dimnames(Ln)$area) {
          if(i %in% names(Ln_met)) {
          Ln[,,,,i,] <- Ln_met[[i]]
          }
        }
        units(Ln) <- units(Ln_met[[1]])
      
        ## Discards numbers
        Dn <- FLQuant(NA, dimnames = new_dimnames)
        Dn_met <- lapply(fleet_x@metiers[mtr_vector[stk_vector == y]], function(x) x@catches[[y]]@discards.n)
        for(i in dimnames(Ln)$area) {
          if(i %in% names(Dn_met)) {
          Dn[,,,,i,] <- Dn_met[[i]]
          }
        }
        units(Dn) <- units(Dn_met[[1]])
        
       ## Landings wts
       Lw <- FLQuant(NA, dimnames = new_dimnames)
       Lw_met <- lapply(fleet_x@metiers[mtr_vector[stk_vector == y]], function(x) x@catches[[y]]@landings.wt)
       for(i in dimnames(Lw)$area) {
         if(i %in% names(Lw_met)) {
         Lw[,,,,i,] <- Lw_met[[i]]
         }
       }
       units(Lw) <- units(Lw_met[[1]])
        
       ## Discards wts
       Dw <- FLQuant(NA, dimnames = new_dimnames)
       Dw_met <- lapply(fleet_x@metiers[mtr_vector[stk_vector == y]], function(x) x@catches[[y]]@discards.wt)
       for(i in dimnames(Dw)$area) {
         if(i %in% names(Dw_met)) {
         Dw[,,,,i,] <- Dw_met[[i]]
         }
       }
       units(Dw) <- units(Dw_met[[1]])
       
       ## Price
       Price <- FLQuant(NA, dimnames = new_dimnames)
       Price_met <- lapply(fleet_x@metiers[mtr_vector[stk_vector == y]], function(x) x@catches[[y]]@price)
       for(i in dimnames(Price)$area) {
         if(i %in% names(Price_met)) {
         Price[,,,,i,] <- Price_met[[i]]
         }
         }
       units(Price) <- units(Price_met[[1]])
       
       ## Selectivity
       Cs <- FLQuant(NA, dimnames = new_dimnames)
       Csel <- lapply(fleet_x@metiers[mtr_vector[stk_vector == y]], function(x) {
         sweep(x@catches[[y]]@catch.q,c(2:6), apply(x@catches[[y]]@catch.q, c(2:6),sum),"/")}
       )
       for(i in dimnames(Cs)$area) {
         if(i %in% names(Csel)) {
         Cs[,,,,i,] <- Csel[[i]]
         }
       }
       units(Cs) <- units(Csel[[1]])
        
       ## Catchability
       Cqmt <- lapply(fleet_x@metiers[mtr_vector[stk_vector == y]], function(x) {
        apply(x@catches[[y]]@catch.q, c(2:6),sum)})
       newdimnames <- dimnames(Cqmt[[1]])
       newdimnames$area <- fleet_x@metiers@names
       Cq <- FLQuant(NA, dimnames = newdimnames)
       
       for(i in dimnames(Cq)$area) {
         if(i %in% names(Cqmt)) {
         Cq[,,,,i,] <- Cqmt[[i]]
         }
       }
       units(Cq) <- units(Cqmt[[1]])
        
      }
      
      
      # -------------------------------------------------------------------#
      # SECTION 2.2: Biomass-aggregated stocks
      # -------------------------------------------------------------------#
      
      #############################
      
      ## insert summaries into FLCatch object
      
      catch_y <- FLFishery::FLCatch(name = y,
                                    landings.n = Ln,
                                    discards.n = Dn,
                                    landings.wt = Lw,
                                    discards.wt = Dw,
                                    price = Price,
                                    catch.sel = Cs,
                                    catch.q = FLPar(0, 
                                                    dimnames=list(params=c('alpha','beta'), 
                                                                  year = dimnames(Ln)$year,
                                                                  area = dimnames(Ln)$area,
                                                                  iter = dimnames(Ln)$iter),
                                                    units='NA'))
      
      ## Insert full catchability time-series
      catch_y@catch.q["alpha",] <- Cq

        return(catch_y)
      
    })))
    

    
    names(catches) <- unique(stk_vector)
    
    # suppress following message:
    # Found more than one class "FLCatch" in cache; using the first, from namespace 'FLFleet'
    # Also defined by ‘FLFishery’
    
    ## Embed catches in FLFishery object
    fishery <- suppressMessages(FLFishery::FLFishery(catches))
    
    ## NOTE I NEED TO UPDATE THE Stock NAMES IN FISHERY TOO!!!
    
    ## calculate weighted average for variable costs
    if(!isTRUE(keep_metier)){
    fishery_vcost <- Reduce("+", Map(function(x) {x@effshare %*% x@vcost},
                                     fleet_x@metiers))
    
    ## Fill FLFishery slots
    fishery@capacity  <- fleet_x@capacity
    fishery@effort    <- fleet_x@effort
    fishery@vcost[]   <- fishery_vcost
    fishery@fcost     <- fleet_x@fcost
    }
    
  if(isTRUE(keep_metier)){
    ## Fill FLFishery slots
    
    newdimnames <- list(year = dimnames(fishery)$year,
                        area = dimnames(fishery)$area,
                        iter = dimnames(fishery)$iter)
      
    ## Variable costs
    Vc <- FLQuant(NA, dimnames = newdimnames)
    for(i in 1:length(fleet_x@metiers)) {
      Vc[,,,,i,] <- fleet_x@metiers[[i]]@vcost
    }
    units(Vc) <- units(fleet_x@metiers[[1]]@vcost)  
    
    # fixed costs
    Fc <- FLQuant(NA, dimnames = newdimnames)
    for(i in 1:length(fleet_x@metiers)) {
      Fc[,,,,i,] <- fleet_x@fcost/length(newdimnames$area)
    }
    units(Fc) <- units(fleet_x@fcost)
    
    # capacity
    Cap <- FLQuant(NA, dimnames = newdimnames)
    for(i in 1:length(fleet_x@metiers)) {
      Cap[,,,,i,] <- fleet_x@capacity/length(newdimnames$area)
    }
    units(Cap) <- units(fleet_x@capacity)
  
    ## Effort should be per area
    Ef <- FLQuant(NA, dimnames = newdimnames)
    
    for(i in 1:length(fleet_x@metiers)) {
      Ef[,,,,i,] <- fleet_x@effort * fleet_x@metiers[[i]]@effshare
    }
    units(Ef) <- units(fleet_x@effort)
    
    
    
    fishery@capacity  <- Cap
    fishery@effort    <- Ef
    fishery@vcost     <- Vc
    fishery@fcost     <- Fc
    
    }
    
    
    
    # fishery@crewshare <- fleet_x@crewshare
    
    ## Additional slots
    # fishery@orevenue  <- ..
    # fishery@hperiod   <- ..
    
    return(fishery)
    
  })))
  names(fisheries) <- fleets@names
  
  return(fisheries)
}