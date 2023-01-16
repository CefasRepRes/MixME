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
#'
#' @return object of class \code{FLFisheries}
#' 
#' @export

FLFleet2FLFishery <- function(fleets, na_replace = 0, verbose = TRUE) {
  
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
          stop(paste0("For metier ", name(x), ", and stock ", y,
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
                                    catch.q = FLPar(alpha = FLCore::yearMeans(Cq), beta = 0))
      
      ## Attach full catchability as attribute
      attr(catch_y, "catchq")   <- Cq
      
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
    fishery_vcost <- Reduce("+", Map(function(x) {x@effshare %*% x@vcost},
                            fleet_x@metiers))
    
    ## Fill FLFishery slots
    fishery@capacity  <- fleet_x@capacity
    fishery@effort    <- fleet_x@effort
    fishery@vcost[]   <- fishery_vcost
    fishery@fcost     <- fleet_x@fcost
    # fishery@crewshare <- fleet_x@crewshare
    
    ## Additional slots
    # fishery@orevenue  <- ..
    # fishery@hperiod   <- ..
    
    return(fishery)
    
  })))
  names(fisheries) <- fleets@names
  
  return(fisheries)
}
