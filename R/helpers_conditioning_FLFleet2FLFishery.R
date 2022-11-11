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
#' \code{FLCatches} by averaging aross metiers.
#'
#' @param fleets
#'
#' @return object of class \code{FLFisheries}

FLFleet2FLFishery <- function(fleets, na_replace = 0) {

  # HOW TO HANDLE THIS WHEN FLFISHERY::FLCATCH CONTAINS ONLY NUMBERS AND WEIGHTS
  # INFORMATION?

  # HOW TO HANDLE NA CASES FOR WTS, PRICE, CATCH.SEL?? THESE ARE NOT TRUE ZEROES
  # BY DEFAULT, I SHOULD REMOVE NAs FROM THE AVERAGING.

  # ---------------------------------#
  # 1. Some checks
  #
  # 2.  Process data (R version)
  # 2.1 ... Loop over each unique catch
  # 2.2 ... Loop over each metier that catches this stock
  # 2.2.1 ... Create new FLFishery::FLCatch
  # 2.2.1.1 ... Track non-zero effort-share for weights and prices
  # 2.2.1.2 ... Age-structured stocks
  # 2.2.1.3 ... Biomass-structured stocks
  # 2.2.2 ... Append to existing FLFishery::FLCatch
  # 2.2.2.1 ... Update non-zero effort-share for weights and prices
  # 2.2.2.2 ... Age-structured stocks
  # 2.2.2.3 ... Biomass-structured stocks
  # ---------------------------------#

  # ===========================================================================#
  # SECTION 1: Some checks
  # ===========================================================================#

  ## is class FLFleets?


  # ===========================================================================#
  # SECTION 2: Process data (R version)
  # ===========================================================================#

  # suppress following message:
  # Found more than one class "FLCatch" in cache; using the first, from namespace 'FLFleet'
  # Also defined by ‘FLFishery’

  ## Slow R-version
  fisheries <- suppressMessages(FLFishery::FLFisheries(lapply(fleets@names, function(x){

    ## extract x'th fleet
    fleet_x <- fleets[[x]]

    ## extract list of harvested stocks over all metiers
    stk_list <- sapply(fleet_x@metiers@names, function(y) {
      fleet_x@metiers[[y]]@catches@names
      },simplify = TRUE)

    ## extract list of corresponding metier names for each harvested stock
    mtr_list <- sapply(fleet_x@metiers@names, function(y) {
      rep(y, length(fleet_x@metiers[[y]]@catches@names))
    },simplify = TRUE)

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

      # -------------------------------------------------------------------#
      # SECTION 2.2: Loop over each metier that catches this stock
      # -------------------------------------------------------------------#

      for(z in mtr_vector[stk_vector == y]) {

        # -------------------------------------------------------------------#
        # SECTION 2.2.1: Create new FLFishery::FLCatch
        # -------------------------------------------------------------------#

        if(!exists("newcatch_y")) {

          ## extract FLFleets catch
          catch_y <- fleet_x@metiers[[z]]@catches[[y]]

          ## extract metier effort proportional effort-share
          effshare_z <- fleet_x@metiers[[z]]@effshare

          ## extract dimensions
          catch_dim <- dimnames(catch_y)

          # This is an important check. We want to make sure that the age dimension
          # is identical across each of the slots in the object. If there are
          # differences, this could cause problems later on.

          ## CHECK - dimnames match slots
          if(any(catch_dim$age != dimnames(catch_y@landings)$age))
            stop(paste0("For fleet ", x, ", metier ", z,", and stock ", y,
                        ": 'landings' age dimensions do not match overall 'FLCatch' dimensions"))
          if(any(catch_dim$age != dimnames(catch_y@landings.n)$age))
            stop(paste0("For fleet ", x, ", metier ", z,", and stock ", y,
                        ": 'landings.n' age dimensions do not match overall 'FLCatch' dimensions"))
          if(any(catch_dim$age != dimnames(catch_y@landings.wt)$age))
            stop(paste0("For fleet ", x, ", metier ", z,", and stock ", y,
                        ": 'landings.wt' age dimensions do not match overall 'FLCatch' dimensions"))
          if(any(catch_dim$age != dimnames(catch_y@landings.sel)$age))
            stop(paste0("For fleet ", x, ", metier ", z,", and stock ", y,
                        ": 'landings.sel' age dimensions do not match overall 'FLCatch' dimensions"))
          if(any(catch_dim$age != dimnames(catch_y@discards)$age))
            stop(paste0("For fleet ", x, ", metier ", z,", and stock ", y,
                        ": 'discards' age dimensions do not match overall 'FLCatch' dimensions"))
          if(any(catch_dim$age != dimnames(catch_y@discards.n)$age))
            stop(paste0("For fleet ", x, ", metier ", z,", and stock ", y,
                        ": 'discards.n' age dimensions do not match overall 'FLCatch' dimensions"))
          if(any(catch_dim$age != dimnames(catch_y@discards.wt)$age))
            stop(paste0("For fleet ", x, ", metier ", z,", and stock ", y,
                        ": 'discards.wt' age dimensions do not match overall 'FLCatch' dimensions"))
          if(any(catch_dim$age != dimnames(catch_y@discards.sel)$age))
            stop(paste0("For fleet ", x, ", metier ", z,", and stock ", y,
                        ": 'discards.sel' age dimensions do not match overall 'FLCatch' dimensions"))
          if(any(catch_dim$age != dimnames(catch_y@price)$age))
            stop(paste0("For fleet ", x, ", metier ", z,", and stock ", y,
                        ": 'price' age dimensions do not match overall 'FLCatch' dimensions"))

          ## Create an empty FLQuant as template
          catch_quant <- FLCore::FLQuant(NA, dimnames = list(age    = catch_dim$age,
                                                     year   = catch_dim$year,
                                                     unit   = catch_dim$unit,
                                                     season = catch_dim$season,
                                                     area   = catch_dim$area,
                                                     iter   = catch_dim$iter))

          ## Create new FLFishery catch
          newcatch_y <- FLFishery::FLCatch(catch_quant)

          ## expand FLPar slot
          newcatch_y@catch.q <- FLCore::expand(newcatch_y@catch.q, iter = catch_dim$iter)

          # SECTION 2.2.1.1 Extract data from slots and handle NAs
          # -------------------------------------------------------------------#

          # If a weight is zero/NA but numbers exist, throw an error
          Ln <- catch_y@landings.n;  Ln[is.na(Ln)] <- na_replace
          Lw <- catch_y@landings.wt; Lw[is.na(Lw)] <- na_replace

          if(any(Ln > 0 & Lw == 0))
            stop(paste0("For fleet ", x, ", metier ", z,", and stock ", y,
                        ": 'landings.wt' is NA or 0 when 'landings.n' > 0"))

          Dn <- catch_y@discards.n;  Dn[is.na(Dn)] <- na_replace
          Dw <- catch_y@discards.wt; Dw[is.na(Dw)] <- na_replace

          if(any(Dn > 0 &  Dw == 0))
            stop(paste0("For fleet ", x, ", metier ", z,", and stock ", y,
                        ": 'discards.wt' is NA or 0 when 'discards.n' > 0"))

          Price <- catch_y@price; Price[is.na(Price)] <- na_replace

          if(any(Ln > 0 & Price == 0))
            stop(paste0("For fleet ", x, ", metier ", z,", and stock ", y,
                        ": 'price' is NA or 0 when 'landings.n' > 0"))


          # SECTION 2.2.1.2 Track non-zero effort-share for weights and prices
          # -------------------------------------------------------------------#

          # To handle cases where NAs exist in weights and prices, we weight
          # these properties using the total non-NA effort-share.

          ## expand efforts-hare to cover all ages
          effshare_price <- effshare_Dwts <- effshare_Lwts  <- expand(effshare_z,
                                                                      age = catch_dim$age)

          # Create index to track location of NA or zero data
          effshare_Lwts[Lw == 0] <- 0
          effshare_Dwts[Dw == 0] <- 0
          effshare_price[Price == 0] <- 0

          effshare_Lwts_sum <- effshare_Lwts
          effshare_Dwts_sum <- effshare_Dwts
          effshare_price_sum <- effshare_price

          # SECTION 2.2.1.3: Age-structured stocks
          # -------------------------------------------------------------------#

          if(!all(is.na(catch_y@landings.n))) {

            ## fill slots
            newcatch_y@landings.n <- Ln
            newcatch_y@discards.n <- Dn

            # Here, we should only consider existing data - zeroes and missing data
            # do not have much meaning and should be omitted from the calculation.
            #
            # To inflate the weights proportionally, we need to cumulatively sum the
            # effort share FLQuant, keeping NA cases zero. We can then divide by the
            # summed object to scale the resulting summaries to the correct value.
            #
            # We will need to track each of the following four properties
            # independently

            ## take a weighted average for properties
            newcatch_y@landings.wt <- FLCore::FLQuant(sweep(Lw, c(2:6), effshare_z, "*"))
            newcatch_y@discards.wt <- FLCore::FLQuant(sweep(Dw, c(2:6), effshare_z, "*"))
            newcatch_y@catch.sel   <- FLCore::FLQuant(sweep(FLCore::catch.sel(catch_y), c(2:6), effshare_z, "*"))
            newcatch_y@price       <- FLCore::FLQuant(sweep(Price, c(2:6), effshare_z, "*"))

          }

          # -------------------------------------------------------------------#
          # SECTION 2.2.1.4: Biomass-structured stocks
          # -------------------------------------------------------------------#

          ## data is annual stock biomass - numbers and weights are empty
          if(all(is.na(catch_y@landings.n)) & all(is.na(catch_y@landings.wt)) &
             !all(is.na(catch_y@landings))) {

            ## fill slots
            newcatch_y@landings.n  <- catch_y@landings
            newcatch_y@landings.wt[] <- 1

            newcatch_y@discards.n  <- catch_y@discards
            newcatch_y@discards.wt[]  <- 1

            newcatch_y@catch.sel[]   <- 1

            ## take a weighted average
            newcatch_y@price       <- FLCore::FLQuant(sweep(Price, c(2:6), effshare_z, "*"))

          }

          ## Handle NAs in landings/discards
          newcatch_y@landings.n[is.na(newcatch_y@landings.n)] <- na_replace
          newcatch_y@discards.n[is.na(newcatch_y@discards.n)] <- na_replace

          # Catch.q is the primary way we express aggregated fleet behaviour. Hence,
          # zeros represent true zero catchability and should be incorporated into
          # the calculation.

          ## Handle NAs in catch.q
          FLCore::catch.q(catch_y)[is.na(FLCore::catch.q(catch_y))] <- na_replace

          ## catch.q does not contain year dimension - save original as attribute
          attr(newcatch_y, "catchq")   <- FLCore::catch.q(catch_y) %*%  effshare_z
          newcatch_y@catch.q["alpha",] <- FLCore::yearMeans(attr(newcatch_y, "catchq"), 3) # take average of final 3 years of data

        } else {

        # -------------------------------------------------------------------#
        # SECTION 2.2.2: Append to existing FLFishery::FLCatch
        # -------------------------------------------------------------------#

          ## extract FLFleets catch
          catch_y <- fleet_x@metiers[[z]]@catches[[y]]

          ## extract metier effort proportional effortshare
          effshare_z <- fleet_x@metiers[[z]]@effshare

          ## extract dimensions
          catch_dim <- dimnames(catch_y)

          # Check if age dimensions match - expand age dimension if possible
          #
          # First ensure that if there is a mismatch in age dimensions, we are
          # not trying to combine age-disaggregate and age-aggregated data.

          if(catch_dim$age != dimnames(newcatch_y)$age) {
            if(any(catch_dim$age == "all") | any(dimnames(newcatch_y)$age == "all")){
              stop(paste0("For fleet ", x, " and stock ", y,
                          ", mismatch in age dimensions across metiers and some metiers aggregate ages into 'all'"))
            }

            newcatch_y <- FLCore::expand(newcatch_y, age = unique(c(catch_dim$age,dimnames(newcatch_y)$age)))

            ## fill expanded NA slots with zero to prepare for additions
            newcatch_y@landings.n[is.na(newcatch_y@landings.n)] <- 0
            newcatch_y@discards.n[is.na(newcatch_y@discards.n)] <- 0
            newcatch_y@landings.wt[is.na(newcatch_y@landings.wt)] <- 0
            newcatch_y@discards.wt[is.na(newcatch_y@discards.wt)] <- 0

            newcatch_y@catch.sel[is.na(newcatch_y@catch.sel)] <- 0
            newcatch_y@price[is.na(catch_dim$age)]            <- 0

            ## expand effort-share indices
            effshare_Lwts_sum  <- FLCore::expand(effshare_Lwts_sum,
                                                 age = unique(c(catch_dim$age,dimnames(newcatch_y)$age)))
            effshare_Dwts_sum  <- FLCore::expand(effshare_Dwts_sum,
                                                 age = unique(c(catch_dim$age,dimnames(newcatch_y)$age)))
            effshare_price_sum <- FLCore::expand(effshare_price_sum,
                                                 age = unique(c(catch_dim$age,dimnames(newcatch_y)$age)))

            effshare_Lwts_sum[is.na(effshare_Lwts_sum)]   <- 0
            effshare_Dwts_sum[is.na(effshare_Dwts_sum)]   <- 0
            effshare_price_sum[is.na(effshare_price_sum)] <- 0

          }
          # SECTION 2.2.2.1 Extract data from slots and handle NAs
          # -------------------------------------------------------------------#

          # If a weight is zero/NA but numbers exist, throw an error
          Ln <- catch_y@landings.n;  Ln[is.na(Ln)] <- 0
          Lw <- catch_y@landings.wt; Lw[is.na(Lw)] <- 0

          if(any(Ln > 0 & Lw == 0))
            stop(paste0("For fleet ", x, ", metier ", z,", and stock ", y,
                        ": 'landings.wt' is NA or 0 when 'landings.n' > 0"))

          Dn <- catch_y@discards.n;  Dn[is.na(Dn)] <- 0
          Dw <- catch_y@discards.wt; Dw[is.na(Dw)] <- 0

          if(any(Dn > 0 &  Dw == 0))
            stop(paste0("For fleet ", x, ", metier ", z,", and stock ", y,
                        ": 'discards.wt' is NA or 0 when 'discards.n' > 0"))

          Price <- catch_y@price; Price[is.na(Price)] <- 0

          if(any(Ln > 0 & Price == 0))
            stop(paste0("For fleet ", x, ", metier ", z,", and stock ", y,
                        ": 'price' is NA or 0 when 'landings.n' > 0"))

          # SECTION 2.2.2.2: Update non-zero effort-share for weights and prices
          # -------------------------------------------------------------------#

          ## expand efforts-hare to cover all ages
          effshare_price <- effshare_Dwts <- effshare_Lwts  <- expand(effshare_z,
                                                                      age = catch_dim$age)

          # Create index to track location of NA or zero data
          effshare_Lwts[Lw == 0] <- 0
          effshare_Dwts[Dw == 0] <- 0
          effshare_price[Price == 0] <- 0

          effshare_Lwts_sum  <- effshare_Lwts_sum  + effshare_Lwts
          effshare_Dwts_sum  <- effshare_Dwts_sum  + effshare_Dwts
          effshare_price_sum <- effshare_price_sum + effshare_price

          # -------------------------------------------------------------------#
          # SECTION 2.2.2.3 Age-structured stocks
          # -------------------------------------------------------------------#

          if(!all(is.na(catch_y@landings.n))) {

            ## Handle NAs in landings/discards
            catch_y@landings.n[is.na(catch_y@landings.n)] <- na_replace
            catch_y@discards.n[is.na(catch_y@discards.n)] <- na_replace
            catch_y@landings[is.na(catch_y@landings)] <- na_replace
            catch_y@discards[is.na(catch_y@discards)] <- na_replace

            ## fill slots
            newcatch_y@landings.n[catch_dim$age,] <- newcatch_y@landings.n[catch_dim$age,] + catch_y@landings.n
            newcatch_y@discards.n[catch_dim$age,] <- newcatch_y@discards.n[catch_dim$age,] + catch_y@discards.n

            ## take a weighted average for properties
            newcatch_y@landings.wt[catch_dim$age,] <- newcatch_y@landings.wt[catch_dim$age,] + FLCore::FLQuant(sweep(catch_y@landings.wt, c(2:6), effshare_z, "*"))
            newcatch_y@discards.wt[catch_dim$age,] <- newcatch_y@discards.wt[catch_dim$age,] + FLCore::FLQuant(sweep(catch_y@discards.wt, c(2:6), effshare_z, "*"))
            newcatch_y@catch.sel[catch_dim$age,]   <- newcatch_y@catch.sel[catch_dim$age, ]  + FLCore::FLQuant(sweep(FLCore::catch.sel(catch_y), c(2:6), effshare_z, "*"))
            newcatch_y@price[catch_dim$age,]       <- newcatch_y@price[catch_dim$age,]       + FLCore::FLQuant(sweep(catch_y@price, c(2:6), effshare_z, "*"))

          }

          # -------------------------------------------------------------------#
          # SECTION 2.2.2.4: Biomass-structured stocks
          # -------------------------------------------------------------------#

          ## data is annual stock biomass - numbers and weights are empty
          if(all(is.na(catch_y@landings.n)) & all(is.na(catch_y@landings.wt)) &
             !all(is.na(catch_y@landings))) {

            ## Handle NAs in landings/discards
            catch_y@landings.n[is.na(catch_y@landings.n)] <- na_replace
            catch_y@discards.n[is.na(catch_y@discards.n)] <- na_replace
            catch_y@landings[is.na(catch_y@landings)] <- na_replace
            catch_y@discards[is.na(catch_y@discards)] <- na_replace

            ## fill slots
            newcatch_y@landings.n[catch_dim$age,] <- newcatch_y@landings.n[catch_dim$age,] + catch_y@landings
            newcatch_y@discards.n[catch_dim$age,] <- newcatch_y@discards.n[catch_dim$age,] + catch_y@discards

            ## take a weighted average for properties
            newcatch_y@price[catch_dim$age,] <- newcatch_y@price[catch_dim$age,] + FLCore::FLQuant(sweep(catch_y@price, c(2:6), effshare_z, "*"))

          }

          ## Handle NAs in catch.q
          FLCore::catch.q(catch_y)[is.na(FLCore::catch.q(catch_y))] <- na_replace

          ## catch.q does not contain year dimension - save original as attribute
          attr(newcatch_y, "catchq")   <- attr(newcatch_y, "catchq") + FLCore::catch.q(catch_y) %*%  effshare_z
          newcatch_y@catch.q["alpha",] <- FLCore::yearMeans(attr(newcatch_y, "catchq"), 3) # take average of final 3 years of data

        } # end if exists("newcatch_y")
      } # end loop over metier

      ## Re-scale weighted mean landings/discards weights & prices using non-zero
      ## effort-share

      if(any(effshare_Lwts_sum > 0)) {
        newcatch_y@landings.wt[effshare_Lwts_sum > 0] <- sweep(newcatch_y@landings.wt[effshare_Lwts_sum > 0], c(1:6),
                                                               effshare_Lwts_sum[effshare_Lwts_sum > 0], "/")
      }
      if(any(effshare_Dwts_sum > 0)) {
        newcatch_y@discards.wt[effshare_Dwts_sum > 0] <- sweep(newcatch_y@discards.wt[effshare_Dwts_sum > 0], c(1:6),
                                                               effshare_Dwts_sum[effshare_Dwts_sum > 0], "/")
      }
      if(any(effshare_price_sum > 0)) {
        newcatch_y@price[effshare_price_sum > 0]      <- sweep(newcatch_y@price[effshare_price_sum > 0], c(1:6),
                                                               effshare_price_sum[effshare_price_sum > 0], "/")
      }

      return(newcatch_y)
    })))
    names(catches) <- unique(stk_vector)

    # suppress following message:
    # Found more than one class "FLCatch" in cache; using the first, from namespace 'FLFleet'
    # Also defined by ‘FLFishery’

    ## Embed catches in FLFishery object
    fishery <- suppressMessages(FLFishery::FLFishery(catches))

    ## calculate weighted average for variable costs
    fishery_vcost <- sapply(fleet_x@metiers@names, function(zz) {
      fleet_x@metiers[[zz]]@effshare %*% fleet_x@metiers[[zz]]@vcost
    }, simplify = "array")


    ## Fill FLFishery slots
    fishery@capacity  <- fleet_x@capacity
    fishery@effort    <- fleet_x@effort
    fishery@vcost[]   <- apply(fishery_vcost, c(1:6), sum)
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
