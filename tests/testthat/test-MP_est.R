## ============================================================================#
## Test user-supplied (FLBiol, FLFisheries) estimation method
## ============================================================================#

test_that("estMixME single-stock (FLBiol) estimation works", {
  
  ## Generate token OM & OEM
  ## --------------------------------------------------------------------------#
  flq <- FLCore::FLQuant(1, 
                         dim = c(2,10,1,1,1,1), 
                         dimnames = list(age  = c("1","2"),
                                         year = c("1","2","3","4","5","6","7","8","9","10")))
  flt <- FLFishery::FLFishery(FLFishery::FLCatch(flq))
  om  <- list(stks = FLCore::FLBiols(FLCore::FLBiol(flq)),
              flts = FLFishery::FLFisheries(list(flt)))
  
  ## Generate names
  names(om$stks) <- "1"
  om$stks$`1`@name <- "1"
  names(om$flts) <- "A"
  names(om$flts$A) <- "1"
  
  ## Fill necessary slots
  om$stks$`1`@n[] <- 1
  om$stks$`1`@m[] <- 1
  om$flts$A$`1`@landings.n[]  <- 1
  om$flts$A$`1`@landings.wt[] <- 1
  om$flts$A$`1`@discards.n[]  <- 1
  om$flts$A$`1`@discards.wt[] <- 1
  om$flts$A$`1`@catch.q <- FLPar(1, 
                                 dimnames=list(params=c('alpha','beta'), 
                                               year = dimnames(flq)$year, 
                                               iter = dimnames(flq)$iter),
                                 units='NA')
  om$flts$A$`1`@catch.sel[] <- 1
  om$flts$A@effort[] <- 1
  attr(om$flts$A$`1`,"quotashare") <- quantSums(flq)/2
  
  ## Copy to observation error model
  oem <- om
  oem$stks$`1`@n[] <- NA
  oem$flts$A@effort[] <- NA
  oem$flts$A$`1`@catch.q[] <- NA
  oem$flts$A$`1`@catch.sel[] <- NA
  
  ## Generate token stock assessment model & other arguments
  ## --------------------------------------------------------------------------#
  
  estfun <- function(stk, tracking, ...) {
    stk@n[] <- 2
    
    tracking$stk["B.est",] <- 2
    tracking$stk["SB.est",] <- 2
    
    ## estimate final data year
    dy <- dims(stk)$maxyear
    
    ## Store estimated properties
    if (dy %in% dimnames(stk_est$tracking$stk)$year) {
      tracking$stk["F.est", ac(dy)]  <- FLCore::fbar(stk0)[,ac(dy)]
      tracking$stk["B.est", ac(dy)]  <- FLCore::stock(stk0)[,ac(dy)]
      tracking$stk["SB.est", ac(dy)] <- FLCore::ssb(stk0)[,ac(dy)]
      
      tracking$stk["C.est", ac(dy)] <- FLCore::catch(stk0)[,ac(dy)]
      tracking$stk["L.est", ac(dy)] <- FLCore::landings(stk0)[,ac(dy)]
      tracking$stk["D.est", ac(dy)] <- FLCore::discards(stk0)[,ac(dy)]
      
      tracking$sel_est[,ac(dy)] <- sweep(FLCore::harvest(stk0)[,ac(dy)], 
                                         c(2:6), fbar(stk0)[,ac(dy)], "/")
    }
    
    return(list(stk0 = stk,
                tracking = tracking))
  }
  
  ## arguments
  args <- list(ay = 10,
               iy = 9,
               management_lag = 1)
  estmethod <- list("1" = estfun)
  tracking  <- makeTracking(om, c("9","10"))
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  debug(estMixME)
  out <- estMixME(x = "1",
                  stk = oem$stks,
                  flt = oem$flts,
                  idx = NULL,
                  ctrl = NULL,
                  om = om,
                  args = args,
                  estmethod = estmethod,
                  tracking = tracking,
                  fitList = NULL,
                  fwdList = NULL)
  
  ## Check performance
  ## --------------------------------------------------------------------------#
  
  ## check structure
  ## check dimensions - age
  ## check dimensions - year
  ## check content
  
})

## ============================================================================#
## Test user-supplied (FLStocks) estimation method
## ============================================================================#

test_that("estMixME single-stock (FLStocks) estimation works", {
  
  ## Generate token OM & OEM
  ## --------------------------------------------------------------------------#
  flq <- FLCore::FLQuant(1, 
                         dim = c(3,10,1,1,1,1), 
                         dimnames = list(age  = c("1","2","3"),
                                         year = c("1","2","3","4","5","6","7","8","9","10")))
  flt <- FLFishery::FLFishery(FLFishery::FLCatch(flq))
  om  <- list(stks = FLCore::FLBiols(FLCore::FLBiol(flq)),
              flts = FLFishery::FLFisheries(list(flt)))
  
  ## Generate names
  names(om$stks) <- "1"
  om$stks$`1`@name <- "1"
  names(om$flts) <- "A"
  names(om$flts$A) <- "1"
  
  ## Fill necessary slots
  om$stks$`1`@n[] <- 1
  om$stks$`1`@m[] <- 1
  om$flts$A$`1`@landings.n[]  <- 1
  om$flts$A$`1`@landings.wt[] <- 1
  om$flts$A$`1`@discards.n[]  <- 1
  om$flts$A$`1`@discards.wt[] <- 1
  om$flts$A$`1`@catch.q <- FLPar(1, 
                                 dimnames=list(params=c('alpha','beta'), 
                                               year = dimnames(flq)$year, 
                                               iter = dimnames(flq)$iter),
                                 units='NA')
  om$flts$A$`1`@catch.sel[] <- 1
  om$flts$A@effort[] <- 1
  attr(om$flts$A$`1`,"quotashare") <- quantSums(flq)/2
  
  ## Copy to observation error model
  oem <- om
  # undebug(as.FLStock, signature = signature(object = "FLBiol"))
  oem$stks <- FLStocks("1" = as.FLStock(oem$stks$`1`, oem$flts))
  
  oem$stks$`1`@stock.n[] <- NA
  oem$stks$`1`@harvest[] <- NA
  
  ## Generate token stock assessment model & other arguments
  ## --------------------------------------------------------------------------#
  
  estfun <- function(stk, tracking, ...) {
    stk@stock.n[] <- 2
    stk@harvest[] <- 3
    
    ## estimate final data year
    dy <- dims(stk)$maxyear
    
    ## Store estimated properties
    if (dy %in% dimnames(tracking$stk)$year) {
      tracking$stk["F.est", ac(dy)]  <- FLCore::fbar(stk)[,ac(dy)]
      tracking$stk["B.est", ac(dy)]  <- FLCore::stock(stk)[,ac(dy)]
      tracking$stk["SB.est", ac(dy)] <- FLCore::ssb(stk)[,ac(dy)]
      
      tracking$stk["C.est", ac(dy)] <- FLCore::catch(stk)[,ac(dy)]
      tracking$stk["L.est", ac(dy)] <- FLCore::landings(stk)[,ac(dy)]
      tracking$stk["D.est", ac(dy)] <- FLCore::discards(stk)[,ac(dy)]
      
      tracking$sel_est[,ac(dy)] <- sweep(FLCore::harvest(stk)[,ac(dy)], 
                                         c(2:6), fbar(stk)[,ac(dy)], "/")
    }
    
    return(list(stk0 = stk,
                tracking = tracking))
  }
  
  ## arguments
  args <- list(ay = 10,
               iy = 9,
               management_lag = 1)
  estmethod <- list("1" = estfun)
  tracking  <- makeTracking(om, c("9","10"))
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  out <- estMixME(x = "1",
                  stk = oem$stks,
                  flt = NULL,
                  idx = NULL,
                  ctrl = NULL,
                  om = om,
                  args = args,
                  estmethod = estmethod,
                  tracking = tracking,
                  fitList = NULL,
                  fwdList = NULL)
  
  ## Check performance
  ## --------------------------------------------------------------------------#
  
  ## check structure
  ## check dimensions - age
  ## check dimensions - year
  ## check content
  
})

## ============================================================================#
## Test perfect observation
## ============================================================================#

test_that("estMixME single-stock perfect observation works", {
  
  ## Generate token OEM
  ## --------------------------------------------------------------------------#
  
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  out <- estMixME(x = ,
                  stk = ,
                  flt = ,
                  idx = ,
                  ctrl = ,
                  om = ,
                  args = ,
                  estmethod = ,
                  tracking = ,
                  fitList = NULL,
                  fwdList = NULL)
  
  ## Check performance
  ## --------------------------------------------------------------------------#
  
  ## check structure
  ## check dimensions - age
  ## check dimensions - year
  ## check content
  
})