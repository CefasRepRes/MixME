## ============================================================================#
## Test user-supplied stock estimation method
## ============================================================================#

test_that("estMixME single-stock estimation works", {
  
  ## Generate token OEM
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
  om$stks$`1`@n[] <- NA
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
                  fitList = ,
                  fwdList = )
  
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