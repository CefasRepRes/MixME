test_that("hcrMixME single-stock (FLStocks) constant catch works", {
  
  ## Generate token OM & estimated stock
  ## --------------------------------------------------------------------------#
  flq <- FLCore::FLQuant(1, 
                         dim = c(3,10,1,1,1,1), 
                         dimnames = list(age  = c("1","2","3"),
                                         year = c("1","2","3","4","5","6","7","8","9","10")))
  flt <- FLFishery::FLFishery(i = FLFishery::FLCatch(flq))
  om  <- list(stks = FLCore::FLBiols(i = FLCore::FLBiol(flq)),
              flts = FLFishery::FLFisheries(list(A = flt)))
  
  ## Generate names
  om$stks$i@name <- "i"
  
  ## Fill necessary slots
  om$stks$i@n[] <- 1
  om$stks$i@m[] <- 1
  om$stks$i@mat$mat[] <- 1
  om$stks$i@wt[]  <- 1
  om$stks$i@spwn[] <- 0
  om$flts$A$i@landings.n[]  <- 1
  om$flts$A$i@landings.wt[] <- 1
  om$flts$A$i@discards.n[]  <- 1
  om$flts$A$i@discards.wt[] <- 1
  om$flts$A$i@catch.q <- FLPar(1, 
                                 dimnames=list(params=c('alpha','beta'), 
                                               year = dimnames(flq)$year, 
                                               iter = dimnames(flq)$iter),
                                 units='NA')
  om$flts$A$i@catch.sel[] <- 1
  om$flts$A@effort[] <- 1
  attr(om$flts$A$i,"quotashare") <- quantSums(flq)/2
  
  stks <- FLStocks("i" = as.FLStock(om$stks$i, om$flts))
  
  ## Other arguments
  ## --------------------------------------------------------------------------#
  
  ## arguments
  args <- list(ay = 10,
               iy = 9,
               management_lag = 1)
  
  hcrmethod <- list("i" = "hcrFixedC")
  hcrgroup  <- list("i" = "i")
  tracking  <- makeTracking(om, c("9","10"))
  ctrg      <- list("i" = 0.5)
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  out <- hcrMixME(x = "i",
                  stk  = stks,
                  flt  = NULL,
                  idx  = NULL,
                  args = args,
                  hcrpars = NULL,
                  hcrmethod = hcrmethod,
                  hcrgroup  = hcrgroup,
                  ctrg = ctrg,
                  ftrg = NULL,
                  tracking = tracking)
  
})
