## ============================================================================#
## Test HCR parameterisation function
## ============================================================================#

test_that("phcrMixME works", {
  
  ## Generate token stock
  ## --------------------------------------------------------------------------#
  flq <- FLCore::FLQuant(1, 
                         dim = c(3,10,1,1,1,1), 
                         dimnames = list(age  = c("1","2","3"),
                                         year = c("1","2","3","4","5","6","7","8","9","10")))
  stk  <- FLStock(flq)
  
  expect_error(phcrMixME(stk = stk, args = NULL, tracking = NULL))
  expect_error(phcrMixME(stk = stk, args = NULL, hcrpars = NULL, tracking = NULL))
  
  out <- phcrMixME(stk = stk, args = NULL, hcrpars = list(i = c(trgt = 1)), tracking = NULL)
  
  expect_type(out, "list")
  expect_type(out$hcrpars, "list")
  expect_equal(class(out$hcrpars$i)[1], "FLPar")
  expect_equal(c(out$hcrpars$i), 1)
})

## ============================================================================#
## Test in-built Constant Catch HCR (FLBiols, FLStocks)
## ============================================================================#

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
  args <- list(ay = 9,
               iy = 9,
               frq = 1,
               management_lag = 1)
  
  hcrmethod <- list("i" = "hcrFixedC")
  hcrgroup  <- list("i" = "i")
  tracking  <- makeTracking(om, c("9","10"))
  ctrg      <- list("i" = 0.5)
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  out1 <- hcrMixME(x = "i",
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
  
  out2 <- hcrMixME(x = "i",
                   stk  = om$stks,
                   flt  = om$flts,
                   idx  = NULL,
                   args = args,
                   hcrpars = NULL,
                   hcrmethod = hcrmethod,
                   hcrgroup  = hcrgroup,
                   ctrg = ctrg,
                   ftrg = NULL,
                   tracking = tracking)
  
  ## Check performance
  ## --------------------------------------------------------------------------#
  
  ## check structure
  expect_type(out1, "list")
  expect_type(out1$ctrl, "S4")
  expect_type(out1$tracking, "list")
  
  expect_type(out2, "list")
  expect_type(out2$ctrl, "S4")
  expect_type(out2$tracking, "list")
  
  ## check content
  expect_equal(out1$ctrl@iters[1,"value",1], 0.5)
  expect_equal(out2$ctrl@iters[1,"value",1], 0.5)
  expect_equal(as.character(out1$ctrl@target[1,"quant"]), "catch")
  expect_equal(as.character(out2$ctrl@target[1,"quant"]), "catch")
  
  ## check content - tracking
  expect_equal(c(out1$tracking$stk["hcr.adv","10"]), 0.5)
  expect_equal(c(out2$tracking$stk["hcr.adv","10"]), 0.5)
})

## ============================================================================#
## Test in-built Constant F HCR (FLBiols, FLStocks)
## ============================================================================#

test_that("hcrMixME single-stock (FLStocks) constant F works", {
  
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
  args <- list(ay = 9,
               iy = 9,
               frq = 1,
               management_lag = 1)
  
  hcrmethod <- list("i" = "hcrFixedF")
  hcrgroup  <- list("i" = "i")
  tracking  <- makeTracking(om, c("9","10"))
  ftrg      <- list("i" = 0.5)
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  out1 <- hcrMixME(x = "i",
                   stk  = stks,
                   flt  = NULL,
                   idx  = NULL,
                   args = args,
                   hcrpars = NULL,
                   hcrmethod = hcrmethod,
                   hcrgroup  = hcrgroup,
                   ctrg = NULL,
                   ftrg = ftrg,
                   tracking = tracking)
  
  out2 <- hcrMixME(x = "i",
                   stk  = om$stks,
                   flt  = om$flts,
                   idx  = NULL,
                   args = args,
                   hcrpars = NULL,
                   hcrmethod = hcrmethod,
                   hcrgroup  = hcrgroup,
                   ctrg = NULL,
                   ftrg = ftrg,
                   tracking = tracking)
  
  ## Check performance
  ## --------------------------------------------------------------------------#
  
  ## check structure
  expect_type(out1, "list")
  expect_type(out1$ctrl, "S4")
  expect_type(out1$tracking, "list")
  
  expect_type(out2, "list")
  expect_type(out2$ctrl, "S4")
  expect_type(out2$tracking, "list")
  
  ## check content
  expect_equal(out1$ctrl@iters[1,"value",1], 0.5)
  expect_equal(out2$ctrl@iters[1,"value",1], 0.5)
  expect_equal(as.character(out1$ctrl@target[1,"quant"]), "fbar")
  expect_equal(as.character(out2$ctrl@target[1,"quant"]), "fbar")
  
  ## check content - tracking
  expect_equal(c(out1$tracking$stk["hcr.adv","10"]), 0.5)
  expect_equal(c(out2$tracking$stk["hcr.adv","10"]), 0.5)
})

## ============================================================================#
## Test in-built ICES hockey-stick HCR (FLBiols, FLStocks)
## ============================================================================#

test_that("hcrMixME single-stock (FLStocks) ICES HCR works", {
  
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
  stks$i@m.spwn[] <- 0
  stks$i@harvest.spwn[] <- 0
  
  ## Other arguments
  ## --------------------------------------------------------------------------#
  
  ## arguments
  args <- list(ay = 9,
               iy = 9,
               frq = 1,
               management_lag = 1)
  
  hcrpars1   <- list("i" = c(Ftrgt = 0.5,  Btrigger = 1, Blim =  0.25))
  hcrpars2   <- list("i" = c(Ftrgt = 0.5,  Btrigger = 4, Blim =  0.25))
  hcrmethod <- list("i" = "hcrICES")
  hcrgroup  <- list("i" = "i")
  tracking  <- makeTracking(om, c("9","10"))
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  ## FLStocks & SSB above Btrigger
  out1 <- hcrMixME(x = "i",
                   stk  = stks,
                   flt  = NULL,
                   idx  = NULL,
                   args = args,
                   hcrpars   = hcrpars1,
                   hcrmethod = hcrmethod,
                   hcrgroup  = hcrgroup,
                   ctrg = NULL,
                   ftrg = NULL,
                   tracking = tracking)
  
  ## FLStocks & SSB below Btrigger
  out2 <- hcrMixME(x = "i",
                   stk  = stks,
                   flt  = NULL,
                   idx  = NULL,
                   args = args,
                   hcrpars   = hcrpars2,
                   hcrmethod = hcrmethod,
                   hcrgroup  = hcrgroup,
                   ctrg = NULL,
                   ftrg = NULL,
                   tracking = tracking)
  
  out3 <- hcrMixME(x = "i",
                   stk  = om$stks,
                   flt  = om$flts,
                   idx  = NULL,
                   args = args,
                   hcrpars   = hcrpars1,
                   hcrmethod = hcrmethod,
                   hcrgroup  = hcrgroup,
                   ctrg = NULL,
                   ftrg = NULL,
                   tracking = tracking)
  
  out4 <- hcrMixME(x = "i",
                   stk  = om$stks,
                   flt  = om$flts,
                   idx  = NULL,
                   args = args,
                   hcrpars   = hcrpars2,
                   hcrmethod = hcrmethod,
                   hcrgroup  = hcrgroup,
                   ctrg = NULL,
                   ftrg = NULL,
                   tracking = tracking)
  
  ## Check performance
  ## --------------------------------------------------------------------------#
  
  ## check structure
  expect_type(out1, "list")
  expect_type(out2, "list")
  expect_type(out3, "list")
  expect_type(out4, "list")
  expect_type(out1$ctrl, "S4")
  expect_type(out2$ctrl, "S4")
  expect_type(out3$ctrl, "S4")
  expect_type(out4$ctrl, "S4")
  expect_type(out1$tracking, "list")
  expect_type(out2$tracking, "list")
  expect_type(out3$tracking, "list")
  expect_type(out4$tracking, "list")
  
  ## check content
  expect_equal(out1$ctrl@iters[1,"value",1], 0.5)
  expect_equal(out2$ctrl@iters[1,"value",1], (3/4) * 0.5)
  expect_equal(out3$ctrl@iters[1,"value",1], 0.5)
  expect_equal(out4$ctrl@iters[1,"value",1], (3/4) * 0.5)
  
  expect_equal(as.character(out1$ctrl@target[1,"quant"]), "fbar")
  expect_equal(as.character(out2$ctrl@target[1,"quant"]), "fbar")
  expect_equal(as.character(out3$ctrl@target[1,"quant"]), "fbar")
  expect_equal(as.character(out4$ctrl@target[1,"quant"]), "fbar")
  
  ## check content - tracking
  expect_equal(c(out1$tracking$stk["hcr.adv","10"]), 0.5)
  expect_equal(c(out2$tracking$stk["hcr.adv","10"]), (3/4) * 0.5)
  expect_equal(c(out3$tracking$stk["hcr.adv","10"]), 0.5)
  expect_equal(c(out4$tracking$stk["hcr.adv","10"]), (3/4) * 0.5)
})
