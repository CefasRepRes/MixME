test_that("hcrRun multi-stock (FLBiols, FLStocks) HCR works", {
  
  ## Generate token OM & estimated stocks
  ## --------------------------------------------------------------------------#
  flq <- FLCore::FLQuant(1, 
                         dim = c(3,10,1,1,1,1), 
                         dimnames = list(age  = c("1","2","3"),
                                         year = c("1","2","3","4","5","6","7","8","9","10")))
  flt <- FLFishery::FLFishery(i = FLFishery::FLCatch(flq),
                              j = FLFishery::FLCatch(flq),
                              k = FLFishery::FLCatch(flq))
  om  <- list(stks = FLCore::FLBiols(i = FLCore::FLBiol(flq),
                                     j = FLCore::FLBiol(flq),
                                     k = FLCore::FLBiol(flq)),
              flts = FLFishery::FLFisheries(list(flt)))
  
  ## Generate names
  om$stks$i@name <- "i"
  om$stks$j@name <- "j"
  om$stks$k@name <- "k"
  names(om$flts) <- "A"
  
  ## Fill necessary slots
  om$stks$i@n[] <- om$stks$j@n[] <- om$stks$k@n[] <- 1
  om$stks$i@m[] <- om$stks$j@m[] <- om$stks$k@m[] <- 1
  om$stks$i@mat$mat[] <- om$stks$j@mat$mat[] <- om$stks$k@mat$mat[] <- 1
  om$stks$i@wt[]      <- om$stks$j@wt[]      <- om$stks$k@wt[]      <- 1
  om$stks$i@spwn[]    <- om$stks$j@spwn[]    <- om$stks$k@spwn[]    <- 0
  
  
  om$flts$A$i@landings.n[]  <- om$flts$A$j@landings.n[]  <- om$flts$A$k@landings.n[]  <- 1
  om$flts$A$i@landings.wt[] <- om$flts$A$j@landings.wt[] <- om$flts$A$k@landings.wt[] <- 1
  om$flts$A$i@discards.n[]  <- om$flts$A$j@discards.n[]  <- om$flts$A$k@discards.n[]  <- 1
  om$flts$A$i@discards.wt[] <- om$flts$A$j@discards.wt[] <- om$flts$A$k@discards.wt[] <- 1
  om$flts$A$i@catch.sel[]   <- om$flts$A$j@catch.sel[]   <- om$flts$A$k@catch.sel[]   <- 1
  attr(om$flts$A$i,"quotashare") <- attr(om$flts$A$j,"quotashare") <- attr(om$flts$A$k,"quotashare") <- quantSums(flq)/3
  
  om$flts$A$i@catch.q <- FLPar(1, 
                               dimnames=list(params=c('alpha','beta'), 
                                             year = dimnames(flq)$year, 
                                             iter = dimnames(flq)$iter),
                               units='NA')
  om$flts$A$j@catch.q <- FLPar(1, 
                               dimnames=list(params=c('alpha','beta'), 
                                             year = dimnames(flq)$year, 
                                             iter = dimnames(flq)$iter),
                               units='NA')
  om$flts$A$k@catch.q <- FLPar(1, 
                               dimnames=list(params=c('alpha','beta'), 
                                             year = dimnames(flq)$year, 
                                             iter = dimnames(flq)$iter),
                               units='NA')
  
  om$flts$A@effort[] <- 1
  
  stks <- FLStocks(i = as.FLStock(om$stks$i, om$flts),
                   j = as.FLStock(om$stks$j, om$flts),
                   k = as.FLStock(om$stks$k, om$flts))
  
  ## Generate token multi-stock HCR & other arguments
  ## --------------------------------------------------------------------------#
  
  hcrfun <- function(stk, args, tracking, ...) {
    cc <- ifelse(stk@name == "i", 10, ifelse(stk@name == "j", 5, 1))
    return(list(ctrl = FLasher::fwdControl(list(year = args$ay + args$management_lag, 
                                                quant = "catch", 
                                                value = cc)),
                tracking = tracking))
  }
  
  hcrMultifun <- function(stk, args, hcrpars, tracking) {
    lapply(names(stk), function(x) {
      ll <- hcrfun(stk  = stk[[x]], args = args, tracking[[x]])
      ll$tracking$stk["hcr.adv", ac(args$ay + args$management_lag)] <- ll$ctrl@iters[1,"value",1]
      return(list(ctrl     = ll$ctrl,
                  tracking = ll$tracking))
    })
  }
  
  ## arguments
  args <- list(ay = 9,
               iy = 9,
               management_lag = 1)
  hcrmethod <- list(grp = hcrMultifun,
                    k   = hcrfun)
  hcrgroup  <- list(grp = c("i","j"),
                    k   = "k")
  tracking  <- makeTracking(om, c("9","10"))
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  out1 <- hcrRun(stk = stks,
                flt = NULL,
                idx = NULL,
                args = args,
                hcrmethod = hcrmethod,
                hcrgroup  = hcrgroup,
                ctrg = NULL,
                ftrg = NULL,
                tracking = tracking)
  
  out2 <- hcrRun(stk = om$stks,
                 flt = om$flts,
                 idx = NULL,
                 args = args,
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
  
  expect_type(out1$ctrl, "list")
  expect_type(out1$ctrl[[1]], "S4")
  expect_type(out1$tracking, "list")

  expect_type(out2$ctrl, "list")
  expect_type(out2$ctrl[[1]], "S4")
  expect_type(out2$tracking, "list")
  
  ## check dimensions - year
  expect_equal(out1$ctrl[["i"]]@target$year, 10)
  expect_equal(out1$ctrl[["j"]]@target$year, 10)
  expect_equal(out1$ctrl[["k"]]@target$year, 10)
  expect_equal(dimnames(out1$tracking[["i"]]$stk)$year, as.character(9:10))
  expect_equal(dimnames(out1$tracking[["j"]]$stk)$year, as.character(9:10))
  expect_equal(dimnames(out1$tracking[["k"]]$stk)$year, as.character(9:10))
  
  expect_equal(out2$ctrl[["i"]]@target$year, 10)
  expect_equal(out2$ctrl[["j"]]@target$year, 10)
  expect_equal(out2$ctrl[["k"]]@target$year, 10)
  expect_equal(dimnames(out2$tracking[["i"]]$stk)$year, as.character(9:10))
  expect_equal(dimnames(out2$tracking[["j"]]$stk)$year, as.character(9:10))
  expect_equal(dimnames(out2$tracking[["k"]]$stk)$year, as.character(9:10))
  
  ## check content - ctrl
  expect_equal(out1$ctrl[["i"]]@iters[1,"value",1], 10)
  expect_equal(out1$ctrl[["j"]]@iters[1,"value",1], 5)
  expect_equal(out1$ctrl[["k"]]@iters[1,"value",1], 1)
  
  expect_equal(out2$ctrl[["i"]]@iters[1,"value",1], 10)
  expect_equal(out2$ctrl[["j"]]@iters[1,"value",1], 5)
  expect_equal(out2$ctrl[["k"]]@iters[1,"value",1], 1)
  
  expect_equal(as.character(out1$ctrl[["i"]]@target$quant), "catch")
  expect_equal(as.character(out1$ctrl[["j"]]@target$quant), "catch")
  expect_equal(as.character(out1$ctrl[["k"]]@target$quant), "catch")
  
  expect_equal(as.character(out2$ctrl[["i"]]@target$quant), "catch")
  expect_equal(as.character(out2$ctrl[["j"]]@target$quant), "catch")
  expect_equal(as.character(out2$ctrl[["k"]]@target$quant), "catch")
  
  expect_equal(c(out1$tracking[["i"]]$stk["hcr.adv","10"]), 10)
  expect_equal(c(out1$tracking[["j"]]$stk["hcr.adv","10"]), 5)
  expect_equal(c(out1$tracking[["k"]]$stk["hcr.adv","10"]), 1)
  
  expect_equal(c(out2$tracking[["i"]]$stk["hcr.adv","10"]), 10)
  expect_equal(c(out2$tracking[["j"]]$stk["hcr.adv","10"]), 5)
  expect_equal(c(out2$tracking[["k"]]$stk["hcr.adv","10"]), 1)
})
