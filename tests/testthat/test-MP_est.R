## ============================================================================#
## Test user-supplied (FLBiol, FLFisheries) estimation method
## ============================================================================#

test_that("estMixME single-stock (FLBiol) estimation works", {
  
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
  om$stks$`1`@m[] <- 0.1
  om$stks$`1`@mat$mat[] <- 1
  om$stks$`1`@wt[]  <- 1
  om$stks$`1`@spwn[] <- 0
  om$stks$`1`@rec@params <- FLPar(1)
  om$stks$`1`@rec@model  <- FLCore::geomean()$model
  om$flts$A$`1`@landings.n[]  <- 0.5
  om$flts$A$`1`@discards.n[]  <- 0.5
  om$flts$A$`1`@landings.wt[] <- 1
  om$flts$A$`1`@discards.wt[] <- 1
  om$flts$A$`1`@catch.q <- FLPar(1, 
                                 dimnames=list(params=c('alpha','beta'), 
                                               year = dimnames(flq)$year, 
                                               iter = dimnames(flq)$iter),
                                 units='NA')
  om$flts$A$`1`@catch.q["beta",] <- 0
  om$flts$A$`1`@catch.sel[] <- 1
  om$flts$A@effort[] <- 1
  attr(om$flts$A$`1`,"quotashare") <- quantSums(flq)/2
  
  ## Calculate corresponding stock numbers, landings, discards
  f1 <- (om$flts$A$`1`@catch.q["alpha", ] * om$flts$A@effort) %*% om$flts$A$`1`@catch.sel
  
  om$stks$`1`@n[,1] <- c(1, 0.3328711, 0.1660896)
  
  tt <- FLasher::fwd(om$stks, om$flts, FLasher::fwdControl(data.frame(year=2:10, value = 1, quant = "f", biol = 1, minAge = 1, maxAge = 3)))
  om$stks <- tt$biols
  om$flts <- tt$fisheries
  
  ## Copy to observation error model
  stk0 <- om$stks
  stk0$`1`@n[] <- NA
  
  flt0 <- list("1" = om$flts)
  flt0$`1`$A@effort[] <- NA
  flt0$`1`$A$`1`@catch.q[] <- NA
  flt0$`1`$A$`1`@catch.sel[] <- NA
  
  ## Generate token stock assessment model & other arguments
  ## --------------------------------------------------------------------------#
  
  estfun <- function(stk, flt, tracking, ...) {
    
    ## Fill slots
    stk@n[] <- c(1, 0.3328711, 0.1660896)
    flt$A@effort[] <- 1
    flt$A$`1`@catch.q["alpha",] <- 1
    flt$A$`1`@catch.q["beta",] <- 0
    flt$A$`1`@catch.sel[] <- 1
    
    ## estimate final data year
    dy <- dims(stk)$maxyear
    
    ## Store estimated properties
    if (dy %in% dimnames(tracking$stk)$year) {
      tracking$stk["F.est", ac(dy)]  <- FLCore::fbar(stk, flt$A)[,ac(dy)]
      tracking$stk["B.est", ac(dy)]  <- FLCore::tsb(stk, flt$A)[,ac(dy)]
      tracking$stk["SB.est", ac(dy)] <- FLCore::ssb(stk)[,ac(dy)]
      
      tracking$stk["C.est", ac(dy)] <- FLCore::catch(flt$A$`1`)[,ac(dy)]
      tracking$stk["L.est", ac(dy)] <- FLCore::landings(flt$A$`1`)[,ac(dy)]
      tracking$stk["D.est", ac(dy)] <- FLCore::discards(flt$A$`1`)[,ac(dy)]
      
      tracking$sel_est[,ac(dy)] <- 
        sweep(FLFishery::harvest(stk, flt$A)[,ac(dy)], 
                                         c(2:6), fbar(stk, flt$A)[,ac(dy)], "/")
    }
    
    return(list(stk0 = stk,
                flt0 = flt,
                tracking = tracking))
  }
  
  ## arguments
  args <- list(ay = 10,
               iy = 9,
               management_lag = 1)
  estmethod <- list("1" = estfun)
  estgroup  <- list("1" = "1")
  tracking  <- makeTracking(om, c("9","10"))
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  # undebug(estfun)
  # undebug(harvest, signature = signature(object = "FLBiol", catch = "FLFishery"))
  # undebug(harvest, signature = signature(object = "FLQuant", catch = "FLQuant"))
  # undebug(tsb, signature = signature(object = "FLBiol", catch = "FLFishery"))
  out <- estMixME(x = "1",
                  stk = stk0,
                  flt = flt0,
                  idx = NULL,
                  ctrl = NULL,
                  om = om,
                  args = args,
                  estmethod = estmethod,
                  estgroup = estgroup,
                  tracking = tracking,
                  fitList = NULL,
                  fwdList = NULL)
  
  ## Check performance
  ## --------------------------------------------------------------------------#
  
  ## check structure
  expect_type(out, "list")
  expect_type(out$stk0, "S4")
  expect_type(out$flt0, "list")
  expect_true(is.null(out$sr0))
  expect_type(out$tracking, "list")
  
  ## check dimensions - age
  expect_equal(dimnames(out$stk0)$age, as.character(1:3))
  expect_equal(dimnames(out$flt0$A$`1`)$age, as.character(1:3))
  
  ## check dimensions - year
  expect_equal(dimnames(out$stk0)$year, as.character(1:10))
  expect_equal(dimnames(out$flt0$A)$year, as.character(1:10))
  
  ## check content - est
  expect_equal(round(sum(out$stk0@n),5), round(sum(quantSums(om$stks$`1`@n[,"10"])*10),5))
  expect_equal(sum(out$stk0@wt), 1*3*10)
  expect_equal(round(sum(out$flt0$A$`1`@landings.n),5), round(sum(FLCore::landings.n(om$flts$A$`1`)),5))
  expect_equal(round(sum(out$flt0$A$`1`@discards.n),5), round(sum(FLCore::discards.n(om$flts$A$`1`)),5))
  expect_equal(sum(out$flt0$A@effort), 10)
  expect_equal(sum(out$flt0$A$`1`@catch.q["alpha",]), 10)
  
  ## check content - tracking
  expect_equal(round(sum(out$tracking$stk["F.est", "10"]),5), mean(f1[,"10"]))
  expect_equal(round(sum(out$tracking$stk["B.est","10"]),5), round(sum(om$stks$`1`@wt[,"10"] * om$stks$`1`@n[,"10"]),5))
  expect_equal(round(sum(out$tracking$stk["SB.est","10"]),5), round(sum(om$stks$`1`@wt[,"10"] * FLCore::mat(om$stks$`1`)[,"10"] * om$stks$`1`@n[,"10"]),5))
  expect_equal(sum(out$tracking$stk["C.est", "10"]), sum(FLCore::catch(om$flts$A$`1`[,"10"])))
  expect_equal(sum(out$tracking$stk["L.est", "10"]), sum(FLCore::landings(om$flts$A$`1`[,"10"])))
  expect_equal(sum(out$tracking$stk["D.est", "10"]), sum(FLCore::landings(om$flts$A$`1`[,"10"])))
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
  om$stks$`1`@mat$mat[] <- 1
  om$stks$`1`@wt[]  <- 1
  om$stks$`1`@spwn[] <- 0
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
  oem$stks$`1`@stock[]   <- NA
  oem$stks$`1`@harvest[] <- NA
  oem$stks$`1`@m.spwn[]  <- 0
  oem$stks$`1`@harvest.spwn[] <- 0
  
  ## Generate token stock assessment model & other arguments
  ## --------------------------------------------------------------------------#
  
  estfun <- function(stk, tracking, ...) {
    stk@stock.n[] <- 2
    stk@stock[]   <- FLCore::computeStock(stk)
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
  estgroup  <- list("1" = "1")
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
                  estgroup = estgroup,
                  tracking = tracking,
                  fitList = NULL,
                  fwdList = NULL)
  
  ## Check performance
  ## --------------------------------------------------------------------------#
  
  ## check structure
  expect_type(out, "list")
  expect_type(out$stk0, "S4")
  expect_true(is.null(out$flt0))
  expect_true(is.null(out$sr0))
  expect_type(out$tracking, "list")
  
  ## check dimensions - age
  expect_equal(dimnames(out$stk0)$age, as.character(1:3))
  
  ## check dimensions - year
  expect_equal(dimnames(out$stk0)$year, as.character(1:10))
  
  ## check content - est
  expect_equal(sum(out$stk0@stock.n), 2*3*10)
  expect_equal(sum(out$stk0@stock.wt), 1*3*10)
  expect_equal(sum(out$stk0@catch.n), 2*3*10)
  expect_equal(sum(out$stk0@landings.n), 1*3*10)
  expect_equal(sum(out$stk0@discards.n), 1*3*10)
  expect_equal(sum(out$stk0@harvest), 3*3*10)
  
  ## check content - tracking
  expect_equal(sum(out$tracking$stk["F.est", "10"]), 3)
  expect_equal(sum(out$tracking$stk["B.est","10"]), 6)
  expect_equal(sum(out$tracking$stk["SB.est","10"]), 6)
  expect_equal(sum(out$tracking$stk["C.est", "10"]), 6)
  expect_equal(sum(out$tracking$stk["L.est", "10"]), 3)
  expect_equal(sum(out$tracking$stk["D.est", "10"]), 3)
  
})

## ============================================================================#
## Test perfect observation (FLBiol, FLFisheries)
## ============================================================================#

test_that("estMixME single-stock perfect observation works", {
  
  ## Generate token OEM
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
  om$stks$`1`@mat$mat[] <- 1
  om$stks$`1`@wt[]  <- 1
  om$stks$`1`@spwn[] <- 0
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
  stk0 <- om$stks
  stk0$`1`@n[] <- NA
  
  flt0 <- list("1" = om$flts)
  flt0$`1`$A@effort[] <- NA
  flt0$`1`$A$`1`@catch.q[] <- NA
  flt0$`1`$A$`1`@catch.sel[] <- NA
  
  ## Other arguments
  ## --------------------------------------------------------------------------#
  
  ## arguments
  args <- list(ay = 10,
               iy = 9,
               management_lag = 1,
               frange = list("1" = c(1,3)),
               use_fastF = TRUE)
  estmethod <- list("1" = "perfectObs")
  estgroup  <- list("1" = "1")
  tracking  <- makeTracking(om, c("9","10"))
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  out <- estMixME(x = "1",
                  stk = stk0,
                  flt = flt0,
                  idx = NULL,
                  ctrl = NULL,
                  om = om,
                  args = args,
                  estmethod = estmethod,
                  estgroup = estgroup,
                  tracking = tracking,
                  fitList = NULL,
                  fwdList = NULL)
  
  ## Check performance
  ## --------------------------------------------------------------------------#
  
  ## check structure
  expect_type(out, "list")
  expect_type(out$stk0, "S4")
  expect_type(out$flt0, "list")
  expect_true(is.null(out$sr0))
  expect_type(out$tracking, "list")
  
  ## check dimensions - age
  expect_equal(dimnames(out$stk0)$age, as.character(1:3))
  expect_equal(dimnames(out$flt0$A$`1`)$age, as.character(1:3))
  
  ## check dimensions - year
  expect_equal(dimnames(out$stk0)$year, as.character(1:10))
  expect_equal(dimnames(out$flt0$A)$year, as.character(1:10))
  
  ## check content - est
  expect_equal(sum(out$stk0@n), 30)
  expect_equal(sum(out$stk0@wt), 1*3*10)
  expect_equal(sum(out$flt0$A$`1`@landings.n), 1*3*10)
  expect_equal(sum(out$flt0$A$`1`@discards.n), 1*3*10)
  expect_equal(sum(out$flt0$A@effort), 10)
  expect_equal(sum(out$flt0$A$`1`@catch.q["alpha",]), 10)
  
  ## check content - tracking
  expect_equal(sum(out$tracking$stk["F.est", "10"]), 1)
  expect_equal(sum(out$tracking$stk["B.est","10"]), 3)
  expect_equal(sum(out$tracking$stk["SB.est","10"]), 3)
  expect_equal(sum(out$tracking$stk["C.est", "10"]), 6)
  expect_equal(sum(out$tracking$stk["L.est", "10"]), 3)
  expect_equal(sum(out$tracking$stk["D.est", "10"]), 3)
  
})
