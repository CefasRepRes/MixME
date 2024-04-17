test_that("estRun multi-stock (FLStock) estimation works", {
  
  ## Generate token OM & OEM
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
  om$stks$i@wt[]      <- om$stks$k@wt[]      <- 1
  om$stks$j@wt[]      <- 2
  om$stks$i@spwn[]    <- om$stks$j@spwn[]    <- om$stks$k@spwn[]    <- 0
  
  
  om$flts$A$i@landings.n[]  <- om$flts$A$j@landings.n[]  <- om$flts$A$k@landings.n[]  <- 1
  om$flts$A$i@landings.wt[] <- om$flts$A$k@landings.wt[] <- 1
  om$flts$A$j@landings.wt[] <- 2
  om$flts$A$i@discards.n[]  <- om$flts$A$j@discards.n[]  <- om$flts$A$k@discards.n[]  <- 1
  om$flts$A$i@discards.wt[] <- om$flts$A$k@discards.wt[] <- 1
  om$flts$A$j@discards.wt[] <- 2
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
  
  ## Copy to observation error model
  oem <- om
  # undebug(as.FLStock, signature = signature(object = "FLBiol"))
  oem$stks <- FLStocks(i = as.FLStock(oem$stks$i, oem$flts),
                       j = as.FLStock(oem$stks$j, oem$flts),
                       k = as.FLStock(oem$stks$k, oem$flts))
  
  ## Note that weights for j = 1
  oem$stks$i@stock.n[] <- oem$stks$j@stock.n[] <- oem$stks$k@stock.n[] <- NA
  oem$stks$i@stock[]   <- oem$stks$j@stock[]   <- oem$stks$k@stock[]   <- NA
  oem$stks$i@harvest[] <- oem$stks$j@harvest[] <- oem$stks$k@harvest[] <- NA
  
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
  
  estMultifun <- function(stks, ...) {
    lapply(names(stks), function(x) estfun(stks[[x]], tracking[[x]]))
  }
  
  ## arguments
  args <- list(ay = 10,
               iy = 9,
               management_lag = 1)
  estmethod <- list(grp = estMultifun,
                    k   = estfun)
  estgroup  <- list(grp = c("i","j"),
                    k   = "k")
  tracking  <- makeTracking(om, c("9","10"))
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  out <- estRun(stk = oem$stks,
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
  expect_type(out$stk, "list")
  expect_type(out$stk[[1]], "S4")
  
  expect_type(out$flt, "list")
  expect_true(is.null(out$flt[[1]]))
  
  expect_type(out$sr, "list")
  expect_true(is.null(out$sr[[1]]))
  expect_type(out$tracking, "list")
  
  ## check dimensions - age
  expect_equal(dimnames(out$stk[["i"]])$age, as.character(1:3))
  expect_equal(dimnames(out$stk[["j"]])$age, as.character(1:3))
  expect_equal(dimnames(out$stk[["k"]])$age, as.character(1:3))
  
  ## check dimensions - year
  expect_equal(dimnames(out$stk[["i"]])$year, as.character(1:10))
  expect_equal(dimnames(out$stk[["j"]])$year, as.character(1:10))
  expect_equal(dimnames(out$stk[["k"]])$year, as.character(1:10))
  
  ## check content - est
  expect_equal(sum(out$stk[["i"]]@stock.n), 2*3*10)
  expect_equal(sum(out$stk[["j"]]@stock.n), 2*3*10)
  expect_equal(sum(out$stk[["k"]]@stock.n), 2*3*10)
  
  expect_equal(sum(out$stk[["i"]]@stock.wt), 1*3*10)
  expect_equal(sum(out$stk[["j"]]@stock.wt), 2*3*10)
  expect_equal(sum(out$stk[["k"]]@stock.wt), 1*3*10)
  
  expect_equal(sum(out$stk[["i"]]@stock), 2*3*10)
  expect_equal(sum(out$stk[["j"]]@stock), 4*3*10)
  expect_equal(sum(out$stk[["k"]]@stock), 2*3*10)
  
  expect_equal(sum(out$stk[["i"]]@catch.n), 2*3*10)
  expect_equal(sum(out$stk[["j"]]@catch.n), 2*3*10)
  expect_equal(sum(out$stk[["k"]]@catch.n), 2*3*10)
  
  expect_equal(sum(out$stk[["i"]]@landings.n), 1*3*10)
  expect_equal(sum(out$stk[["j"]]@landings.n), 1*3*10)
  expect_equal(sum(out$stk[["k"]]@landings.n), 1*3*10)
  
  expect_equal(sum(out$stk[["i"]]@discards.n), 1*3*10)
  expect_equal(sum(out$stk[["j"]]@discards.n), 1*3*10)
  expect_equal(sum(out$stk[["k"]]@discards.n), 1*3*10)
  
  expect_equal(sum(out$stk[["i"]]@harvest), 3*3*10)
  expect_equal(sum(out$stk[["j"]]@harvest), 3*3*10)
  expect_equal(sum(out$stk[["k"]]@harvest), 3*3*10)
  
  ## check content - tracking
  expect_equal(sum(out$tracking$i$stk["F.est", "10"]), 3)
  expect_equal(sum(out$tracking$i$stk["B.est","10"]),  6)
  expect_equal(sum(out$tracking$i$stk["SB.est","10"]), 6)
  expect_equal(sum(out$tracking$i$stk["C.est", "10"]), 6)
  expect_equal(sum(out$tracking$i$stk["L.est", "10"]), 3)
  expect_equal(sum(out$tracking$i$stk["D.est", "10"]), 3)
  
  expect_equal(sum(out$tracking$j$stk["F.est", "10"]), 3)
  expect_equal(sum(out$tracking$j$stk["B.est","10"]),  12)
  expect_equal(sum(out$tracking$j$stk["SB.est","10"]), 12)
  expect_equal(sum(out$tracking$j$stk["C.est", "10"]), 6)
  expect_equal(sum(out$tracking$j$stk["L.est", "10"]), 3)
  expect_equal(sum(out$tracking$j$stk["D.est", "10"]), 3)
  
  expect_equal(sum(out$tracking$k$stk["F.est", "10"]), 3)
  expect_equal(sum(out$tracking$k$stk["B.est","10"]),  6)
  expect_equal(sum(out$tracking$k$stk["SB.est","10"]), 6)
  expect_equal(sum(out$tracking$k$stk["C.est", "10"]), 6)
  expect_equal(sum(out$tracking$k$stk["L.est", "10"]), 3)
  expect_equal(sum(out$tracking$k$stk["D.est", "10"]), 3)
})
