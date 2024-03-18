## ============================================================================#
## FLStock input
## ============================================================================#

test_that("oemTrimFLStock works", {
  
  ## Generate token stock
  flq <- FLCore::FLQuant(1, 
                         dim = c(2,10,1,1,1,1), 
                         dimnames = list(age  = c("1","2"),
                                         year = c("1","2","3","4","5","6","7","8","9","10")))
  stks <- FLCore::FLStocks(FLCore::FLStock(flq))
  names(stks) <- "1"
  
  ## Fill stock slots
  stks$`1`@catch.n[]  <- 1
  stks$`1`@catch.wt[] <- 1
  FLCore::catch(stks$`1`) <- FLCore::computeCatch(stks$`1`)
  
  ## Generate inputs
  x  <- "1"
  ay <- 10
  idx_timing   <- sapply(stks@names, function(x) 
    sapply("idx1", function(y) -1, USE.NAMES = FALSE, simplify = TRUE),
    USE.NAMES = TRUE, simplify = FALSE)
  catch_timing <- sapply(stks@names, function(x) -1, USE.NAMES = TRUE, simplify = FALSE)
  
  ## Run function
  stk0 <- MixME:::oemTrimFLStock(stks$`1`,
                                 x,
                                 ay,
                                 idx_timing,
                                 catch_timing)
  
  ## expect FLStock
  expect_type(stk0, "S4")
  expect_equal(class(stk0)[1], "FLStock")
  
  ## expect correct dimensions
  expect_equal(dimnames(stk0@catch)$year, as.character(1:9))
  
  ## expect correct content
  expect_equal(sum(stk0@catch), 2*9)
})

## ============================================================================#
## FLFisheries input
## ============================================================================#

test_that("oemTrimFLFisheries works", {
  
  ## Generate token stock
  flq <- FLCore::FLQuant(1, 
                         dim = c(2,10,1,1,1,1), 
                         dimnames = list(age  = c("1","2"),
                                         year = c("1","2","3","4","5","6","7","8","9","10")))
  flt <- FLFishery::FLFishery(FLFishery::FLCatch(flq))
  flts = FLFishery::FLFisheries(list(flt))
  names(flts) <- "A"
  names(flts$A) <- "1"
  flts$A$`1`@landings.n[]  <- 1
  flts$A$`1`@landings.wt[] <- 1
  flts$A$`1`@discards.n[]  <- 1
  flts$A$`1`@discards.wt[] <- 1
  flts$A$`1`@catch.q <- FLPar(1, 
                              dimnames=list(params=c('alpha','beta'), 
                                            year = dimnames(flq)$year, 
                                            iter = dimnames(flq)$iter),
                              units='NA')
  flts$A$`1`@catch.sel[] <- 1
  flts$A@effort[] <- 2
  attr(flts$A$`1`,"quotashare") <- quantSums(flq)/2
  
  ## Run function
  flt0 <- MixME:::oemTrimFLFisheries(object = flts, minyr = 1, maxyr = 8)
  
  ## check structure
  expect_type(flt0, "list")
  expect_equal(class(flt0)[1], "FLFisheries")
  
  expect_type(flt0$A, "list")
  expect_equal(class(flt0$A)[1], "FLFishery")
  
  expect_type(flt0$A$`1`, "S4")
  expect_equal(class(flt0$A$`1`)[1], "FLCatch")
  
  ## check dimensions - age
  expect_equal(dimnames(flt0$A$`1`)$age, c("1","2"))
  
  ## check dimensions - year
  expect_equal(dimnames(flt0$A)$year, as.character(1:8))
  expect_equal(dimnames(flt0$A$`1`)$year, as.character(1:8))
  expect_equal(dimnames(flt0$A$`1`@catch.q)$year, as.character(1:8))
  expect_equal(dimnames(attr(flt0$A$`1`, "quotashare"))$year, as.character(1:8))
  
  ## check content
  expect_equal(sum(flt0$A@effort), 16)
  expect_equal(sum(flt0$A@capacity), 8)
  expect_equal(sum(FLCore::catch.n(flt0$A$`1`)), 32)
  expect_equal(sum(FLCore::catch.q(flt0$A$`1`)), 16)
  expect_equal(sum(attr(flt0$A$`1`,"quotashare")), 8)
})
