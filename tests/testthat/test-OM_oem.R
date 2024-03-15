## Test FLStock OEM structure
test_that("oemMixME outputs are a named list", {
  
  ## Generate token OM
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
  
  ## Generate token observation and deviance structure
  ## --------------------------------------------------------------------------#
  
  stk_oem <- FLStocks(FLStock(flq))
  stk_oem$`1`@catch.wt[] <- 1
  
  idx_oem <- FLIndices(FLIndex(flq[2,]))
  names(idx_oem) <- "X"
  idx_oem$X@index.q[] <- 0.5
  idx_oem$X@index[]   <- 1
  range(idx_oem$X)[c("startf", "endf")] <- c(1,1)
  
  stk_dev <- sapply(names(om$flts), function(x) return(flq), simplify = "array", USE.NAMES = TRUE)
  idx_dev <- flq
  
  deviances    = list(stk = list("1" = stk_dev),
                      idx = list("1" = idx_dev))
  observations = list(stk = stk_oem,
                      idx = list("1" = idx_oem))
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  ## define additional inputs
  x  = "1"
  args         = list(iy = 9, ay = 9, management_lag = 1)
  tracking     = makeTracking(om, projyrs = c("9","10"))
  catch_timing = sapply(om$stks@names, function(x) -1, USE.NAMES = TRUE, simplify = FALSE)
  idx_timing   = sapply(om$stks@names, function(x) 
    sapply("idx1", function(y) -1, USE.NAMES = FALSE, simplify = TRUE),
    USE.NAMES = TRUE, simplify = FALSE)
  use_stk_oem         = c("1" = TRUE)
  use_catch_residuals = c("1" = FALSE)
  use_idx_residuals   = c("1" = FALSE)
  use_om_weights      = c("1" = FALSE)
  
  ## generate observations
  oem <- oemMixME(x, 
                  om, 
                  deviances, 
                  observations, 
                  args, 
                  tracking, 
                  catch_timing,
                  idx_timing,
                  use_stk_oem,
                  use_catch_residuals,
                  use_idx_residuals,
                  use_om_weights)
  
  ## check structure
  expect_equal(is.list(oem), TRUE)
  expect_named(oem, c("stk", "flt", "idx", "tracking"))
  expect_type(oem$stk, "S4")
  expect_equal(class(oem$stk)[1], "FLStock")
  
  expect_type(oem$idx, "list")
  expect_equal(class(oem$idx)[1], "FLIndices")
  
  expect_type(oem$idx$X, "S4")
  expect_equal(class(oem$idx$X)[1], "FLIndex")
  
  ## check dimensions - age
  expect_equal(dimnames(oem$stk)$age, c("1","2"))
  expect_equal(dimnames(oem$idx$X)$age,"2")
  
  ## check dimensions - year
  expect_equal(dimnames(oem$stk)$year, as.character(1:8))
  expect_equal(dimnames(oem$idx$X)$year, as.character(1:8))
  
  ## check content
  expect_equal(sum(oem$stk@catch.n), 32)
  expect_equal(sum(oem$idx$X@index.q), 4)
  expect_equal(sum(oem$idx$X@index), 0.5*1*exp(-1*(1+1))*8)
  
})
