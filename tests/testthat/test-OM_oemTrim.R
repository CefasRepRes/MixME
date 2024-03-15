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
