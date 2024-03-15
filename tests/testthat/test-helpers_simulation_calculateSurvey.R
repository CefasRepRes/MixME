## ============================================================================#
## FLBiol / FLIndex
## ============================================================================#

test_that("calculateSurvey works with FLBiol and FLIndex", {
  
  ## Generate token inputs
  ## --------------------------------------------------------------------------#
  flq <- FLCore::FLQuant(1, 
                         dim = c(2,10,1,1,1,1), 
                         dimnames = list(age  = c("1","2"),
                                         year = c("1","2","3","4","5","6","7","8","9","10")))
  stk <- FLCore::FLBiol(flq)
  flt <- FLFishery::FLFisheries(list(FLFishery::FLFishery(FLFishery::FLCatch(flq))))
  idx <- FLIndex(flq[2,])
  
  name(stk) <- "1"
  names(flt) <- "A"
  names(flt$A) <- "1"
  
  ## Fill slots
  stk@n[]                 <- 1
  stk@m[]                 <- 1
  flt$A$`1`@landings.n[]  <- 1
  flt$A$`1`@landings.wt[] <- 1
  flt$A$`1`@discards.n[]  <- 1
  flt$A$`1`@discards.wt[] <- 1
  flt$A$`1`@catch.q <- FLPar(1, 
                             dimnames=list(params=c('alpha','beta'), 
                                           year = dimnames(flq)$year, 
                                           iter = dimnames(flq)$iter),
                             units='NA')
  flt$A$`1`@catch.sel[] <- 1
  flt$A@effort[] <- 1
  
  idx@index[]   <- 1
  idx@index.q[] <- 0.5
  range(idx)[c("startf", "endf")] <- c(1,1)
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  idx0 <- calculateSurvey(stk = stk,
                          flt = flt,
                          idx = idx, use_q = TRUE,
                          use_time = TRUE)
  
  expect_equal(sum(idx0@index), 0.5*1*exp(-1*(1+1))*10)
})

## ============================================================================#
## FLBiol / FLIndices
## ============================================================================#

test_that("calculateSurvey works with FLBiol and FLIndices", {
  
  ## Generate token inputs
  ## --------------------------------------------------------------------------#
  flq <- FLCore::FLQuant(1, 
                         dim = c(2,10,1,1,1,1), 
                         dimnames = list(age  = c("1","2"),
                                         year = c("1","2","3","4","5","6","7","8","9","10")))
  stk <- FLCore::FLBiol(flq)
  flt <- FLFishery::FLFisheries(list(FLFishery::FLFishery(FLFishery::FLCatch(flq))))
  idx <- FLIndices(FLIndex(flq[2,]))
  
  name(stk) <- "1"
  names(flt) <- "A"
  names(flt$A) <- "1"
  names(idx) <- "X"
  
  ## Fill slots
  stk@n[]                 <- 1
  stk@m[]                 <- 1
  flt$A$`1`@landings.n[]  <- 1
  flt$A$`1`@landings.wt[] <- 1
  flt$A$`1`@discards.n[]  <- 1
  flt$A$`1`@discards.wt[] <- 1
  flt$A$`1`@catch.q <- FLPar(1, 
                             dimnames=list(params=c('alpha','beta'), 
                                           year = dimnames(flq)$year, 
                                           iter = dimnames(flq)$iter),
                             units='NA')
  flt$A$`1`@catch.sel[] <- 1
  flt$A@effort[] <- 1

  idx$X@index[]   <- 1
  idx$X@index.q[] <- 0.5
  range(idx$X)[c("startf", "endf")] <- c(1,1)
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  idx0 <- calculateSurvey(stk = stk,
                          flt = flt,
                          idx = idx, use_q = TRUE,
                          use_time = TRUE)
  
  expect_equal(sum(idx0$X@index), 0.5*1*exp(-1*(1+1))*10)
})

## ============================================================================#
## FLBiol / List
## ============================================================================#

test_that("calculateSurvey works with FLBiol and List", {
  
  ## Generate token inputs
  ## --------------------------------------------------------------------------#
  flq <- FLCore::FLQuant(1, 
                         dim = c(2,10,1,1,1,1), 
                         dimnames = list(age  = c("1","2"),
                                         year = c("1","2","3","4","5","6","7","8","9","10")))
  stk <- FLCore::FLBiols(FLCore::FLBiol(flq))
  flt <- FLFishery::FLFisheries(list(FLFishery::FLFishery(FLFishery::FLCatch(flq))))
  idx <- list("1" = FLIndices(FLIndex(flq[2,])))
  
  names(stk) <- "1"
  name(stk$`1`) <- "1"
  names(flt) <- "A"
  names(flt$A) <- "1"
  names(idx$`1`) <- "X"
  
  ## Fill slots
  stk$`1`@n[]             <- 1
  stk$`1`@m[]             <- 1
  flt$A$`1`@landings.n[]  <- 1
  flt$A$`1`@landings.wt[] <- 1
  flt$A$`1`@discards.n[]  <- 1
  flt$A$`1`@discards.wt[] <- 1
  flt$A$`1`@catch.q <- FLPar(1, 
                             dimnames=list(params=c('alpha','beta'), 
                                           year = dimnames(flq)$year, 
                                           iter = dimnames(flq)$iter),
                             units='NA')
  flt$A$`1`@catch.sel[] <- 1
  flt$A@effort[] <- 1
  
  idx$`1`$X@index[]   <- 1
  idx$`1`$X@index.q[] <- 0.5
  range(idx$`1`$X)[c("startf", "endf")] <- c(1,1)
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  idx0 <- calculateSurvey(stk = stk,
                          flt = flt,
                          idx = idx, use_q = TRUE,
                          use_time = TRUE)
  
  expect_equal(sum(idx0$`1`$X@index), 0.5*1*exp(-1*(1+1))*10)
})
