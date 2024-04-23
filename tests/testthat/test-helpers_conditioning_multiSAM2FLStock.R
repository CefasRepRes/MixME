test_that("conversion of SAM outputs into deterministic FLR stock objects", {
  
  ## v0.11.0 () - OK
  fit1 <- stockassessment::fitfromweb("HAD7bk_2020_Assessment")
  flr1 <- multiSAM2FLStock(fit1, useSAMcatch = TRUE, uncertainty = FALSE, niter = 1)
  
  ## check dimensions
  expect_equal(dims(flr1)$age, length(fit1$conf$minAge:fit1$conf$maxAge))
  expect_equal(dims(flr1)$year, fit1$data$noYears)
  
  ## check data
  expect_equal(c(fit1$data$natMor), c(t(flr1@m[,drop = TRUE])))
  expect_equal(c(fit1$data$propMat), c(t(flr1@mat[,drop = TRUE])))
  expect_equal(c(fit1$data$stockMeanWeight), c(t(flr1@stock.wt[,drop = TRUE])))
  expect_equal(c(fit1$data$landMeanWeight), c(t(flr1@landings.wt[,drop = TRUE])))
  expect_equal(c(fit1$data$catchMeanWeight), c(t(flr1@catch.wt[,drop = TRUE])))
  expect_equal(c(fit1$data$disMeanWeight), c(t(flr1@discards.wt[,drop = TRUE])))
  
  ## check estimates
  expect_equal(c(stockassessment::faytable(fit1)), c(t(flr1@harvest[drop = TRUE])))
  expect_equal(c(stockassessment::ntable(fit1)),   c(t(flr1@stock.n[drop = TRUE])))
  
  ## v0.11.0 (2023) - OK
  fit2 <- stockassessment::fitfromweb("NS_saithe_2023")
  flr2 <- multiSAM2FLStock(fit2, useSAMcatch = TRUE, uncertainty = FALSE, niter = 1)
  
  ## check dimensions
  expect_equal(dims(flr2)$age, length(fit2$conf$minAge:fit2$conf$maxAge))
  expect_equal(dims(flr2)$year, fit2$data$noYears)
  
  ## check data
  expect_equal(c(fit2$data$natMor), c(t(flr2@m[,drop = TRUE])))
  expect_equal(c(fit2$data$propMat), c(t(flr2@mat[,drop = TRUE])))
  expect_equal(c(fit2$data$stockMeanWeight), c(t(flr2@stock.wt[,drop = TRUE])))
  expect_equal(c(fit2$data$landMeanWeight), c(t(flr2@landings.wt[,drop = TRUE])))
  expect_equal(c(fit2$data$catchMeanWeight), c(t(flr2@catch.wt[,drop = TRUE])))
  expect_equal(c(fit2$data$disMeanWeight), c(t(flr2@discards.wt[,drop = TRUE])))
  
  ## check estimates
  expect_equal(c(stockassessment::faytable(fit2)), c(t(flr2@harvest[drop = TRUE])))
  expect_equal(c(stockassessment::ntable(fit2)),   c(t(flr2@stock.n[drop = TRUE])))
  
  ## v0.12.0 (2023) - OK
  fit3 <- stockassessment::fitfromweb("NShaddock_WGNSSK2023_Run1")
  flr3 <- multiSAM2FLStock(fit3, useSAMcatch = TRUE, uncertainty = FALSE, niter = 1)
  
  ## check dimensions
  expect_equal(dims(flr3)$age, length(fit3$conf$minAge:fit3$conf$maxAge))
  expect_equal(dims(flr3)$year, fit3$data$noYears)
  
  ## check data
  expect_equal(c(fit3$data$natMor),          c(t(flr3@m[,drop = TRUE])))
  expect_equal(c(fit3$data$propMat),         c(t(flr3@mat[,drop = TRUE])))
  expect_equal(c(fit3$data$stockMeanWeight), c(t(flr3@stock.wt[,drop = TRUE])))
  expect_equal(c(fit3$data$landMeanWeight),  c(t(flr3@landings.wt[,drop = TRUE])[-52,]))
  expect_equal(c(fit3$data$catchMeanWeight), c(t(flr3@catch.wt[,drop = TRUE])[-52,]))
  expect_equal(c(fit3$data$disMeanWeight),   c(t(flr3@discards.wt[,drop = TRUE])[-52,]))
  
  ## check estimates
  expect_equal(c(stockassessment::faytable(fit3)[-52,]), c(t(flr3@harvest[drop = TRUE])[-52,]))
  expect_equal(c(stockassessment::ntable(fit3)),   c(t(flr3@stock.n[drop = TRUE])))
  
  ## v0.12.0 (2024) - OK
  fit4 <- stockassessment::fitfromweb("sole20_24_2024")
  flr4 <- multiSAM2FLStock(fit4, useSAMcatch = TRUE, uncertainty = FALSE, niter = 1)
  
  ## check dimensions
  expect_equal(dims(flr4)$age, length(fit4$conf$minAge:fit4$conf$maxAge))
  expect_equal(dims(flr4)$year, fit4$data$noYears)
  
  ## check data
  expect_equal(c(fit4$data$natMor),          c(t(flr4@m[,drop = TRUE])))
  expect_equal(c(fit4$data$propMat),         c(t(flr4@mat[,drop = TRUE])))
  expect_equal(c(fit4$data$stockMeanWeight), c(t(flr4@stock.wt[,drop = TRUE])))
  expect_equal(c(fit4$data$landMeanWeight),  c(t(flr4@landings.wt[,drop = TRUE])))
  expect_equal(c(fit4$data$catchMeanWeight), c(t(flr4@catch.wt[,drop = TRUE])))
  expect_equal(c(fit4$data$disMeanWeight),   c(t(flr4@discards.wt[,drop = TRUE])))
  
  ## check estimates
  expect_equal(c(stockassessment::faytable(fit4)), c(t(flr4@harvest[drop = TRUE])))
  expect_equal(c(stockassessment::ntable(fit4)),   c(t(flr4@stock.n[drop = TRUE])))
})

test_that("conversion of SAM outputs into stochastic FLR stock objects", {
  
  ## Will not work with stockassessment version < 0.12.0
  
  ## v0.11.0 () - error in multiSAMvariates (reading minWeek)
  # fit1 <- stockassessment::fitfromweb("HAD7bk_2020_Assessment")
  # expect_error(multiSAM2FLStock(fit1, useSAMcatch = TRUE, uncertainty = TRUE, niter = 100))
  
  ## v0.11.0 (2023) - error in multiSAMvariates - (reading minWeek)
  # fit2 <- stockassessment::fitfromweb("NS_saithe_2023")
  # expect_error(multiSAM2FLStock(fit2, useSAMcatch = TRUE, uncertainty = TRUE, niter = 100))
  
  ## v0.12.0 (2023) - OK
  fit3 <- stockassessment::fitfromweb("NShaddock_WGNSSK2023_Run1")
  flr3 <- multiSAM2FLStock(fit3, useSAMcatch = TRUE, uncertainty = TRUE, niter = 100)
  
  ## Check dimensions
  expect_equal(dims(flr3)$age, length(fit3$conf$minAge:fit3$conf$maxAge))
  expect_equal(dims(flr3)$year, fit3$data$noYears)
  expect_equal(dims(flr3)$iter, 100)
  
  ## Check that values are stochastic
  expect_true(var(c(flr3@catch[1,1,1,1,1,])) > 0)
  expect_true(var(c(flr3@stock[1,1,1,1,1,])) > 0)
  expect_true(var(c(ssb(flr3)[1,1,1,1,1,])) > 0)
  
  ## v0.12.0 (2024) - OK
  fit4 <- stockassessment::fitfromweb("sole20_24_2024")
  flr4 <- multiSAM2FLStock(fit4, useSAMcatch = TRUE, uncertainty = TRUE, niter = 100)
  
  ## Check dimensions
  expect_equal(dims(flr4)$age, length(fit4$conf$minAge:fit4$conf$maxAge))
  expect_equal(dims(flr4)$year, fit4$data$noYears)
  expect_equal(dims(flr4)$iter, 100)
  
  ## Check that values are stochastic
  expect_true(var(c(flr4@catch[1,1,1,1,1,])) > 0)
  expect_true(var(c(flr4@stock[1,1,1,1,1,])) > 0)
  expect_true(var(c(ssb(flr4)[1,1,1,1,1,])) > 0)
  
})

