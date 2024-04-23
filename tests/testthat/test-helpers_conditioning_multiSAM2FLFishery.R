test_that("conversion of SAM outputs into deterministic FLR fishery objects", {

  ## v0.11.0 () - OK
  fit1 <- stockassessment::fitfromweb("HAD7bk_2020_Assessment")
  flr1 <- suppressWarnings(multiSAM2FLFishery(fit1, stkname = "had", useSAMcatch = TRUE, uncertainty = FALSE, niter = 1))
  
  ## check dimensions
  expect_equal(dims(flr1$`Residual catch`)$year, fit1$data$noYears)
  expect_equal(dims(flr1$`Residual catch`$had)$age, length(fit1$conf$minAge:fit1$conf$maxAge))
  expect_equal(dims(flr1$`Residual catch`$had)$year, fit1$data$noYears)
  
  ## check data
  expect_equal(c(fit1$data$landMeanWeight), c(t(flr1$`Residual catch`$had@landings.wt[,drop = TRUE])))
  expect_equal(c(fit1$data$disMeanWeight), c(t(flr1$`Residual catch`$had@discards.wt[,drop = TRUE])))
  
  ## check estimates
  expect_equal(c(stockassessment::faytable(fit1)), c(t(attr(flr1$`Residual catch`$had, "partF")[drop = TRUE])))
  
  ## v0.11.0 - OK
  fit2 <- stockassessment::fitfromweb("NS_saithe_2023")
  flr2 <- suppressWarnings(multiSAM2FLFishery(fit2, stkname = "saithe", useSAMcatch = TRUE, uncertainty = FALSE, niter = 1))
  
  ## check dimensions
  expect_equal(dims(flr2$`Residual catch`)$year, fit2$data$noYears)
  expect_equal(dims(flr2$`Residual catch`$saithe)$age, length(fit2$conf$minAge:fit2$conf$maxAge))
  expect_equal(dims(flr2$`Residual catch`$saithe)$year, fit2$data$noYears)
  
  ## check data
  expect_equal(c(fit2$data$landMeanWeight), c(t(flr2$`Residual catch`$saithe@landings.wt[,drop = TRUE])))
  expect_equal(c(fit2$data$disMeanWeight), c(t(flr2$`Residual catch`$saithe@discards.wt[,drop = TRUE])))
  
  ## check estimates
  expect_equal(c(stockassessment::faytable(fit2)), c(t(attr(flr2$`Residual catch`$saithe, "partF")[drop = TRUE])))
  
  ## v0.12.0 (2023) - OK
  fit3 <- stockassessment::fitfromweb("NShaddock_WGNSSK2023_Run1")
  flr3 <- suppressWarnings(multiSAM2FLFishery(fit3, stkname = "had", useSAMcatch = TRUE, uncertainty = FALSE, niter = 1))
  
  ## check dimensions
  expect_equal(dims(flr3$`Residual catch`)$year, fit3$data$noYears)
  expect_equal(dims(flr3$`Residual catch`$had)$age, length(fit3$conf$minAge:fit3$conf$maxAge))
  expect_equal(dims(flr3$`Residual catch`$had)$year, fit3$data$noYears)
  
  ## check data
  expect_equal(c(fit3$data$landMeanWeight), c(t(flr3$`Residual catch`$had@landings.wt[,drop = TRUE])[-52,]))
  expect_equal(c(fit3$data$disMeanWeight), c(t(flr3$`Residual catch`$had@discards.wt[,drop = TRUE])[-52,]))
  
  ## check estimates
  expect_equal(c(stockassessment::faytable(fit3)), c(t(attr(flr3$`Residual catch`$had, "partF")[drop = TRUE])))
  
  ## v0.12.0 (2024) - OK
  fit4 <- stockassessment::fitfromweb("sole20_24_2024")
  flr4 <- suppressWarnings(multiSAM2FLFishery(fit4, stkname = "sol", useSAMcatch = TRUE, uncertainty = FALSE, niter = 1))
  
  ## check dimensions
  expect_equal(dims(flr4$`Residual catch`)$year, fit4$data$noYears)
  expect_equal(dims(flr4$`Residual catch`$sol)$age, length(fit4$conf$minAge:fit4$conf$maxAge))
  expect_equal(dims(flr4$`Residual catch`$sol)$year, fit4$data$noYears)
  
  ## check data
  expect_equal(c(fit4$data$landMeanWeight), c(t(flr4$`Residual catch`$sol@landings.wt[,drop = TRUE])[-52,]))
  expect_equal(c(fit4$data$disMeanWeight), c(t(flr4$`Residual catch`$sol@discards.wt[,drop = TRUE])[-52,]))
  
  ## check estimates
  expect_equal(c(stockassessment::faytable(fit4)), c(t(attr(flr4$`Residual catch`$sol, "partF")[drop = TRUE])))
  
})

test_that("conversion of SAM outputs into stochastic FLR fishery objects", {
  
  ## v0.12.0 (2023) - OK
  fit3 <- stockassessment::fitfromweb("NShaddock_WGNSSK2023_Run1")
  flr3 <- suppressWarnings(multiSAM2FLFishery(fit3, stkname = "had", useSAMcatch = TRUE, uncertainty = TRUE, niter = 100))
  
  ## check dimensions
  expect_equal(dims(flr3$`Residual catch`)$year, fit3$data$noYears)
  expect_equal(dims(flr3$`Residual catch`)$iter, 100)
  expect_equal(dims(flr3$`Residual catch`$had)$age, length(fit3$conf$minAge:fit3$conf$maxAge))
  expect_equal(dims(flr3$`Residual catch`$had)$year, fit3$data$noYears)
  expect_equal(dims(flr3$`Residual catch`$had)$iter, 100)
  
  ## check data
  expect_equal(c(fit3$data$landMeanWeight), c(t(flr3$`Residual catch`$had@landings.wt[,drop = TRUE][,-52,1])))
  expect_equal(c(fit3$data$disMeanWeight),  c(t(flr3$`Residual catch`$had@discards.wt[,drop = TRUE][,-52,1])))
  
  ## Check that values are stochastic
  expect_true(var(c(catch(flr3$`Residual catch`$had)[1,1,1,1,1,])) > 0)
  expect_true(var(c(attr(flr3$`Residual catch`$had, "partF")[drop = TRUE][1,1,])) > 0)
  
  ## v0.12.0 (2024) - OK
  fit4 <- stockassessment::fitfromweb("sole20_24_2024")
  flr4 <- suppressWarnings(multiSAM2FLFishery(fit4, stkname = "sol", useSAMcatch = TRUE, uncertainty = TRUE, niter = 100))
  
  ## check dimensions
  expect_equal(dims(flr4$`Residual catch`)$year, fit4$data$noYears)
  expect_equal(dims(flr4$`Residual catch`)$iter, 100)
  expect_equal(dims(flr4$`Residual catch`$sol)$age, length(fit4$conf$minAge:fit4$conf$maxAge))
  expect_equal(dims(flr4$`Residual catch`$sol)$year, fit4$data$noYears)
  expect_equal(dims(flr4$`Residual catch`$sol)$iter, 100)
  
  ## check data
  expect_equal(c(fit4$data$landMeanWeight), c(t(flr4$`Residual catch`$sol@landings.wt[,drop = TRUE][,-52,1])))
  expect_equal(c(fit4$data$disMeanWeight),  c(t(flr4$`Residual catch`$sol@discards.wt[,drop = TRUE][,-52,1])))
  
  ## Check that values are stochastic
  expect_true(var(c(catch(flr4$`Residual catch`$sol)[1,1,1,1,1,])) > 0)
  expect_true(var(c(attr(flr4$`Residual catch`$sol, "partF")[drop = TRUE][2,1,])) > 0)
  
})