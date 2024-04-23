test_that("conversion of SAM outputs into stochastic FLR fleet and stock objects", {
  
  ## v0.12.0 (2023) - OK
  fit1 <- stockassessment::fitfromweb("NShaddock_WGNSSK2023_Run1")
  flr1 <- multiSAM2FLR(fit1, stkname = "had", useSAMcatch = TRUE, uncertainty = TRUE, niter = 100)

  ## Check structure
  expect_equal(class(flr1), "list")
  expect_equal(names(flr1), c("stks","flts","idxs"))
  expect_equal(class(flr1$stks)[1], "FLStocks")
  expect_equal(class(flr1$flts)[1], "FLFisheries")
  expect_equal(class(flr1$idxs), "list")
  
  expect_equal(names(flr1$stks), names(flr1$flts$`Residual catch`))
  
  ## Check dimensions
  expect_equal(dimnames(flr1$stks$had)$age,  dimnames(flr1$flts$`Residual catch`$had)$age)
  expect_equal(dimnames(flr1$stks$had)$year, dimnames(flr1$flts$`Residual catch`$had)$year)
  expect_equal(dimnames(flr1$stks$had)$iter, dimnames(flr1$flts$`Residual catch`$had)$iter)
  
  ## check content - expect consistent catch and F values
  expect_equal(c(t(flr1$stks$had@catch.n[,,,,,1,drop = TRUE])[-52,]), c(stockassessment::caytable(fit1)[-52,]))
  expect_equal(c(t(catch.n(flr1$flts$`Residual catch`$had)[,,,,,1,drop = TRUE])[-52,]), c(stockassessment::caytable(fit1)[-52,]))
  expect_equal(c(flr1$stks$had@catch[,,,,,1])[-52],              unname(stockassessment::catchtable(fit1)[,1]))
  expect_equal(flr1$stks$had@harvest[,,,,,1,drop = TRUE][,-52], attr(flr1$flts$`Residual catch`$had, "partF")[,,,,,1, drop = TRUE][,-52])
  
  ## v0.12.0 (2024) - Error making FLFisheries
  fit2 <- stockassessment::fitfromweb("sole20_24_2024")
  flr2 <- multiSAM2FLR(fit2, stkname = "sole", useSAMcatch = TRUE, uncertainty = TRUE, niter = 100)
  
  ## Check structure
  expect_equal(class(flr2), "list")
  expect_equal(names(flr2), c("stks","flts","idxs"))
  expect_equal(class(flr2$stks)[1], "FLStocks")
  expect_equal(class(flr2$flts)[1], "FLFisheries")
  expect_equal(class(flr2$idxs), "list")
  
  expect_equal(names(flr2$stks), names(flr2$flts$`Residual catch`))
  
  ## Check dimensions
  expect_equal(dimnames(flr2$stks$sole)$age,  dimnames(flr2$flts$`Residual catch`$had)$age)
  expect_equal(dimnames(flr2$stks$sole)$year, dimnames(flr2$flts$`Residual catch`$had)$year)
  expect_equal(dimnames(flr2$stks$sole)$iter, dimnames(flr2$flts$`Residual catch`$had)$iter)
  
  ## check content - expect consistent catch and F values
  expect_equal(c(t(flr2$stks$sole@catch.n[,,,,,1,drop = TRUE])), c(stockassessment::caytable(fit2)))
  expect_equal(c(t(catch.n(flr2$flts$`Residual catch`$sole)[,,,,,1,drop = TRUE])), c(stockassessment::caytable(fit2)))
  expect_equal(c(flr2$stks$sole@catch[,,,,,1]),              unname(stockassessment::catchtable(fit2)[,1]))
  expect_equal(flr2$stks$sole@harvest[,,,,,1,drop = TRUE], attr(flr2$flts$`Residual catch`$sole, "partF")[,,,,,1, drop = TRUE])
  
})

