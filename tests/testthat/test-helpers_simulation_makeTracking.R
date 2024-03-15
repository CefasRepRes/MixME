test_that("tracking object is generated", {
  
  ## Generate inputs
  flq <- FLCore::FLQuant(1, 
                         dim = c(2,10,1,1,1,1), 
                         dimnames = list(age  = c("1","2"),
                                         year = c("1","2","3","4","5","6","7","8","9","10")))
  flt <- FLFishery::FLFishery(FLFishery::FLCatch(flq))
  om  <- list(stks = FLCore::FLBiols(FLCore::FLBiol(flq)),
              flts = FLFishery::FLFisheries(list(flt)))
  projyrs <- "10"
  
  ## Generate make tracking object
  tracking <- makeTracking(om, projyrs)
  
  ## check tracking object structure
  expect_type(tracking, "list")
  expect_named(tracking, c("1","quota","iterfail","optim","message","uptake","overquota","choke"))
  expect_named(tracking$`1`, c("advice", "sel_om", "sel_est", "stk"))
  expect_equal(dimnames(tracking$`1`$stk)$metric, c("F.om","B.om","SB.om","C.om","L.om","D.om",
                                                    "C.obs","L.obs","D.obs","conv.est","F.est",
                                                    "B.est","SB.est","C.est","L.est","D.est","hcr.adv"))
  
  ## check tracking object dimensions - age
  expect_equal(dimnames(tracking$`1`$sel_om)$age, dimnames(tracking$`1`$sel_est)$age)
  
  ## check tracking object dimensions - year
  expect_equal(dimnames(tracking$`1`$stk)$year, dimnames(tracking$`1`$advice)$year)
  expect_equal(dimnames(tracking$`1`$stk)$year, dimnames(tracking$quota)$year)
  expect_equal(dimnames(tracking$`1`$stk)$year, dimnames(tracking$iterfail)$year)
  expect_equal(dimnames(tracking$`1`$stk)$year, dimnames(tracking$optim)$year)
  expect_equal(dimnames(tracking$`1`$stk)$year, dimnames(tracking$message)$year)
  expect_equal(dimnames(tracking$`1`$stk)$year, dimnames(tracking$uptake)$year)
  expect_equal(dimnames(tracking$`1`$stk)$year, dimnames(tracking$overquota)$year)
  expect_equal(dimnames(tracking$`1`$stk)$year, dimnames(tracking$choke)$year)
  
  ## check tracking object dimensions - stocks
  expect_equal(dimnames(tracking$quota)$stk, dimnames(tracking$uptake)$stk)
  expect_equal(dimnames(tracking$quota)$stk, dimnames(tracking$overquota)$stk)
  
  ## check tracking object dimensions - fleets
  expect_equal(dimnames(tracking$quota)$flt, dimnames(tracking$uptake)$flt)
  expect_equal(dimnames(tracking$quota)$flt, dimnames(tracking$overquota)$flt)
  expect_equal(dimnames(tracking$quota)$flt, dimnames(tracking$choke)$flt)
  
  ## check tracking object dimensions - iteration
  expect_equal(dimnames(tracking$`1`$advice)$iter, dimnames(tracking$`1`$sel_om)$iter)
  expect_equal(dimnames(tracking$`1`$advice)$iter, dimnames(tracking$`1`$sel_est)$iter)
  expect_equal(dimnames(tracking$`1`$advice)$iter, dimnames(tracking$`1`$stk)$iter)
  expect_equal(dimnames(tracking$`1`$advice)$iter, dimnames(tracking$quota)$iter)
  expect_equal(dimnames(tracking$`1`$advice)$iter, dimnames(tracking$iterfail)$iter)
  expect_equal(dimnames(tracking$`1`$advice)$iter, dimnames(tracking$optim)$iter)
  expect_equal(dimnames(tracking$`1`$advice)$iter, dimnames(tracking$message)$iter)
  expect_equal(dimnames(tracking$`1`$advice)$iter, dimnames(tracking$uptake)$iter)
  expect_equal(dimnames(tracking$`1`$advice)$iter, dimnames(tracking$overquota)$iter)
  expect_equal(dimnames(tracking$`1`$advice)$iter, dimnames(tracking$choke)$iter)
  
})
