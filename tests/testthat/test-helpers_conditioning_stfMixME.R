test_that("extending years works", {
  
  om0 <- calculateQuotashare(singlestock_MixME_om$stks,
                             singlestock_MixME_om$flts)
  om1 <- stfMixME(om0)
  
  expect_equal(dim(om1$stks$had@wt)[2], dim(om0$stks$had@wt)[2]+3)
  
})
