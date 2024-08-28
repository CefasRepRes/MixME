test_that("fa_cpp works", {
  
  ## load libraries
  library(MixME)
  
  ## load example data
  data("mixedfishery_MixME_om")
  
  ## Array for input
  arr <- array(0,
               dim = c(dim(mixedfishery_MixME_om$stks$cod@n), 
                       length(mixedfishery_MixME_om$flts)),
               dimnames = c(dimnames(mixedfishery_MixME_om$stks$cod@n),
                            list(flt = names(mixedfishery_MixME_om$flts))))
  
  ## fun function
  fa <- MixME:::fa_cpp(arr, mixedfishery_MixME_om$flts, stockname = "cod")
  
  ## check that matches manual calculation
  expect_equal(unname(fa[,"2019",,,,,1]),  
               c(mixedfishery_MixME_om$flts$OTB_A$cod@catch.q["alpha","2019"] * 
                   mixedfishery_MixME_om$flts$OTB_A@effort[,"2019"] %*%
                   mixedfishery_MixME_om$flts$OTB_A$cod@catch.sel[,"2019"]))
  
  ## check that function work with truncated input
  fa2 <- MixME:::fa_cpp(arr[,"2019",,,,,1,drop=FALSE], mixedfishery_MixME_om$flts, stockname = "cod")
  expect_equal(fa[,"2019",,,,,1], 
               fa2[,,,,,,1])
})
