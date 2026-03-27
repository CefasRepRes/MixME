test_that("reconditioning years works", {
  
  om0_ref <- calculateQuotashare(singlestock_MixME_om$stks,
                                 singlestock_MixME_om$flts)
  
  y0 <- as.numeric(head(dimnames(om0_ref$stks$had)$year, 1))
  yr <- as.numeric(tail(dimnames(om0_ref$stks$had)$year, 1))
  
  om0 <- list(stks = FLCore::window(om0_ref$stks, y0, yr-1),
              flts = FLCore::window(om0_ref$flts, y0, yr-1))
  
  om1 <- MixME:::hindcast_stf(om0, 
                              om0_ref, 
                              c('catch.q','catch.sel','landfrac','landings.wt','discards.wt','effort-share'),
                              "fleet",
                              nyears = 1,
                              wts.nyears = 3,
                              sel.nyears = 3,
                              qs.nyears = 3, 
                              verbose = FALSE)
  
  ## check year dimension
  expect_equal(dim(om1$stks$had@wt)[2], dim(om0_ref$stks$had@wt)[2])
  
  ## check that parameters have been correctly reconditioned
  
  ## catch selectivity
  expect_equal(c(om1$flts$fleet$had@catch.sel[,"2019"]),
               c(apply(om1$flts$fleet$had@catch.sel[,as.character(2016:2018)],c(1),mean)))
  
  ## landings fraction
  ln <- om1$flts$fleet$had@landings.n[,as.character(2016:2018)]
  cn <- om1$flts$fleet$had@landings.n[,as.character(2016:2018)] + 
    om1$flts$fleet$had@discards.n[,as.character(2016:2018)]
  lf <- ln/cn
  lf[is.na(lf)] <- 0
  
  expect_equal(c(om1$flts$fleet$had@landings.n[,"2019"]),
               c(apply(lf,c(1),mean)))
  
  ## landings weights
  expect_equal(c(om1$flts$fleet$had@landings.wt[,"2019"]),
               c(apply(om1$flts$fleet$had@landings.wt[,as.character(2016:2018)],c(1),mean)))
  
  ## discards weights
  dw <- om1$flts$fleet$had@discards.wt[,as.character(2016:2018)]
  dw[dw==0] <- NA
  expect_equal(c(om1$flts$fleet$had@discards.wt[,"2019"]),
               c(apply(dw,c(1),mean,na.rm=T)))
  
  ## effort-share
  expect_equal(c(om1$flts$fleet@effort[,"2019"]),
               c(apply(om1$flts$fleet@effort[,as.character(2016:2018)],c(1),mean)))
  
})
