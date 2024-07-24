## ============================================================================#
## Test forward projection function (single stock system)
## ============================================================================#

test_that("forward projection module for single-stock works", {
  
  ## Generate token OM & estimated stock
  ## --------------------------------------------------------------------------#
  flq <- FLCore::FLQuant(1, 
                         dim = c(3,10,1,1,1,1), 
                         dimnames = list(age  = c("1","2","3"),
                                         year = c("1","2","3","4","5","6","7","8","9","10")))
  flt <- FLFishery::FLFishery(i = FLFishery::FLCatch(flq))
  om  <- list(stks = FLCore::FLBiols(i = FLCore::FLBiol(flq)),
              flts = FLFishery::FLFisheries(list(A = flt)))
  
  ## Generate names
  om$stks$i@name <- "i"
  
  ## Fill necessary slots
  om$stks$i@m[] <- 0.2
  om$stks$i@mat$mat[] <- 0; om$stks$i@mat$mat[2:3,] <- 1
  om$stks$i@wt[]  <- 1
  om$stks$i@spwn[] <- 0
  om$stks$i@rec <- predictModel(model="geomean", params=FLPar(a=100, iter = 1))

  om$flts$A$i@landings.n[] <- 0.5
  om$flts$A$i@discards.n[] <- 0.5
  om$flts$A$i@landings.wt[] <- 1
  om$flts$A$i@discards.wt[] <- 1
  om$flts$A$i@catch.q <- FLPar(1, 
                               dimnames=list(params=c('alpha','beta'), 
                                             year = dimnames(flq)$year, 
                                             iter = dimnames(flq)$iter),
                               units='NA')
  om$flts$A$i@catch.q["beta"] <- 0
  om$flts$A$i@catch.sel[] <- c(0.1, 0.3, 0.6)
  om$flts$A@effort[] <- 1
  attr(om$flts$A$i,"quotashare") <- quantSums(flq)/3

  ## Initial numbers
  om$stks$i@n[1,1] <- 100
  om$stks$i@n[2,1] <- 50
  om$stks$i@n[3,1] <- 25
  
  ctrlArgs <- lapply(1, function(x) {
    list(year = 2:9,
         quant = "effort",
         fishery = "A",
         value = 1)
  })
  om_fwd <- FLasher::fwd(om$stks, om$flts, control = do.call(FLasher::fwdControl, ctrlArgs))
  
  ## assign projected values
  om$stks$i@n[,2:9]            <- om_fwd$biols$i@n[,2:9]
  om$flts$A$i@landings.n[,2:8] <- om_fwd$fisheries$A$i@landings.n[,2:8]
  om$flts$A$i@discards.n[,2:8] <- om_fwd$fisheries$A$i@discards.n[,2:8]
  
  ## Generate additional arguments
  ## --------------------------------------------------------------------------#
  
  advice <- 10
  
  args <- list(ay = 9,
               iy = 9,
               frq = 1,
               fy = 10,
               management_lag = 1,
               frange = list(i = c(1,3)))
  tracking  <- makeTracking(om, c("9","10"))
  tracking$i$advice[1, "9",] <- advice
  
  adviceType <- "catch"
  effortType <- matrix("min", nrow = 1, ncol = length(args$iy:args$fy), 
                       dimnames = list(flt = "A", year = args$iy:args$fy))
  exceptions <- matrix(1, dimnames = list(stk = "i",
                                          flt = "A"))
  multiplier <- matrix(1, dimnames = list(stk = "i",
                                          flt = "A"))
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  # undebug(fwdMixME)
  # undebug(FLasher::fwd, signature = signature(object = "FLBiols", fishery = "FLFisheries", control = "fwdControl"))
  # undebug(effortBaranov)
  out1 <- fwdMixME(om           = om,
                   args         = args,
                   tracking     = tracking,
                   sr_residuals = NULL, # list or FLQuants of recruitment residuals
                   proc_res     = NULL, # where is process error noise stored?
                   adviceType   = adviceType,
                   effortType   = effortType,
                   exceptions   = exceptions,
                   multiplier   = multiplier,
                   effort_max   = 100)
  
  ## Check performance
  ## --------------------------------------------------------------------------#
  
  ## check structure
  expect_type(out1, "list")
  expect_type(out1$om, "list")
  expect_type(out1$om$stks, "list")
  expect_type(out1$om$flts, "list")
  expect_type(out1$om$stks$i, "S4")
  expect_type(out1$om$flts$A, "list")
  expect_type(out1$om$flts$A$i, "S4")
  
  expect_type(out1$tracking, "list")
  expect_type(out1$tracking$i, "list")
  expect_equal(class(out1$tracking$i$stk)[1], "FLQuant")
  
  ## check content - tracking
  expect_equal(out1$tracking$choke[1,"9",], 1)            # choke
  expect_equal(out1$tracking$overquota[1,,"9",], 0)       # overquota
  expect_equal(round(out1$tracking$uptake[1,,"9",],5), 0) # uptake
  expect_equal(round(out1$tracking$optim[1,"9",],5), 0)   # optim - objective
  expect_equal(out1$tracking$optim[2,"9",], 0)            # optim - convergence
  expect_equal(out1$tracking$iterfail[1,1], 0)            # iterfail
  expect_equal(out1$tracking$quota[,,"9",], advice)       # quota
  
  expect_equal(out1$tracking$i$advice[1,"9",1], advice)   # advice
  expect_equal(c(out1$tracking$i$sel_om[,"9"])/sum(c(out1$tracking$i$sel_om[,"9"])), c(0.1,0.3, 0.6)) # selectivity
  expect_equal(c(out1$tracking$i$stk["F.om", "9"]),
               mean((out1$om$flts$A$i@catch.q["alpha","9"] * out1$om$flts$A@effort[,"9"]) %*% out1$om$flts$A$i@catch.sel[,"9"]))
  expect_equal(c(out1$tracking$i$stk["B.om", "9"]), c(tsb(om$stks$i)[,"9"]))
  expect_equal(c(out1$tracking$i$stk["SB.om","9"]), c(FLCore::ssb(out1$om$stks$i)[,"9"]))
  expect_equal(c(out1$tracking$i$stk["C.om", "9"]), c(FLCore::catch(out1$om$flts$A$i)[,"9"]))
  expect_equal(c(out1$tracking$i$stk["L.om", "9"]), c(FLCore::landings(out1$om$flts$A$i)[,"9"]))
  expect_equal(c(out1$tracking$i$stk["D.om", "9"]), c(FLCore::discards(out1$om$flts$A$i)[,"9"]))
  
  ## check content - operating model catches
  expect_equal(c(FLCore::catch(out1$om$flts$A$i)[,"9"]), 10)
  expect_equal(c(FLCore::landings(out1$om$flts$A$i)[,"9"]), 5)
  expect_equal(c(FLCore::discards(out1$om$flts$A$i)[,"9"]), 5)
  
  ## check content - operating model survivors
  expect_equal(c(out1$om$stks$i@n[2,"10"]), 
               c(out1$om$stks$i@n[1,"9"] * 
                   exp(-(0.2 + (out1$om$flts$A$i@catch.q["alpha","9"] * out1$om$flts$A@effort[,"9"]) %*% out1$om$flts$A$i@catch.sel[1,"9"]))))
  expect_equal(c(out1$om$stks$i@n[3,"10"]), 
               c(out1$om$stks$i@n[2,"9"] * 
                   exp(-(0.2 + (out1$om$flts$A$i@catch.q["alpha","9"] * out1$om$flts$A@effort[,"9"]) %*% out1$om$flts$A$i@catch.sel[2,"9"]))) +
               c(out1$om$stks$i@n[3,"9"] * 
                   exp(-(0.2 + (out1$om$flts$A$i@catch.q["alpha","9"] * out1$om$flts$A@effort[,"9"]) %*% out1$om$flts$A$i@catch.sel[3,"9"]))))
  
})

## ============================================================================#
## Test forward projection function (multi stock system)
## ============================================================================#

test_that("forward projection module for multi-stock works", {
  
  ## Generate token OM & estimated stock
  ## --------------------------------------------------------------------------#
  flq <- FLCore::FLQuant(1, 
                         dim = c(3,10,1,1,1,1), 
                         dimnames = list(age  = c("1","2","3"),
                                         year = c("1","2","3","4","5","6","7","8","9","10")))
  flt <- FLFishery::FLFishery(i = FLFishery::FLCatch(flq),
                              j = FLFishery::FLCatch(flq))
  om  <- list(stks = FLCore::FLBiols(i = FLCore::FLBiol(flq),
                                     j = FLCore::FLBiol(flq)),
              flts = FLFishery::FLFisheries(list(A = flt)))
  
  ## Generate names
  om$stks$i@name <- "i"
  om$stks$j@name <- "j"
  
  ## Fill necessary slots
  om$stks$i@m[] <- 0.2
  om$stks$i@mat$mat[] <- 0; om$stks$i@mat$mat[2:3,] <- 1
  om$stks$i@wt[]  <- 1
  om$stks$i@spwn[] <- 0
  om$stks$i@rec <- predictModel(model="geomean", params=FLPar(a=100, iter = 1))
  
  om$stks$j@m[] <- 0.2
  om$stks$j@mat$mat[] <- 0; om$stks$j@mat$mat[2:3,] <- 1
  om$stks$j@wt[]  <- 1
  om$stks$j@spwn[] <- 0
  om$stks$j@rec <- predictModel(model="geomean", params=FLPar(a=100, iter = 1))
  
  om$flts$A$i@landings.n[] <- 0.5; om$flts$A$j@landings.n[] <- 0.5
  om$flts$A$i@discards.n[] <- 0.5; om$flts$A$j@discards.n[] <- 0.5
  om$flts$A$i@landings.wt[] <- 1;  om$flts$A$j@landings.wt[] <- 1
  om$flts$A$i@discards.wt[] <- 1;  om$flts$A$j@discards.wt[] <- 1
  om$flts$A$i@catch.q <- FLPar(1, 
                               dimnames=list(params=c('alpha','beta'), 
                                             year = dimnames(flq)$year, 
                                             iter = dimnames(flq)$iter),
                               units='NA')
  om$flts$A$j@catch.q <- FLPar(0.5, 
                               dimnames=list(params=c('alpha','beta'), 
                                             year = dimnames(flq)$year, 
                                             iter = dimnames(flq)$iter),
                               units='NA')
  om$flts$A$i@catch.q["beta"] <- 0
  om$flts$A$j@catch.q["beta"] <- 0
  om$flts$A$i@catch.sel[] <- c(0.1, 0.3, 0.6)
  om$flts$A$j@catch.sel[] <- c(0.1, 0.3, 0.6)
  om$flts$A@effort[] <- 1
  attr(om$flts$A$i,"quotashare") <- quantSums(flq)/3
  attr(om$flts$A$j,"quotashare") <- quantSums(flq)/3
  
  ## Initial numbers
  om$stks$i@n[1,1] <- om$stks$j@n[1,1] <- 100
  om$stks$i@n[2,1] <- om$stks$j@n[2,1] <- 50
  om$stks$i@n[3,1] <- om$stks$j@n[3,1] <- 25
  
  ctrlArgs <- lapply(1, function(x) {
    list(year = 2:9,
         quant = "effort",
         fishery = "A",
         value = 1)
  })
  om_fwd <- FLasher::fwd(om$stks, om$flts, control = do.call(FLasher::fwdControl, ctrlArgs))
  
  om_fwd$biols$i@n
  om_fwd$biols$j@n
  catch(om_fwd$fisheries$A$i)
  catch(om_fwd$fisheries$A$j)
  
  ## assign projected values
  om$stks$i@n[,2:9]            <- om_fwd$biols$i@n[,2:9]
  om$stks$j@n[,2:9]            <- om_fwd$biols$j@n[,2:9]
  om$flts$A$i@landings.n[,2:8] <- om_fwd$fisheries$A$i@landings.n[,2:8]
  om$flts$A$j@landings.n[,2:8] <- om_fwd$fisheries$A$j@landings.n[,2:8]
  om$flts$A$i@discards.n[,2:8] <- om_fwd$fisheries$A$i@discards.n[,2:8]
  om$flts$A$j@discards.n[,2:8] <- om_fwd$fisheries$A$j@discards.n[,2:8]
  
  ## Generate additional arguments
  ## --------------------------------------------------------------------------#
  
  advice <- 10
  
  args <- list(ay = 9,
               iy = 9,
               frq = 1,
               fy = 10,
               management_lag = 1,
               frange = list(i = c(1,3),
                             j = c(1,3)))
  tracking  <- makeTracking(om, c("9","10"))
  tracking$i$advice[1, "9",] <- advice
  tracking$j$advice[1, "9",] <- advice
  
  adviceType <- "catch"
  effortType <- matrix("max", nrow = 1, ncol = length(args$iy:args$fy), 
                       dimnames = list(flt = "A", year = args$iy:args$fy))
  exceptions <- matrix(c(1,1), dimnames = list(stk = c("i","j"),
                                          flt = "A"))
  multiplier <- matrix(c(1,1), dimnames = list(stk = c("i","j"),
                                          flt = "A"))
  
  ## Run function
  ## --------------------------------------------------------------------------#
  
  # undebug(fwdMixME)
  # undebug(FLasher::fwd, signature = signature(object = "FLBiols", fishery = "FLFisheries", control = "fwdControl"))
  # undebug(effortBaranov)
  out1 <- fwdMixME(om           = om,
                   args         = args,
                   tracking     = tracking,
                   sr_residuals = NULL, # list or FLQuants of recruitment residuals
                   proc_res     = NULL, # where is process error noise stored?
                   adviceType   = adviceType,
                   effortType   = effortType,
                   exceptions   = exceptions,
                   multiplier   = multiplier,
                   effort_max   = 100)
  
  ## Check performance
  ## --------------------------------------------------------------------------#
  
  ## check structure
  expect_type(out1, "list")
  expect_type(out1$om, "list")
  expect_type(out1$om$stks, "list")
  expect_type(out1$om$flts, "list")
  expect_type(out1$om$stks$i, "S4")
  expect_type(out1$om$flts$A, "list")
  expect_type(out1$om$flts$A$i, "S4")
  
  expect_type(out1$tracking, "list")
  expect_type(out1$tracking$i, "list")
  expect_equal(class(out1$tracking$i$stk)[1], "FLQuant")
  
  ## check content - tracking
  expect_equal(out1$tracking$choke[1,"9",], 2)            # choke
  expect_equal(out1$tracking$overquota[1,,"9",], c(catch(out1$om$flts$A$i)[,"9"]) - advice)       # overquota
  
  expect_equal(out1$tracking$uptake[1,,"9",], advice - c(catch(out1$om$flts$A$i)[,"9"])) # uptake
  expect_equal(round(out1$tracking$uptake[2,,"9",],5), 0) # uptake
  
  expect_equal(round(out1$tracking$optim[1,"9",],5), 0)   # optim - objective
  expect_equal(out1$tracking$optim[2,"9",], 0)            # optim - convergence
  expect_equal(out1$tracking$iterfail[1,1], 0)            # iterfail
  expect_equal(out1$tracking$quota[,,"9",], c(i = advice,j = advice))       # quota
  
  expect_equal(out1$tracking$i$advice[1,"9",1], advice)   # advice
  expect_equal(out1$tracking$j$advice[1,"9",1], advice)   # advice
  
  expect_equal(c(out1$tracking$i$sel_om[,"9"])/sum(c(out1$tracking$i$sel_om[,"9"])), c(0.1,0.3, 0.6)) # selectivity
  expect_equal(c(out1$tracking$j$sel_om[,"9"])/sum(c(out1$tracking$j$sel_om[,"9"])), c(0.1,0.3, 0.6)) # selectivity
  
  expect_equal(c(out1$tracking$i$stk["F.om", "9"]),
               mean((out1$om$flts$A$i@catch.q["alpha","9"] * out1$om$flts$A@effort[,"9"]) %*% out1$om$flts$A$i@catch.sel[,"9"]))
  expect_equal(c(out1$tracking$i$stk["B.om", "9"]), c(tsb(om$stks$i)[,"9"]))
  expect_equal(c(out1$tracking$i$stk["SB.om","9"]), c(FLCore::ssb(out1$om$stks$i)[,"9"]))
  expect_equal(c(out1$tracking$i$stk["C.om", "9"]), c(FLCore::catch(out1$om$flts$A$i)[,"9"]))
  expect_equal(c(out1$tracking$i$stk["L.om", "9"]), c(FLCore::landings(out1$om$flts$A$i)[,"9"]))
  expect_equal(c(out1$tracking$i$stk["D.om", "9"]), c(FLCore::discards(out1$om$flts$A$i)[,"9"]))
  
  expect_equal(c(out1$tracking$j$stk["F.om", "9"]),
               mean((out1$om$flts$A$j@catch.q["alpha","9"] * out1$om$flts$A@effort[,"9"]) %*% out1$om$flts$A$j@catch.sel[,"9"]))
  expect_equal(c(out1$tracking$j$stk["B.om", "9"]), c(tsb(om$stks$j)[,"9"]))
  expect_equal(c(out1$tracking$j$stk["SB.om","9"]), c(FLCore::ssb(out1$om$stks$j)[,"9"]))
  expect_equal(c(out1$tracking$j$stk["C.om", "9"]), c(FLCore::catch(out1$om$flts$A$j)[,"9"]))
  expect_equal(c(out1$tracking$j$stk["L.om", "9"]), c(FLCore::landings(out1$om$flts$A$j)[,"9"]))
  expect_equal(c(out1$tracking$j$stk["D.om", "9"]), c(FLCore::discards(out1$om$flts$A$j)[,"9"]))
  
  ## check content - operating model catches
  expect_equal(c(FLCore::catch(out1$om$flts$A$j)[,"9"]), 10)
  expect_equal(c(FLCore::landings(out1$om$flts$A$j)[,"9"]), 5)
  expect_equal(c(FLCore::discards(out1$om$flts$A$j)[,"9"]), 5)
  
  ## check content - operating model survivors
  expect_equal(c(out1$om$stks$i@n[2,"10"]), 
               c(out1$om$stks$i@n[1,"9"] * 
                   exp(-(0.2 + (out1$om$flts$A$i@catch.q["alpha","9"] * out1$om$flts$A@effort[,"9"]) %*% out1$om$flts$A$i@catch.sel[1,"9"]))))
  expect_equal(c(out1$om$stks$i@n[3,"10"]), 
               c(out1$om$stks$i@n[2,"9"] * 
                   exp(-(0.2 + (out1$om$flts$A$i@catch.q["alpha","9"] * out1$om$flts$A@effort[,"9"]) %*% out1$om$flts$A$i@catch.sel[2,"9"]))) +
                 c(out1$om$stks$i@n[3,"9"] * 
                     exp(-(0.2 + (out1$om$flts$A$i@catch.q["alpha","9"] * out1$om$flts$A@effort[,"9"]) %*% out1$om$flts$A$i@catch.sel[3,"9"]))))
  
  expect_equal(c(out1$om$stks$j@n[2,"10"]), 
               c(out1$om$stks$j@n[1,"9"] * 
                   exp(-(0.2 + (out1$om$flts$A$j@catch.q["alpha","9"] * out1$om$flts$A@effort[,"9"]) %*% out1$om$flts$A$j@catch.sel[1,"9"]))))
  expect_equal(c(out1$om$stks$j@n[3,"10"]), 
               c(out1$om$stks$j@n[2,"9"] * 
                   exp(-(0.2 + (out1$om$flts$A$j@catch.q["alpha","9"] * out1$om$flts$A@effort[,"9"]) %*% out1$om$flts$A$j@catch.sel[2,"9"]))) +
                 c(out1$om$stks$j@n[3,"9"] * 
                     exp(-(0.2 + (out1$om$flts$A$j@catch.q["alpha","9"] * out1$om$flts$A@effort[,"9"]) %*% out1$om$flts$A$j@catch.sel[3,"9"]))))
  
})
