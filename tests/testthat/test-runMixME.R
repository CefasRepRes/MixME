## ============================================================================#
## Test conditioning simple single-stock Operating Model
## ============================================================================#

test_that("conditioning a simple single-stock Operating model works", {
  
  ## load libraries
  library(FLCore)
  library(FLFishery)
  library(mse)
  library(stockassessment)
  library(MixME)
  
  ## load example data
  data("singlestock_MixME_om")
  
  ## A list of stks (FLBiols) and flts (FLFisheries)
  expect_equal(summary(singlestock_MixME_om)["stks",1], "1")
  expect_equal(summary(singlestock_MixME_om)["stks",2], "FLBiols")
  expect_equal(summary(singlestock_MixME_om)["stks",3], "list")
  expect_equal(summary(singlestock_MixME_om)["flts",1], "1")
  expect_equal(summary(singlestock_MixME_om)["flts",2], "FLFisheries")
  expect_equal(summary(singlestock_MixME_om)["flts",3], "list")
  
  ## Dimensions for each stock and fleet
  expect_equal(dimnames(singlestock_MixME_om$stks[[1]])$year, as.character(1993:2019))
  expect_equal(dimnames(singlestock_MixME_om$flts[[1]])$year, as.character(1993:2019))
  expect_equal(dimnames(catch.q(singlestock_MixME_om$flts$fleet$had))$year, as.character(1993:2019))
  
  ## Check that stock and catch names match
  expect_true(names(singlestock_MixME_om$flts$fleet) %in% names(singlestock_MixME_om$stks))
  
  ## Check stock-recruit relationship
  expect_equal(c(singlestock_MixME_om$stks$had@rec@params), c(23.72114, 12397.40023))
  
  ## Calculate quota-share
  out <- calculateQuotashare(stks = singlestock_MixME_om$stks, 
                             flts = singlestock_MixME_om$flts, verbose = TRUE)
  singlestock_MixME_om$stks <- out$stks
  singlestock_MixME_om$flts <- out$flts
  
  ## Check 
  expect_true(all(c(attr(singlestock_MixME_om$flts$fleet$had, "quotashare")) == 1))
  
  out <- stfMixME(singlestock_MixME_om,
                  method = "yearMeans", 
                  nyears = 20, 
                  wts.nyears = 3, 
                  sel.nyears = 3, 
                  qs.nyears = 3, 
                  verbose = TRUE)
  
  ## Check projection
  expect_equal(dimnames(out$stks$had)$year, as.character(1993:2039))
  expect_equal(dimnames(out$flts$fleet)$year, as.character(1993:2039))
  
  ## Overwrite outputs
  singlestock_MixME_om$stks <- out$stks
  singlestock_MixME_om$flts <- out$flts
  
  ## Initial simulation year
  iy <- 2020
  
  ## Create an arbitrary forecast target for each fishery
  ctrlArgs <- lapply(1:length(singlestock_MixME_om$flts), function(x) {
    list(year = iy,
         quant = "effort",
         fishery = names(singlestock_MixME_om$flts)[x],
         value = 1)
  })
  
  ## make an FCB matricx
  ctrlArgs$FCB <- makeFCB(biols = singlestock_MixME_om$stks, flts = singlestock_MixME_om$flts)
  
  ## Generate effort-based FLasher::fwd forecast control
  flasher_ctrl <- do.call(FLasher::fwdControl, ctrlArgs)
  
  ## carry out the projection
  omfwd <- FLasher::fwd(object    = singlestock_MixME_om$stks, 
                        fishery   = singlestock_MixME_om$flts, 
                        control   = flasher_ctrl)
  
  ## assign the 
  singlestock_MixME_om$stks$had@n[,ac(iy)] <- omfwd$biols$had@n[,ac(iy)]
  
  ## convert FLBiol to FLStocks
  stk_oem <- FLStocks(lapply(singlestock_MixME_om$stks, function(x) {
    xx <- suppressWarnings(as.FLStock(x, singlestock_MixME_om$flts$fleet))
    stock.n(xx)[] <- NA
    stock(xx)[]   <- NA
    harvest(xx)[] <- NA
    return(xx)
  }))
  
  ## build MixME input object
  singlestock_MixME_input <- makeMixME(om = singlestock_MixME_om, 
                                       catch_obs = stk_oem, 
                                       management_lag = 0, 
                                       management_type = "fixedC", 
                                       parallel = FALSE)
  
  ## Stock estimation
  expect_equal(singlestock_MixME_input$ctrl_obj$est@args$estmethod$had, "perfectObs")
  
  ## Harvest control rule parameterisation
  expect_true(is.null(singlestock_MixME_input$ctrl_obj$phcr))
  
  ## Harvest control rule (update catch target)
  expect_equal(singlestock_MixME_input$ctrl_obj$hcr@args$hcrmethod$had, "hcrFixedC")
  expect_equal(singlestock_MixME_input$ctrl_obj$hcr@args$ctrg$had, 1)
  
  ## Advice implementation
  expect_true(is.null(singlestock_MixME_input$ctrl_obj$isys@args$isysmethod$had))
  
  ## Forward projection
  expect_true(is.null(singlestock_MixME_input$ctrl_obj$fwd@args$sr_residuals))
  expect_true(is.null(singlestock_MixME_input$ctrl_obj$fwd@args$proc_res))
  expect_equal(singlestock_MixME_input$ctrl_obj$fwd@args$adviceType, "catch")
  expect_equal(singlestock_MixME_input$ctrl_obj$fwd@args$effortType, "min")
  expect_equal(singlestock_MixME_input$ctrl_obj$fwd@args$exceptions, matrix(1, dimnames = list("had", "fleet")))
  expect_equal(singlestock_MixME_input$ctrl_obj$fwd@args$multiplier, matrix(1, dimnames = list("had", "fleet")))
  
  singlestock_MixME_input$ctrl_obj$hcr@args$ctrg$had <- 1000
  
  ## Full set of simulation arguments
  expect_equal(singlestock_MixME_input$args$fy, 2039)
  expect_equal(singlestock_MixME_input$args$y0, 1993)
  expect_equal(singlestock_MixME_input$args$iy, 2020)
  expect_equal(singlestock_MixME_input$args$management_lag, 0)
  expect_equal(singlestock_MixME_input$args$frq, 1)
  expect_equal(singlestock_MixME_input$args$parallel, FALSE)
  expect_equal(singlestock_MixME_input$args$nworkers, 1)
  expect_equal(singlestock_MixME_input$args$frange$had, c(NA,NA))
  expect_equal(singlestock_MixME_input$args$adviceInit$had, matrix(NA))
  expect_equal(singlestock_MixME_input$args$testfwd, FALSE)
  expect_equal(singlestock_MixME_input$args$maxRetry, 10)
  expect_equal(singlestock_MixME_input$args$useTMB, TRUE)
  expect_equal(singlestock_MixME_input$args$seed, NULL)
  expect_equal(singlestock_MixME_input$args$verbose, TRUE)
  
  ## Update fbar ranges
  singlestock_MixME_input$args$frange$had <- c("minfbar" = 3, "maxfbar" = 5)
  
  ## Update observation arguments
  singlestock_MixME_input$oem@args$catch_timing$had <- 0
  
  ## Full set of OEM inputs
  expect_equal(singlestock_MixME_input$oem@args$use_stk_oem, TRUE)
  expect_equal(singlestock_MixME_input$oem@args$use_catch_residuals, FALSE)
  expect_equal(singlestock_MixME_input$oem@args$use_idx_residuals, FALSE)
  expect_equal(singlestock_MixME_input$oem@args$use_om_weights, TRUE)
  expect_equal(singlestock_MixME_input$oem@args$catch_timing$had, 0)
  expect_equal(singlestock_MixME_input$oem@args$idx_timing, NULL)
  
  ## run the simulation
  res <- runMixME(om  = singlestock_MixME_input$om, 
                  oem = singlestock_MixME_input$oem,
                  ctrl_obj = singlestock_MixME_input$ctrl_obj,
                  args     = singlestock_MixME_input$args)
  
  ## Check outputs
  expect_equal(round(tail(c(quantSums(res$om$stks$had@n * 
                                        res$om$stks$had@wt * 
                                        res$om$stks$had@mat$mat)),1),1), 104341.1) # ssb
  expect_equal(round(tail(c(quantSums(res$om$flts$fleet$had@landings.n * 
                                        res$om$flts$fleet$had@landings.wt + 
                                        res$om$flts$fleet$had@discards.n * 
                                        res$om$flts$fleet$had@discards.wt)), 1),1), 1000) # catch
  expect_true(all(round(res$tracking$uptake[1,,,],3) == 0)) # uptake
  
})

## ============================================================================#
## Test conditioning simple mixed fishery Operating Model
## ============================================================================#

test_that("conditioning a simple mixed fishery Operating model works", {
  
  ## load libraries
  library(FLCore)
  library(FLFishery)
  library(mse)
  library(stockassessment)
  library(MixME)
  
  ## load example data
  data("mixedfishery_MixME_om")
  
  out <- calculateQuotashare(stks = mixedfishery_MixME_om$stks, flts = mixedfishery_MixME_om$flts, verbose = TRUE)
  mixedfishery_MixME_om$stks <- out$stks
  mixedfishery_MixME_om$flts <- out$flts
  
  out <- stfMixME(mixedfishery_MixME_om,
                  method = "yearMeans", 
                  nyears = 20, 
                  wts.nyears = 3, 
                  sel.nyears = 3, 
                  qs.nyears = 3, 
                  verbose = TRUE)
  
  ## Overwrite outputs
  mixedfishery_MixME_om$stks <- out$stks
  mixedfishery_MixME_om$flts <- out$flts
  
  ## calculate stock numbers in initial year
  ## initial projection year
  iy = 2020
  
  ## arbitrary effort-based target for each fleet
  ctrlArgs <- lapply(1:length(mixedfishery_MixME_om$flts), function(x) {
    list(year = iy,
         quant = "effort",
         fishery = names(mixedfishery_MixME_om$flts)[x],
         value = 1)
  })
  ctrlArgs$FCB <- makeFCB(biols = mixedfishery_MixME_om$stks, 
                          flts = mixedfishery_MixME_om$flts)
  
  ## Generate effort-based FLasher::fwd forecast control
  flasher_ctrl <- do.call(FLasher::fwdControl, ctrlArgs)
  
  omfwd <- FLasher::fwd(object    = mixedfishery_MixME_om$stks, 
                        fishery   = mixedfishery_MixME_om$flts, 
                        control   = flasher_ctrl)
  
  mixedfishery_MixME_om$stks$had@n[,ac(iy)] <- omfwd$biols$had@n[,ac(iy)]
  mixedfishery_MixME_om$stks$cod@n[,ac(iy)] <- omfwd$biols$cod@n[,ac(iy)]
  
  ## convert FLBiol to FLStocks
  stk_oem <- FLStocks(lapply(mixedfishery_MixME_om$stks, function(x) {
    
    ## identify where corresponding catch occurs in fleet structure
    catch <- sapply(mixedfishery_MixME_om$flts, function(y) which(names(y) %in% name(x)))
    
    ## coerce to FLStock
    xx <- suppressWarnings(as.FLStock(x, mixedfishery_MixME_om$flts, full = FALSE, catch = catch))
    
    ## remove excess data
    stock.n(xx)[] <- NA
    stock(xx)[]   <- NA
    
    ## return result
    return(xx)
  }))
  
  ## Convert FLBiol to FLStock to check if method works
  stk_oem2 <- FLStocks(lapply(mixedfishery_MixME_om$stks, function(x) {
    xx <- as(x, "FLStock")
    
    ## summarise slots
    xcatchn  <- Reduce("+", lapply(lapply(mixedfishery_MixME_om$flts, "[[", name(x)), catch.n))
    xcatchwt <- suppressWarnings(weighted.mean(FLQuants(lapply(lapply(mixedfishery_MixME_om$flts, "[[", name(x)), catch.wt)),
                                               FLQuants(lapply(lapply(mixedfishery_MixME_om$flts, "[[", name(x)), catch.n))))
    
    xlandingsn  <- Reduce("+", lapply(lapply(mixedfishery_MixME_om$flts, "[[", name(x)), landings.n))
    xlandingswt <- suppressWarnings(weighted.mean(FLQuants(lapply(lapply(mixedfishery_MixME_om$flts, "[[", name(x)), landings.wt)),
                                                  FLQuants(lapply(lapply(mixedfishery_MixME_om$flts, "[[", name(x)), landings.n))))
    
    xdiscardsn  <- Reduce("+", lapply(lapply(mixedfishery_MixME_om$flts, "[[", name(x)), discards.n))
    xdiscardswt <- suppressWarnings(weighted.mean(FLQuants(lapply(lapply(mixedfishery_MixME_om$flts, "[[", name(x)), discards.wt)),
                                                  FLQuants(lapply(lapply(mixedfishery_MixME_om$flts, "[[", name(x)), discards.n))))
    
    catch.n(xx)  <- xcatchn
    catch.wt(xx) <- xcatchwt
    catch(xx)    <- computeCatch(xx)
    
    landings.n(xx)  <- xlandingsn
    landings.wt(xx) <- xlandingswt
    landings(xx)    <- computeLandings(xx)
    
    discards.n(xx)  <- xdiscardsn
    discards.wt(xx) <- xdiscardswt
    discards(xx)    <- computeDiscards(xx)
    
    stock.n(xx)[] <- NA
    stock(xx)[]   <- NA
    
    harvest.spwn(xx)[] <- m(xx) %*% spwn(x)
    units(xx)[["harvest"]] <- "f"
    
    return(xx)
  }))
  
  ## check equivalence of method
  expect_equal(stk_oem, stk_oem2)
  
  ## assemble simulation inputs
  input <- makeMixME(om = mixedfishery_MixME_om, 
                     catch_obs = stk_oem, 
                     management_lag = 0, 
                     management_type = "fixedC", 
                     effort_type = "min",
                     parallel = FALSE)
  
  ## Update observation arguments
  input$oem@args$catch_timing$cod <- 0
  input$oem@args$catch_timing$had <- 0
  
  ## Update management arguments
  input$ctrl_obj$hcr@args$ctrg$cod <- 1000
  input$ctrl_obj$hcr@args$ctrg$had <- 1000
  
  ## Update simulation arguments
  input$args$iy
  
  ## Update fbar ranges
  input$args$frange$cod <- c("minfbar" = 2, "maxfbar" = 4)
  input$args$frange$had <- c("minfbar" = 3, "maxfbar" = 5)
  
  ## run the simulation
  res <- runMixME(om  = input$om, 
                  oem = input$oem,
                  ctrl_obj = input$ctrl_obj,
                  args     = input$args)
  
  # ## Check outputs
  expect_true(all(apply(res$tracking$iterfail, 1, mean) == 0))
  expect_true(all(res$tracking$optim["convergence",,] == 0))
  expect_true(all(round(res$tracking$optim["objective",,],5) == 0))
  expect_true(all(round(apply(res$tracking$quota, c(1,3), sum),5) == 1000))
  
  expect_true(all(round(res$tracking$overquota,3)["cod",,,1] == 0))
  expect_true(all(round(res$tracking$overquota,3)["had",,,1] == 0))
  
  expect_true(max(res$tracking$overquota, na.rm = TRUE) < 1e-5)
  
  expect_no_error(plot_timeseries_MixME(res, quantity = "ssb"))
  expect_no_error(plot_timeseries_MixME(res, quantity = "fbar"))
  expect_no_error(plot_timeseries_MixME(res, quantity = "catch"))
  expect_no_error(plot_timeseries_MixME(res, quantity = "uptake"))
  
  expect_true(all(res$tracking$choke == 1))
  
  expect_no_error(plot_timeseries_MixME(res, quantity = "effort", minyr = 2020))
  
  ## Add reference points
  hcrpars <- list(cod = c(Blim = 107000), 
                  had = c(Blim = 9227))
  res$ctrl_obj$phcr <- mseCtrl(args   = list(hcrpars = hcrpars))
  
  ## Plot risk time-series
  expect_no_error(plot_timeseries_MixME(res, quantity = "risk"))
  
})

## ============================================================================#
## Test running a simple mixed fishery constant catch simulation
## ============================================================================#

test_that("running a simple mixed fishery constant catch simulation works", {
  
  ## load libraries
  library(FLCore)
  library(FLFishery)
  library(mse)
  library(stockassessment)
  library(MixME)
  
  ## load example data
  data("mixedfishery_MixME_input")
  
  res <- runMixME(om  = mixedfishery_MixME_input$om, 
                  oem = mixedfishery_MixME_input$oem,
                  ctrl_obj = mixedfishery_MixME_input$ctrl_obj,
                  args     = mixedfishery_MixME_input$args)
  
  # ## Check outputs
  expect_true(all(apply(res$tracking$iterfail, 1, mean) == 0))
  expect_true(all(res$tracking$optim["convergence",,] == 0))
  expect_true(all(round(res$tracking$optim["objective",,],5) == 0))
  expect_true(all(round(apply(res$tracking$quota, c(1,3), sum),5) == 1000))
  
  expect_true(all(round(res$tracking$overquota,3)["cod",,,1] == 0))
  expect_true(all(round(res$tracking$overquota,3)["had",,,1] == 0))
  
  expect_true(max(res$tracking$overquota, na.rm = TRUE) < 1e-5)
  
  expect_no_error(plot_timeseries_MixME(res, quantity = "ssb"))
  expect_no_error(plot_timeseries_MixME(res, quantity = "fbar"))
  expect_no_error(plot_timeseries_MixME(res, quantity = "catch"))
  expect_no_error(plot_timeseries_MixME(res, quantity = "uptake"))
  
  expect_true(all(res$tracking$choke == 1))
  
  expect_no_error(plot_timeseries_MixME(res, quantity = "effort", minyr = 2020))
  
  ## Add reference points
  hcrpars <- list(cod = c(Blim = 107000), 
                  had = c(Blim = 9227))
  res$ctrl_obj$phcr <- mseCtrl(args   = list(hcrpars = hcrpars))
  
  ## Plot risk time-series
  expect_no_error(plot_timeseries_MixME(res, quantity = "risk"))
  
})

## ============================================================================#
## Test running a simple mixed fishery constant fishing mortality simulation
## ============================================================================#

test_that("running a simple mixed fisheriy constant F simulation works", {
  
  ## load libraries
  library(FLCore)
  library(FLFishery)
  library(mse)
  library(stockassessment)
  library(MixME)
  
  ## load example data
  data("mixedfishery_MixME_om")
  
  out <- calculateQuotashare(stks = mixedfishery_MixME_om$stks, flts = mixedfishery_MixME_om$flts, verbose = TRUE)
  mixedfishery_MixME_om$stks <- out$stks
  mixedfishery_MixME_om$flts <- out$flts
  
  out <- stfMixME(mixedfishery_MixME_om,
                  method = "yearMeans", 
                  nyears = 20, 
                  wts.nyears = 3, 
                  sel.nyears = 3, 
                  qs.nyears = 3, 
                  verbose = TRUE)
  ## Overwrite outputs
  mixedfishery_MixME_om$stks <- out$stks
  mixedfishery_MixME_om$flts <- out$flts
  
  ## initial projection year
  iy = 2020
  
  ## arbitrary effort-based target for each fleet
  ctrlArgs <- lapply(1:length(mixedfishery_MixME_om$flts), function(x) {
    list(year = iy,
         quant = "effort",
         fishery = names(mixedfishery_MixME_om$flts)[x],
         value = 1)
  })
  ctrlArgs$FCB <- makeFCB(biols = mixedfishery_MixME_om$stks, 
                          flts = mixedfishery_MixME_om$flts)
  
  ## Generate effort-based FLasher::fwd forecast control
  flasher_ctrl <- do.call(FLasher::fwdControl, ctrlArgs)
  
  omfwd <- FLasher::fwd(object    = mixedfishery_MixME_om$stks, 
                        fishery   = mixedfishery_MixME_om$flts, 
                        control   = flasher_ctrl)
  
  mixedfishery_MixME_om$stks$had@n[,ac(iy)] <- omfwd$biols$had@n[,ac(iy)]
  mixedfishery_MixME_om$stks$cod@n[,ac(iy)] <- omfwd$biols$cod@n[,ac(iy)]
  
  ## convert FLBiol to FLStocks
  stk_oem <- FLStocks(lapply(mixedfishery_MixME_om$stks, function(x) {
    
    ## identify where corresponding catch occurs in fleet structure
    catch <- sapply(mixedfishery_MixME_om$flts, function(y) which(names(y) %in% name(x)))
    
    ## coerce to FLStock
    xx <- as.FLStock(x, mixedfishery_MixME_om$flts, catch = catch)
    
    ## remove excess data
    stock.n(xx)[] <- NA
    stock(xx)[]   <- NA
    harvest(xx)[] <- NA
    
    ## return result
    return(xx)
  }))
  
  input <- makeMixME(om = mixedfishery_MixME_om, 
                     catch_obs = stk_oem, 
                     management_lag = 0, 
                     management_type = "fixedF", 
                     parallel = FALSE)
  
  ## observation error model
  input$oem@args$catch_timing$cod <- 0
  input$oem@args$catch_timing$had <- 0
  
  input$oem@observations$stk$cod@range[c("minfbar","maxfbar")] <- c(2,4)
  input$oem@observations$stk$had@range[c("minfbar","maxfbar")] <- c(3,5)
  
  ## check estimation module
  input$ctrl_obj$est@args$estmethod$cod
  input$ctrl_obj$est@args$estmethod$had
  
  ## update HCR
  input$ctrl_obj$hcr@args$hcrmethod$cod
  input$ctrl_obj$hcr@args$hcrmethod$had
  
  input$ctrl_obj$hcr@args$ftrg$cod <- 0.28  # use MSY f-target
  input$ctrl_obj$hcr@args$ftrg$had <- 0.353 # use MSY f-target
  
  ## Check initial projection year
  input$args$iy
  
  ## Update fbar ranges
  input$args$frange$cod <- c("minfbar" = 2, "maxfbar" = 4)
  input$args$frange$had <- c("minfbar" = 3, "maxfbar" = 5)
  
  ## run simulation
  res <- runMixME(om  = input$om, 
                  oem = input$oem,
                  ctrl_obj = input$ctrl_obj,
                  args     = input$args)
  
  # ## Check outputs
  expect_true(all(apply(res$tracking$iterfail, 1, mean) == 0))
  expect_true(all(round(res$tracking$optim["objective",,],5) == 0))
  
  ## Check quota uptake
  expect_true(all(round(res$tracking$uptake["cod",,,1], 5) == 0))
  expect_true(all(round(res$tracking$uptake["had",,,1], 5) > 0))
  
  ## Check maximum overshoot
  expect_true(round(max(res$tracking$overquota, na.rm = TRUE),5) == 0)
  
  expect_no_error(plot_timeseries_MixME(res, quantity = "ssb"))
  expect_no_error(plot_timeseries_MixME(res, quantity = "fbar", minyr = 2020))
  expect_no_error(plot_timeseries_MixME(res, quantity = "catch"))
  expect_no_error(plot_timeseries_MixME(res, quantity = "uptake"))
  
  ## Check choke stock
  expect_true(all(res$tracking$choke == 1))
  
  ## Check constant F
  expect_true(round(mean(res$om$flts$OTB_A@effort[,as.character(2021:2039)]),4) == 1.0976)
  
  ## plot fleet properties
  expect_no_error(plot_timeseries_MixME(res, quantity = "effort", minyr = 2020, maxyr = 2038))
  
  ## Add reference points
  hcrpars <- list(cod = c(Blim = 107000),# 
                  had = c(Blim = 9227))  # https://doi.org/10.17895/ices.advice.5897
  res$ctrl_obj$phcr <- mseCtrl(args   = list(hcrpars = hcrpars))
  
  ## Plot risk time-series
  expect_no_error(plot_timeseries_MixME(res, quantity = "risk"))
  
})

## ============================================================================#
## Test Error Catching
## ============================================================================#

test_that("runMixME catches errors", {
  
  # 0. No OM 
  # 1. Wrong OM names
  # 2. Missing stock names
  # 3. Missing fleet names
  # 4. Mismatches stock and fleet catch names
  # 5. missing quotashare
  # 6. catchability is not year-resolved
  
  ## load dataset
  data("mixedfishery_MixME_input")
  
  ## No OM
  t0 <- mixedfishery_MixME_input
  expect_error(runMixME(NULL, t0$oem, t0$ctrl_obj, t0$args),
               regexp = "'om' must contain stock and fleet data in 'stks' and 'flts' respectively")
  
  ## Wrong OM names
  t1 <- mixedfishery_MixME_input$om
  names(t1) <- c("biols","fisheries")
  expect_error(runMixME(t1, t0$oem, t0$ctrl_obj, t0$args),
               regexp = "'om' must contain stock and fleet data in 'stks' and 'flts' respectively")
  
  ## Missing stock names
  t2 <- mixedfishery_MixME_input$om
  names(t2$stks) <- NULL
  expect_error(runMixME(t2, t0$oem, t0$ctrl_obj, t0$args), regexp = "stocks in 'stks' must be named")
  names(t2$stks) <- NA
  expect_error(runMixME(t2, t0$oem, t0$ctrl_obj, t0$args), regexp = "stocks in 'stks' must be named")
  
  ## Missing fleet names
  t3 <- mixedfishery_MixME_input$om
  names(t3$flts) <- NULL
  expect_error(runMixME(t3, t0$oem, t0$ctrl_obj, t0$args), regexp = "fleets in 'flts' must be named")
  names(t3$flts) <- NA
  expect_error(runMixME(t3, t0$oem, t0$ctrl_obj, t0$args), regexp = "fleets in 'flts' must be named")
  
  ## Mismatched stocks and fleet catches names
  t4 <- mixedfishery_MixME_input$om
  names(t4$flts$OTB_A) <- c("A_cod", "A_had")
  expect_error(runMixME(t4, t0$oem, t0$ctrl_obj, t0$args), regexp = "stock names in 'stks' and catches names in 'flts' must match")
  t4 <- mixedfishery_MixME_input$om
  names(t4$stks) <- c("cd", "hd")
  expect_error(runMixME(t4, t0$oem, t0$ctrl_obj, t0$args), regexp = "stock names in 'stks' and catches names in 'flts' must match")
  
  ## Missing quota-share
  t5 <- mixedfishery_MixME_input$om
  attr(t5$flts$OTB_A$cod,"quotashare") <- NULL
  expect_error(runMixME(t5, t0$oem, t0$ctrl_obj, t0$args), regexp = "each FLCatch must have an FLQuant attached as an attibute named 'quotashare'")
  
  ## Catchability is not year-resolved
  t6 <- mixedfishery_MixME_input$om
  t6$flts$OTB_B$cod@catch.q <- FLPar(c(1,0), dimnames = list(params = c("alpha","beta"),
                                                             iter   = 1))
  expect_error(runMixME(t6, t0$oem, t0$ctrl_obj, t0$args), regexp = "catchability 'catch.q' must contain a year dimension")
  
  ## NA in fleet efforts
  t7 <- mixedfishery_MixME_input$om
  t7$flts$OTB_A@effort[] <- NA
  expect_error(runMixME(t7, t0$oem, t0$ctrl_obj, t0$args), 
               regexp = "fleet effort slots cannot contain NA values in the projection period")
  
  ## Missing simulation arguments
  t8 <- mixedfishery_MixME_input$args; t8$fy <- NULL;     expect_error(runMixME(t0$om, t0$oem, t0$ctrl_obj, t8), "final year 'fy' missing in 'args'.")
  t8 <- mixedfishery_MixME_input$args; t8$iy <- NULL;     expect_error(runMixME(t0$om, t0$oem, t0$ctrl_obj, t8), "Intermediate year 'iy' missing in 'args'.")
  t8 <- mixedfishery_MixME_input$args; t8$frange <- NULL; expect_error(runMixME(t0$om, t0$oem, t0$ctrl_obj, t8), "fishing mortality range 'frange' missing in 'args'")
  t8 <- mixedfishery_MixME_input$args; t8$management_lag <- NULL; expect_error(runMixME(t0$om, t0$oem, t0$ctrl_obj, t8), "management lag 'management_lag' missing in 'args'")
  
  ## Wrong simulation arguments
  t9 <- mixedfishery_MixME_input$args
  t9$fy <- 2019; expect_error(runMixME(t0$om, t0$oem, t0$ctrl_obj, t9), "Final year 'fy' must be greater than intermediate year 'iy'")
  
})
