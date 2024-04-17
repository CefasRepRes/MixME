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
    xx <- as.FLStock(x, singlestock_MixME_om$flts$fleet)
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
