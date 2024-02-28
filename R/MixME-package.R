#' MixME (Mixed-fishery Multi-stock Evaluation)
#' 
#' @description
#' An R package for mixed fisheries Management Strategy Evaluation (MSE) using multi-stock and multi-fleet Operating Models.
#' 
#' @details
#' MixME:
#' 
#'  - can carry out full MSE to evaluate the performance of Management Procedures.
#'  - can carry out smaller-scale simulations with perfect stock observations to evaluate advice rule behaviour.
#'    can accommodate a range of fleet and stock parameter uncertainties.
#'  - makes use of FLR libraries ([https://github.com/flr](https://github.com/flr))
#'  - has built-in helper functions to condition Operating Models from fitted SAM 
#'    (State-space Assessment Model; [https://github.com/fishfollower/SAM](https://github.com/fishfollower/SAM)) 
#'    models with uncertainty
#'  - has built-in diagnostic tools to check for errors occuring in the Management 
#'    Procedure (e.g. assessment model fitting errors) or the Operating Model 
#'    (e.g. fleet effort optimisation)
#' 
#' @name MixME-package
#' @aliases MixME
#' @useDynLib MixME
#' @useDynLib TMBobj
#' @importFrom Rcpp sourceCpp
#' @import TMB
#' @import foreach
#' @import FLCore
#' @import FLFishery
#' @import FLasher
#' @import mse

NULL