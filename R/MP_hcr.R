# ---
# title: 'Management Procedure - Harvest Control Rule methods'
# author: 'Matthew Pace'
# date: 'August 2022'
# ---
#
#' Initialise parameters for the ICES Harvest Control Rule
#'
#' Function takes a named list of Harvest Control Rule parameters for each stock
#' and coerces parameters into \code{FLPar} objects to be used by the Harvest Control
#' Rule.
#'
#' @param args named list of stocks. Each list element contains a named list
#'                of Harvest Control Rule parameters.
#'
#' @return a named list of stocks. Each list element contains an \code{FLPar}
#'         object for stock Harvest Control Rule parameters
#'
#' @export

phcrMixME <- function(stk, args, hcrpars, tracking) {

  ## Check to ensure hcrpars are provided
  if(!exists("hcrpars")){
    stop("phcr 'args' list must contain a list object named 'hcrpars'")
  }

  ## Checks to ensure that stocks and parameters are named
  if(is.null(names(hcrpars)))
    stop("'hcrpars' must contain contain named list of parameters nested by stock")

  ## Coerce parameters into FLPar
  hcrpars0 <- lapply(1:length(hcrpars), function(x) {
    do.call(FLPar, list(hcrpars[[x]]))})
  names(hcrpars0) <- names(hcrpars)

  ## return as list
  return(list(hcrpars = hcrpars0))
}

#' Advice rule implementation for stocks
#'
#' Applies a built-in or user-supplied advice rule for each stock to
#' generate single-stock advice. The advice rule may take the form of a
#' Harvest Control Rule that returns advice based on estimated stock status,
#' fixed catch advice or fixed fishing mortality advice.
#'
#' @param stk list of stocks
#' @param hcrpars (optional) Harvest Control Rule parameters for each stock. A named
#'                nested list. Each named list element corresponds to a stock
#'                and should contain a list of parameters for that stock.
#'                Defaults to \code{NULL}.
#' @param hcrmethod named list of advice rule methods for each stock.
#'                  List elements may be the name of an in-built advice rule
#'                  method or a user-supplied function that is applied to the
#'                  stock. Defaults to 'hcrICES'.
#' @param ctrg (optional) named list of target catch for each stock.
#' @param ftrg (optional) named list of target fishing mortality for each stock
#' @param tracking a named list of tracking objects to monitor emergent dynamic
#'                 properties.
#'
#' @export

hcrMixME <- function(stk,
                    args,
                    hcrpars = NULL,
                    hcrmethod = NULL,
                    ctrg = NULL,
                    ftrg = NULL,
                    tracking,
                    ... ) {

  ## extract timings
  ay   <- args$ay             # current (assessment) year
  mlag <- args$management_lag # lag between assessment year and advice year

  ## TO DO - How to apply a multistock harvest control rule? Cannot loop over
  ##         each stock in that case... How to handle a mix of single stock and
  ##         multistock harvest control rules?

  ## TO DO - How to handle stocks that are not assessed but are simply by-catch?

  ## Extract the methods to be used for each stock
  #hcrmethod <- args$hcrmethod

  ctrlList <- lapply(names(stk), function(x){

    ## IF NOT DONE PREVIOUSLY - NEED TO CLIP THE STOCK TO
    ## THE ADVICE YEAR - MIGHT BECOME REDUNDANT ONCE
    ## I HAVE A PERFECT OR IMPERFECT OBSERVATION MODEL

    stk0 <- window(stk[[x]], end = ay+mlag)

    ## Run user-supplied method (if provided)
    if(is.function(hcrmethod[[x]])){

      out <- do.call(hcrmethod[[x]],
                     list(stk      = stk0,
                          args     = args,
                          hcrpars  = hcrpars[[x]],
                          tracking = tracking[[x]]))

      ## Run ICES Harvest control Rule
    } else if (hcrmethod[[x]] == "hcrICES"){

      out <- hcrICES(stk      = stk0,
                     args     = args,
                     hcrpars  = hcrpars[[x]],
                     tracking = tracking[[x]])

      ## Run Fixed F advice
    } else if (hcrmethod[[x]] == "hcrFixedF"){

      out <- mse::fixedF.hcr(stk      = stk0,
                             ftrg     = ftrg[[x]],
                             args     = args,
                             tracking = tracking[[x]])

      ## Run Fixed Catch advice
    } else if (hcrmethod[[x]] == "hcrFixedC"){

      out <- mse::fixedC.hcr(stk  = stk0,
                             ctrg = ctrg[[x]],
                             args = args,
                             tracking = tracking[[x]])

      # ctrl <- hcrFixedC(stk  = stk[[x]],
      #                   Ctrg = Ctrg[[x]],
      #                   args = args)

    } else {
      stop("Only user-supplied functions, 'hcrICES', 'hcrFixedC' and 'hcrFixedF' are currently supported")
    }

    # A bit of a hacky fix to flexibly handle control object structures.
    # 'value' is either a row or col name.

    ## Update tracking
    if("value" %in% rownames(out$ctrl@iters[,,])) { # ctrl structure from hcrICES
      out$tracking$advice[1,ac(args$ay),] <- out$ctrl@iters[,,]["value",]
    }
    if("value" %in% colnames(out$ctrl@iters[,,])) { # ctrl structure from fixedF
      out$tracking$advice[1,ac(args$ay),] <- out$ctrl@iters[,,][,"value"][1]
    }

    ## return control object
    return(out)
  })

  ## A bit of a shoddy way of re-organising our returned list
  ctrl <- vector(mode = "list", length = length(stk))
  for(x in 1:length(stk)) {

    ctrl[[x]]     <- ctrlList[[x]]$ctrl
    tracking[[x]] <- ctrlList[[x]]$tracking

  }
  names(ctrl) <- names(stk)

  ## return control
  return(list(ctrl = ctrl, tracking = tracking))
}

#' ICES Harvest Control Rule implementation
#'
#' Applies the ICES Harvest Control Rule to estimated stock properties for
#' each stock to generate single-stock advice.
#'
#' For the ICES Harvest Control Rule, the reference points (parameters) that
#' are used are $B_{trigger}$, $F_{target}$, $B_{pa}$, $F_{pa}$ and $B_{lim}$.
#'
#' @param stk Observed stocks. An object of class \code{FLStock}.
#' @param args Additional arguments.
#' @param hcrpars Harvest Control Rule reference points. An object of class \code{FLPar}.
#' @param tracking A named list of tracking objects to monitor emergent dynamic
#'                 properties
#'
#' @author Adapted from WKNSMSE (2019) code by S. H. Fischer.
#'
#' @return A control object containing the adjusted F target for the stock.
#'
#' @export

hcrICES <- function(stk, args, hcrpars, tracking) {

  ## Extract current (assessment) year
  ay <- args$ay

  ## Extract lag between assessment year and advice year
  mlag <- args$management_lag

  ## Extract stock dimensions
  ni <- dims(stk)[["iter"]]

  ## Extract reference points with the correct iteration dimension
  Ftrgt    <- propagate(FLPar(hcrpars["Ftrgt"]), ni)
  Btrigger <- propagate(FLPar(hcrpars["Btrigger"]), ni)

  ## Extract optional reference points
  if ("Blim" %in% dimnames(hcrpars)$params) {
    Blim   <- propagate(FLPar(hcrpars["Blim"]), ni)
  } else {
    Blim   <- propagate(FLPar(0), ni)
  }

  ## Evaluate SSB status
  status_Btrigger <- tail(ssb(stk), 1) / Btrigger # Ratio of SSB to Btrigger (in final year)
  status_Blim     <- tail(ssb(stk), 1) / Blim     # Ratio of SSB to Blim (in final year)

  ## Identify iterations where SSB is below reference points
  pos_Btrigger <- which(status_Btrigger > 1)
  pos_Blim     <- which(status_Blim < 1)

  # -----------------------------------------------------------------#
  # if SSB >= Btrigger;                F = Ftarget
  #    SSB < Btrigger & SSB >= Blim ;  F = Ftarget * (SSB / Btrigger)
  #    SSB < Blim;                     F = 0
  # -----------------------------------------------------------------#

  ## multiplier should not exceed 1
  Fmult <- status_Btrigger
  Fmult[,,,,, pos_Btrigger] <- 1

  ## Adjust F multiplier if SSB is below Blim
  Fmult[,,,,, pos_Blim] <- 0

  ## Generate new Ftarget
  Ftrgt <- Ftrgt * Fmult

  ## create ctrl object
  # ctrl <- mse::getCtrl(values = Ftrgt, quantity = "f", years = ay + mlag, it = ni)
  ctrl <- fwdControl(list(year = ay + mlag, quant = "fbar", value = Ftrgt))

  return(list(ctrl = ctrl, tracking = tracking))
}

#' Fixed Catch advice implementation
#'
#' Generates a control object to implement fixed Catch advice.
#'
#' @param stk  Observed stocks. An object of class \code{FLStock}.
#' @param Ctrg Fixed catch target. An object of class \code{FLQuant} or numeric
#'             value.
#' @param args Additional arguments.
#'
#' @author Adapted from WKNSMSE (2019) code by S. H. Fischer.
#'
#' @return A control object containing the adjusted F target for the stock.
#'
#' @export

hcrFixedC <- function(stk, Ctrg, args) {

  ay <- args$ay

  ## Convert to FLQuant if needed
  if(!is(Ctrg, "FLQuant"))
    Ctrg <- FLQuant(Ctrg, dimnames = list(iter = dimnames(stk@catch)$iter))

  ## Generate control object
  ctrl <- mse::getCtrl(values   = c(Ctrg),
                       quantity = "catch",
                       years    = ay + args$management_lag,
                       it       = dim(Ctrg)[6])

  return(ctrl)
}
