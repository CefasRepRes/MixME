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

phcrMIME <- function(args) {

  ## Checks to ensure that stocks and parameters are named
  if(is.null(names(args)))
    stop("'args' must contain contain named list of parameters nested by stock")

  ## Coerce parameters into FLPar
  hcrpars <- lapply(1:length(args), function(x) {
    do.call(FLPar, args[[x]])})
  names(hcrpars) <- names(args)

  ## return as list
  return(list(hcrpars = hcrpars))
}

#' Advice rule implementation for stocks
#'
#' Applies a built-in or user-supplied advice rule for each stock to
#' generate single-stock advice. The advice rule may take the form of a
#' Harvest Control Rule that returns advice based on estimated stock status,
#' fixed catch advice or fixed fishing mortality advice.
#'
#' @param stk list of stocks
#' @param hcrpars Harvest Control Rule parameters for each stock. A named
#'                nested list. Each named list element corresponds to a stock
#'                and should contain a list of parameters for that stock.
#' @param hcrmethod named list of advice rule methods for each stock.
#'                  List elements may be the name of an in-built advice rule
#'                  method or a user-supplied function that is applied to the
#'                  stock. Defaults to 'hcrICES'.
#'
#' @export

hcrMIME <- function(stk, args, hcrpars) {

  ## TO DO - How to apply a multistock harvest control rule? Cannot loop over
  ##         each stock in that case... How to handle a mix of single stock and
  ##         multistock harvest control rules?

  ## Extract the methods to be used for each stock
  hcrmethod <- args$hcrmethod

  lapply(1:length(stk), function(x){

    ## Run user-supplied method (if provided)
    if(is.function(hcrmethod[[x]])){

      do.call(hcrmethod[[x]],
              list(stk     = stk[[x]],
                   args    = args,
                   hcrpars = hcrpars[[x]]))

      ## Run ICES Harvest control Rule
    } else if (hcrmethod == "hcrICES"){

      ctrl <- hcrICES(stk     = stk[[x]],
                      args    = args,
                      hcrpars = hcrpars[[x]])

      ## Run Fixed F advice
    } else if (hcrmethod == "hcrFixedF"){

      ctrl <- mse::fixedF.hcr(stk      = stk[[x]],
                              ftrg     = ftrg[[x]],
                              args     = args,
                              tracking = NULL)

      ## Run Fixed Catch advice
    } else if (hcrmethod == "hcrFixedC"){

      ctrl <- hcrFixedC(stk  = stk[[x]],
                        Ctrg = Ctrg[[x]],
                        args = args)

    } else {
      stop("Only user-supplied functions, 'hcrICES', 'hcrFixedC' and 'hcrFixedF' are currently supported")
    }

    ## return control object
    return(ctrl)
  })

  ## return control
  return(list(ctrl = ctrl))
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
#'
#' @author Adapted from WKNSMSE (2019) code by S. H. Fischer.
#'
#' @return A control object containing the adjusted F target for the stock.
#'
#' @export

hcrICES <- function(stk, args, hcrpars) {

  ## Extract current (assessment) year
  ay <- args$ay

  ## Extract stock dimensions
  ni <- dim(stk)[6]

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
  pos_Btrigger <- which(status_Btrigger < 1)
  pos_Blim     <- which(status_Blim < 1)

  # -----------------------------------------------------------------#
  # if SSB >= Btrigger;                F = Ftarget
  #    SSB < Btrigger & SSB >= Blim ;  F = Ftarget * (SSB / Btrigger)
  #    SSB < Blim;                     F = 0
  # -----------------------------------------------------------------#

  ## multiplier should not exceed 1
  Fmult <- ifelse(status_Btrigger < 1, status_Btrigger, 1)

  ## Adjust F multiplier if SSB is below Blim
  Fmult[,,,,, pos_Blim] <- 0

  ## Generate new Ftarget
  Ftrgt <- Ftrgt * Fmult

  ## create ctrl object
  ctrl <- mse::getCtrl(values = Ftrgt, quantity = "f", years = ay + 1, it = ni) # perhaps should be ay + args$management_lag?

  return(ctrl)
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
