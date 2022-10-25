# ---
# title: 'Main function to run MIME'
# author: 'Matthew Pace'
# date: 'August 2022'
# ---

runMixME <- function(om, mp, ...) {

  ## Run check on inputs

  ## Convert FLStocks into FLBiols
  om$stks <- FLBiols(lapply(om$stks@names,
                            function(x) {
                              biol <- as(om$stks[[x]],"FLBiol")

                              biol@rec@params <- sr_list[[x]]@params
                              biol@rec@model  <- sr_list[[x]]@model
                              biol@rec$rec    <- NULL
                              return(biol)
                              }))

  ## Generate FLombf

  ## Run mp

}
