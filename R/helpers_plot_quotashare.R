# ---
# title: 'Helper function to plot effort by area, fishery and year'
# author: 'Paul Dolder'
# date: 'July 2025'
# ---
#
#' @title quota share conditioning plot
#'
#' @description \code{plot_quotashare} Function takes an FLFisheries object and returns a plot of the quota share by fishery and stock. 
#' This can be useful to check conditioning before running the model.
#'
#' @param FLFisheries is an `object` of class FLFisheries
#' @param fisheries is a `character` vector of either 'all' or the name of the fisheries to include in the figure.
#' @param stocks is a `chacter` vector of either 'all' or the name of the stocks to include in the figure
#' @param `years` is a `numeric` or `character` vector with the years to include (can be 'all).
#'
#' @return a plot with either effort by year for each area or effort shares.
#'
#' @examples
#' \donttest{
#' data("mixedfishery_MixME_input")
#' fleets <- mixedfishery_MixME_input$om$flts
#' plot_quotashare(FLFisheries = fleets, fisheries = "all", stocks = "all", years = "all")
#' }
#'
#' @export

plot_quotashare <- function(FLFisheries, fisheries = "all", stocks = "all", years = "all") {
  require(dplyr); require(ggplot2)
  
  qs   <- do.call(rbind, lapply(FLFisheries, function(x) do.call(rbind, lapply(x, function(q) cbind(fishery = x@name, stock = q@name, as.data.frame(attr(q, "quotashare")))))))
  
  qs   <- qs |> group_by(fishery, stock, year) |> summarise(q50 = quantile(data, 0.5, na.rm = TRUE),
                                                            q05 = quantile(data, 0.05, na.rm = TRUE),
                                                            q95 = quantile(data, 0.95, na.rm = TRUE))
  
    ## Filters
  if(all(fisheries != "all") | length(fisheries)>1) {
    qs   <- dplyr::filter(qs, fishery %in% fisheries)
      }
  
  if(all(stocks != "all") | length(stocks)>1) {
    qs  <- dplyr::filter(qs, stock %in% stocks)
      }
  
  
  if(all(years != "all") | length(years)>1) {
  qs   <- dplyr::filter(qs, year %in% as.numeric(years))  
  }
  
  print(ggplot(qs, aes(x = year, y = q50)) + geom_line() + 
          geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.2) + theme_bw() + 
          facet_grid(stock ~ fishery) + ylab("Quota Share (fraction)") + 
          theme(axis.text.x = element_text(angle = -90)))
  
  return(qs)
  
}
