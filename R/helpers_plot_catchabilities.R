# ---
# title: 'Helper function to plot catchabilities and identifying outliers'
# author: 'Paul Dolder'
# date: 'July 2025'
# ---
#
#' @title Catchability conditioning plot
#'
#' @description \code{plot_catchability} Function takes an FLFisheries object and returns a plot of the catchabilities by stock, 
#' including identifying any outliers and extreme values. This can be useful to check conditioning before running the model.
#'
#' @param FLFisheries is an `object` of class FLFisheries
#' @param fisheries is a `character` vector of either 'all' or the name of the fisheries to include in the figure.
#' @param stocks is a `character` vector of either 'all' or the name of the stocks to include in the figure.
#' @param years is a `numeric` or `character` vector with the years to include (can be 'all')
#' @param outlier_minmax is a `numeric` vector of length 2, of the minimum and maximum quantiles for catchability by stock that are used to identify outliers.
#'
#' @return a `plot` with catchability by year for each stock, and fishery (colours) and area (shapes) combinations. 
#' The outliers are identified as the red points, extreme values lie in the red shaded area. AND a `dataframe` with the 
#' identified outliers and extreme values. 
#'
#' @examples
#' \donttest{
#' data("mixedfishery_MixME_input")
#' fleets <- mixedfishery_MixME_input$om$flts
#' plot_catchability(FLFisheries = fleets, fleets = 'all', stocks = 'all', outlier_minmax = c(0.001, 0.999))
#' }
#'
#' @export

plot_catchability <- function(FLFisheries, fisheries = "all", stocks = "all", years = "all", outlier_minmax = c(0.001,0.999)) {
  require(dplyr); require(ggplot2)
  
  ## Extract the catch.q slot
  q <- slot_fisheries(FLFisheries, "catch.q")
  
  ## Filters
  if(all(fisheries != "all") | length(fisheries)>1) {
  q <- dplyr::filter(q, fishery %in% fisheries)
  }
  
  if(all(stocks != "all") | length(stocks)>1) {
  q <- dplyr::filter(q, stock %in% stocks)  
  }
  
  
  if(all(years != "all") | length(years)>1) {
    q <- dplyr::filter(q, year %in% as.numeric(years))  
  }

  q <-dplyr::filter(q, params == "alpha")
  
  if(is.null(q$area)) {
    q$area <- 'all'
  }
   
    q <- q |> mutate(logq = log(data))
    
    # Identify any outliers per stock
    q_out <- q |> group_by(stock) |> filter(is.finite(logq)) |> 
      summarise(lower = quantile(logq, outlier_minmax[1]), upper = quantile(logq,outlier_minmax[2])) 
    
    q <- left_join(q, q_out, by = c("stock"))
    q$outlier <- ifelse((q$logq > q$upper | q$logq < q$lower) & is.finite(q$logq), TRUE,FALSE)
    
    ## If there are catchabilities > 0.1 these are suspicious.
    ## So we draw a rectangle round those
   
    print(ggplot(q, aes(x = year, y = logq)) + geom_point(aes(colour = fishery, shape = area, alpha = 0.2)) + 
      geom_point(data = filter(q, outlier), shape = 1, colour = "red") +
      annotate("rect", xmin = min(as.numeric(q$year)-1), xmax = max(as.numeric(q$year)+1),
               ymin = log(0.1), ymax = Inf, alpha = 0.2, fill = "red") + 
      facet_wrap(~ stock, scale = "free_y") + 
      theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = -90)))
  
    print(rbind(filter(q, outlier), filter(q, logq > log(0.1))) ) 
  
}