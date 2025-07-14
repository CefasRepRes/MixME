# ---
# title: 'Helper function to plot discard rates by fishery and year'
# author: 'Paul Dolder'
# date: 'July 2025'
# ---
#
#' @title Discard rate conditioning plot
#'
#' @description \code{plot_discard_rates} Function takes an FLFisheries object and returns a plot of the discard rate by stock or by stock and fishery. 
#' This can be useful to check conditioning before running the model.
#'
#' @param FLFisheries is an `object` of class FLFisheries
#' @param fisheries is a `character` vector of either 'all' or the name of the fisheries to include in the figure.
#' @param stocks is a `character` vector of either 'all' or the name of the stocks to include in the figure.
#' @param years is a `numeric` or `character` vetor of the years to include (can be 'all').
#' @param type is a `character` vector of either `combined` to aggregate across fisheries, or `individual` to plot by fishery.
#'
#' @return a plot with discard rate (fraction discarded) by year for each stock, which can be faceted by individual fisheries. 
#' areas within a fishery are denoted by different linetypes and shapeds. 
#'
#' @examples
#' \donttest{
#' data("mixedfishery_MixME_input")
#' fleets <- mixedfishery_MixME_input$om$flts
#' plot_discard_rates(FLFisheries = fleets, fisheries = "all", stock = "all", type = "individual")
#' }
#'
#' @export

plot_discard_rates <- function(FLFisheries, fisheries = "all", stocks = "all", years = "all", type = "combined") {
  require(dplyr); require(ggplot2)
  
  ## Extract the mean weight slot
  LaWt <- slot_fisheries(FLFisheries, "landings.wt"); colnames(LaWt)[9] <- "landings.wt"
  DiWt <- slot_fisheries(FLFisheries, "discards.wt"); colnames(DiWt)[9] <- "discards.wt"
  
  ## Get the catch numbers to compute the catch weights
  Ln   <- slot_fisheries(FLFisheries, "landings.n"); colnames(Ln)[9] <- "landings.n"
  Dn   <- slot_fisheries(FLFisheries, "discards.n"); colnames(Dn)[9] <- "discards.n"
  
  CWt <- left_join(LaWt, DiWt, by = join_by("fishery", "stock", "age", "year", "unit", "season", "area", "iter"))
  
  ## Add the numbers
  CWt <- left_join(CWt, Ln, by = join_by("fishery", "stock", "age", "year", "unit", "season", "area", "iter"))
  CWt <- left_join(CWt, Dn, by = join_by("fishery", "stock", "age", "year", "unit", "season", "area", "iter"))
  
  # Compute the SOP
  CWt <- CWt |> mutate(landings = landings.n * landings.wt, discards = discards.wt * discards.n) |> 
    group_by(fishery, area, year, stock, iter) |> summarise(landings = sum(landings,na.rm = TRUE), discards = sum(discards,na.rm = TRUE))
  
 
  
  ## Filters
  if(all(fisheries != "all") | length(fisheries)>1) {
    CWt <- dplyr::filter(CWt, fishery %in% fisheries)
  }
  
  if(all(stocks != "all") | length(stocks)>1) {
    CWt <- dplyr::filter(CWt, stock %in% stocks)  
  }
  
  if(all(years != "all") | length(years)>1) {
    CWt <- dplyr::filter(CWt, year %in% as.numeric(years))  
  }
  
  ### Now choose which to plot
  
  if(type == "combined") {
    CWt <- CWt |> group_by(year, stock, iter) |> summarise(landings = sum(landings), discards = sum(discards)) |> 
      mutate(discard_rate = discards/(landings + discards)) |>
      group_by(year, stock) |> summarise(q50_dr = quantile(discard_rate, 0.5, na.rm = TRUE),
                                         q05_dr = quantile(discard_rate, 0.05, na.rm = TRUE),
                                         q95_dr = quantile(discard_rate, 0.95, na.rm = TRUE))
    
    
    print(ggplot(data = CWt, aes(x = year, y = q50_dr)) + geom_point() + geom_line() +
            geom_ribbon(aes(ymin = q05_dr, ymax = q95_dr), alpha = 0.2) + 
             facet_wrap(~stock, scale = "free") + theme_bw() + 
            ylab("discard rate (fraction)") + theme(legend.title = element_blank(), axis.text.x = element_text(angle = -90)))
  }
  
  if(type == "individual") {
    CWt <- CWt |>  group_by(year, stock, fishery, area, iter) |> summarise(landings = sum(landings), discards = sum(discards)) |> 
      mutate(discard_rate = discards/(landings + discards)) |>
      group_by(year, fishery, area, stock) |> summarise(q50_dr = quantile(discard_rate, 0.5, na.rm = TRUE),
                                         q05_dr = quantile(discard_rate, 0.05, na.rm = TRUE),
                                         q95_dr = quantile(discard_rate, 0.95, na.rm = TRUE))
    
    if(fisheries == "all") { ## drop the legend if plotting all as too many, but keep for individual fisheries (or subsets of)
    print(ggplot(data = CWt, aes(x = year, y = q50_dr, group = area)) + geom_point(aes(shape = area)) + geom_line(aes(linetype= area)) +
            geom_ribbon(aes(ymin = q05_dr, ymax = q95_dr), alpha = 0.2) + 
            facet_wrap(fishery~stock, scale = "free") + theme_bw() + 
            ylab("discard rate (fraction)") + theme(legend.position = "none", axis.text.x = element_text(angle = -90)))
    } else {
      print(ggplot(data = CWt, aes(x = year, y = q50_dr, group = area)) + geom_point(aes(shape = area)) + geom_line(aes(linetype= area)) +
              geom_ribbon(aes(ymin = q05_dr, ymax = q95_dr), alpha = 0.2) + 
              facet_wrap(fishery~stock, scale = "free") + theme_bw() + 
              ylab("discard rate (fraction)") + theme(legend.title = element_blank(), axis.text.x = element_text(angle = -90)))
    }
  }
  
  return(CWt)
  
}
