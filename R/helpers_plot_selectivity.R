# ---
# title: 'Helper function to plot selectivity-at-age'
# author: 'Paul Dolder' -- deal with iterations in all functions !!
# date: 'July 2025'
# ---
#
#' @title Selectivity conditioning plot
#'
#' @description \code{plot_selectivity} Function takes an FLFisheries object and returns a plot of the seletcity-at-age by stock and year,
#'  or by fishery, area and stock and year. This can be useful to check conditioning before running the model.
#'
#' @param FLFisheries is an `object` of class FLFisheries
#' @param fisheries is a `character` vector of either 'all' or the name of the fisheries to include in the figure.
#' @param stocks is a `character` vector of either 'all' or the name of the stocks to include in the figure.
#' @param years is a `numeric` or `character` vector with the years to include (can be 'all')
#' @param type is a `character` vector of either `individual` (by fleet and area) or `combined` (aggregated by stock).
#'
#' @return a plot with selectivity-at-age by year for each stock, or by individual fisheries, areas and years for each stock. 
#' A smooth GAM is fit to indicate the selectivity as a function of age by stock and year or stock, fishery, area and year. 
#'
#' @examples
#' \donttest{
#' data("mixedfishery_MixME_input")
#' fleets <- mixedfishery_MixME_input$om$flts
#' plot_selectivity(FLFisheries = fleets, fisheries = 'all', stocks = 'all', type = "combined")
#' }
#'
#' @export

plot_selectivity <- function(FLFisheries, fisheries = "all", years = "all", stocks = "all", type = "combined") {
  require(dplyr); require(ggplot2)
  
  ## Get the catch numbers to compute the stock level selection
  Ln   <- slot_fisheries(FLFisheries, "landings.n"); colnames(Ln)[9] <- "landings.n"
  Dn   <- slot_fisheries(FLFisheries, "discards.n"); colnames(Dn)[9] <- "discards.n"
  
  ## Combine
  Cn <- left_join(Ln, Dn, by = join_by("fishery", "stock", "age", "year", "unit", "season", "area", "iter"))
  

  ## Filters
  if(all(fisheries != "all") | length(fisheries)>1) {
    Cn <- dplyr::filter(Cn, fishery %in% fisheries)
  }
  
  if(all(stocks != "all") | length(stocks)>1) {
    Cn <- dplyr::filter(Cn, stock %in% stocks)  
  }
  
  if(all(years != "all") | length(years)>1) {
    Cn <- dplyr::filter(Cn, year %in% as.numeric(years))  
  }
  
  ## Compute the stock level selectivity for the set of fisheries and stocks
  Cn_St <- Cn |> group_by(stock, age, year, iter) |> 
    summarise(landings.n = sum(landings.n, na.rm =TRUE), discards.n = sum(discards.n, na.rm =TRUE)) |> 
    mutate(sel = landings.n/(landings.n+discards.n)) |> 
    group_by(stock, year, age) |> summarise(sel_q50 = quantile(sel, 0.5, na.rm = TRUE),
                                            sel_q05 = quantile(sel, 0.05, na.rm = TRUE),
                                            sel_q95 = quantile(sel, 0.95, na.rm = TRUE))
  
  ## Compute the quantiles for the fleet and area combinations
  Cn <- Cn |> group_by(fishery, area, stock, age, year, iter) |> 
    mutate(sel = landings.n/(landings.n+discards.n)) |> 
    group_by(fishery, area, stock, year, age) |> summarise(sel_q50 = quantile(sel, 0.5, na.rm = TRUE),
                                            sel_q05 = quantile(sel, 0.05, na.rm = TRUE),
                                            sel_q95 = quantile(sel, 0.95, na.rm = TRUE))

  ### Now choose which to plot
  if(type == "combined") {
  print(ggplot(data = Cn_St, aes(x = age, y = sel_q50, group = year)) + 
    geom_line(aes(colour = year)) + facet_grid(~stock, scale = "free") + 
      geom_ribbon(aes(ymin = sel_q05, ymax = sel_q95), alpha = 0.2) + theme_bw() + ylab("landings selectivity") + 
      theme(axis.text.x = element_text(angle = -90)))
  }
  
  if(type == "individual") {
    if(fisheries == "all") {  ## If plotting all fisheries, remove the legend
    print(ggplot(data = Cn, aes(x = age, y = sel_q50, group = year)) + 
            geom_line(aes(colour = year)) + facet_wrap(fishery~area+stock, scale = "free") + 
            geom_ribbon(aes(ymin = sel_q05, ymax = sel_q95), alpha = 0.2) + theme_bw() + ylab("landings selectivity")+ 
            theme(legend.position = "none", axis.text.x = element_text(angle = -90)))
    } else {
      print(ggplot(data = Cn, aes(x = age, y = sel_q50, group = year)) + 
              geom_line(aes(colour = year)) + facet_wrap(fishery~area+stock, scale = "free") + 
              geom_ribbon(aes(ymin = sel_q05, ymax = sel_q95), alpha = 0.2) + theme_bw() + ylab("landings selectivity")+ 
              theme(axis.text.x = element_text(angle = -90)))
    }
  }
   
  if(type=="combined"){ return(Cn_St) }
  if(type=="individual") {return(Cn)}
  
}