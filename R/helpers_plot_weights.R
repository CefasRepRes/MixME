# ---
# title: 'Helper function to plot weight-at-age'
# author: 'Paul Dolder'
# date: 'July 2025'
# ---
#
#' @title Weight conditioning plot
#'
#' @description \code{plot_weights} Function takes an FLFisheries object and returns a plot of the weights-at-age by stock. 
#' This can include landings weight, discard weight and catch weight. This can be useful to check conditioning before running the model.
#'
#' @param FLFisheries is an `object` of class FLFisheries
#' @param fisheries is a `character` vector of either 'all' or the name of the fisheries to include in the figure.
#' @param stocks is a `character` vector of either 'all' or the name of the stocks to include in the figure.
#' @param years is a `numeric` or `character` vector of the years to include (can be 'all')
#' @param type is a `character` vector of either `landings.wt`, `discards.wt` or `catch.wt`.
#'
#' @return a plot with weight-at-age by year for each stock, with individual fisheries and areas indicated by the points. 
#' A smooth GAM is fit to indicate the median weight as a function of age by stock and year. 
#'
#' @examples
#' \donttest{
#' data("mixedfishery_MixME_input")
#' fleets <- mixedfishery_MixME_input$om$flts
#' plot_weights(FLFisheries = fleets, fisheries = 'all', stocks = 'all', type = "catch.wt")
#' }
#'
#' @export

plot_weights <- function(FLFisheries, fisheries = "all", stocks = "all", years = "all", type = "catch.wt") {
  require(dplyr); require(ggplot2)
  
  ## Extract the landings and discard weights
  LaWt <- slot_fisheries(FLFisheries, "landings.wt"); colnames(LaWt)[9] <- "landings.wt"
  DiWt <- slot_fisheries(FLFisheries, "discards.wt"); colnames(DiWt)[9] <- "discards.wt"
  
  ## Get the catch numbers to compute the catch weights
  Ln   <- slot_fisheries(FLFisheries, "landings.n"); colnames(Ln)[9] <- "landings.n"
  Dn   <- slot_fisheries(FLFisheries, "discards.n"); colnames(Dn)[9] <- "discards.n"
  
  CWt <- left_join(LaWt, DiWt, by = join_by("fishery", "stock", "age", "year", "unit", "season", "area", "iter"))
  
  ## Add the numbers
  CWt <- left_join(CWt, Ln, by = join_by("fishery", "stock", "age", "year", "unit", "season", "area", "iter"))
  CWt <- left_join(CWt, Dn, by = join_by("fishery", "stock", "age", "year", "unit", "season", "area", "iter"))
  
  ## Compute the catch weights
  CWt$catch.wt  <-  ((CWt$landings.wt*CWt$landings.n) + (CWt$discards.wt * CWt$discards.n))/(CWt$landings.n+CWt$discards.n)
  
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
  if(type == "catch.wt") {
    print(ggplot(data = CWt, aes(x = age, y = catch.wt, group = year)) + 
            geom_smooth(aes(colour = year), alpha = 0.2) + 
            geom_point(aes(colour = year), alpha = 0.2) +
            facet_wrap(~stock, scale = "free") + theme_bw())
  }
  
  if(type == "landings.wt") {
    print(ggplot(data = CWt, aes(x = age, y = landings.wt, group = year)) + 
            geom_smooth(aes(colour = year), alpha = 0.2) + 
            geom_point(aes(colour = year), alpha = 0.2) + 
            facet_wrap(~stock, scale = "free") + theme_bw())
  }
  
  if(type == "discards.wt") {
    print(ggplot(data = CWt, aes(x = age, y = discards.wt, group = year)) + 
            geom_smooth(aes(colour = year), alpha = 0.2) + 
            geom_point(aes(colour = year), alpha = 0.2) +
            facet_wrap(~stock, scale = "free") + theme_bw())
  }
  
  return(filter(CWt, !landings.wt == 0 & discards.wt==0))
  
}