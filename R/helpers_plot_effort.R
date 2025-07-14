# ---
# title: 'Helper function to plot effort by area, fishery and year'
# author: 'Paul Dolder'
# date: 'July 2025'
# ---
#
#' @title Effort conditioning plot
#'
#' @description \code{plot_effort} Function takes an FLFisheries object and returns a plot of the effort by area and fishery. 
#' This can be useful to check conditioning before running the model.
#'
#' @param FLFisheries is an `object` of class FLFisheries
#' @param fisheries is a `character` vector of either 'all' or the name of the fisheries to include in the figure.
#' @param `years` is a `numeric` or `character` vector with the years to include (can be 'all).
#' @param type is a `character` vector of either `effort` to aggregate across area, or `shares` to plot fleet-area effort shares by fishery.
#'
#' @return a plot with either effort by year for each area or effort shares.
#'
#' @examples
#' \donttest{
#' data("mixedfishery_MixME_input")
#' fleets <- mixedfishery_MixME_input$om$flts
#' plot_effort(FLFisheries = fleets, fisheries = "all", type = "effort")
#' }
#'
#' @export

plot_effort <- function(FLFisheries, fisheries = "all", years = "all", type = "effort") {
  require(dplyr); require(ggplot2)
  
  ef   <- do.call(rbind, lapply(FLFisheries, function(x) cbind(fishery = x@name, as.data.frame(x@effort))))
  
  efsh <- ef |> group_by(fishery, year, iter) |> mutate(efsh = data/sum(data))
  
  
    ## Filters
  if(all(fisheries != "all") | length(fisheries)>1) {
    ef   <- dplyr::filter(ef, fishery %in% fisheries)
    efsh <- dplyr::filter(efsh, fishery %in% fisheries)
  }
  
  if(all(years != "all") | length(years)>1) {
  ef   <- dplyr::filter(ef, year %in% as.numeric(years))  
  efsh <- dplyr::filter(efsh, year %in% as.numeric(years))
  }
  
  
  if(type == "effort") {
    print(ggplot(ef, aes(x = year, y = data)) + geom_area(aes(fill = area)) + 
      facet_wrap(~fishery, scale = "free_y") + theme_bw() + 
        theme(legend.position = "none", axis.text.x = element_text(angle = - 90)) + 
        ylab("effort (000 kw days)"))
  }
  
  if(type == "shares") {
    
    print(ggplot(efsh, aes(x = year, y = efsh)) + geom_line(aes(linetype = area)) + 
            facet_wrap(~fishery, scale = "free_y") + theme_bw() + ylab("effort shares (fraction)") + 
            theme(legend.position = "none", axis.text.x = element_text(angle = - 90))
            )
  }
  
  
  return(list(effort = ef, effort_share = efsh))
  
}
