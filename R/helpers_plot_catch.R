# ---
# title: 'Helper function to plot catch by fishery and year'
# author: 'Paul Dolder'
# date: 'July 2025'
# ---
#
#' @title Total Catch conditioning plot
#'
#' @description \code{plot_catches} Function takes an FLFisheries object and returns a plot of the total catches by stock or by stock and fishery. 
#' This can be useful to check conditioning before running the model.
#'
#' @param FLFisheries is an `object` of class FLFisheries
#' @param fisheries is a `character` vector of either 'all' or the name of the fisheries to include in the figure.
#' @param stocks is a `character` vector of either 'all' or the name of the stocks to include in the figure.
#' @param `years` is a `numeric` or `character` vector with the years to include (can be 'all).
#' @param type is a `character` vector of either `combined` to aggregate across fisheries, or `individual` to plot by fishery.
#' @param values is a `character` vector of either `numbers` to plot catch numbers-at-age or `weight`
#'
#' @return a plot with total catch (tonnage) by year for each stock, which can be faceted by individual fisheries. 
#' Landings and discards are denoted by different fill colours in the bars. 
#'
#' @examples
#' \donttest{
#' data("mixedfishery_MixME_input")
#' fleets <- mixedfishery_MixME_input$om$flts
#' plot_catches(FLFisheries = fleets, fisheries = "all", stock = "all", years = "all", type = "individual")
#' }
#'
#' @export

plot_catches <- function(FLFisheries, fisheries = "all", stocks = "all", years = "all", type = "combined", values = "weight") {
  require(dplyr); require(ggplot2)
  
  ## Extract the mean weight slots
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
  CWt <- CWt |> mutate(landings = landings.n * landings.wt, discards = discards.wt * discards.n) 
  
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
  
  if(values == "weight") {
    
    CWt |> group_by(fishery, year, stock, iter) |> 
    summarise(landings = sum(landings,na.rm = TRUE), discards = sum(discards,na.rm = TRUE))
  
  if(type == "combined") {
    CWt <- CWt |> group_by(year, stock, iter) |> summarise(landings = sum(landings), discards = sum(discards)) |>
      group_by(year, stock) |> summarise(land_q50 = quantile(landings, 0.5, na.rm = TRUE),
                                         land_q05 = quantile(landings, 0.05, na.rm = TRUE),
                                         land_q95 = quantile(landings, 0.95, na.rm = TRUE),
                                         disc_q50 = quantile(discards, 0.5, na.rm = TRUE),
                                         disc_q05 = quantile(discards, 0.05, na.rm = TRUE),
                                         disc_q95 = quantile(discards, 0.95, na.rm = TRUE)) |>
      tidyr::pivot_longer(cols = c("land_q50","land_q05", "land_q95",
                                   "disc_q50", "disc_q05", "disc_q95"))
    
    ## Get the upper and lower quantiles of the catches for the error bars
    CWt_err      <- CWt 
    CWt_err$type <- ifelse(grepl("q05", CWt_err$name), "low", ifelse(grepl("q50", CWt_err$name), "value", "high")) 
    CWt_err <- CWt_err |> group_by(year, stock, type) |> summarise(value = sum(value)) |> tidyr::pivot_wider(names_from = type, values_from = value)
    
    print(ggplot(data = CWt[CWt$name %in% c("land_q50", "disc_q50"),], aes(x = year, y = value)) +
            geom_col(aes(fill = name), alpha = 0.7) + 
            geom_errorbar(data = CWt_err, aes(ymin = low, ymax = high)) + 
             facet_wrap(~stock, scale = "free") + theme_bw() + 
            scale_fill_manual(values = c("lightblue", "darkblue"),labels = c("discards", "landings"))  + 
            ylab("tonnes (000)") + theme(legend.title = element_blank()))
  }
  
  if(type == "individual") {
    CWt <- CWt |> group_by(year, stock, fishery, iter) |> summarise(landings = sum(landings, na.rm =TRUE), discards = sum(discards, na.rm =TRUE)) |>
      group_by(year, fishery, stock) |> summarise(land_q50 = quantile(landings, 0.5, na.rm = TRUE),
                                         land_q05 = quantile(landings, 0.05, na.rm = TRUE),
                                         land_q95 = quantile(landings, 0.95, na.rm = TRUE),
                                         disc_q50 = quantile(discards, 0.5, na.rm = TRUE),
                                         disc_q05 = quantile(discards, 0.05, na.rm = TRUE),
                                         disc_q95 = quantile(discards, 0.95, na.rm = TRUE)) |>
      tidyr::pivot_longer(cols = c("land_q50","land_q05", "land_q95",
                                   "disc_q50", "disc_q05", "disc_q95"))
    
    ## Get the upper and lower quantiles of the catches for the error bars
    CWt_err      <- CWt 
    CWt_err$type <- ifelse(grepl("q05", CWt_err$name), "low", ifelse(grepl("q50", CWt_err$name), "value", "high")) 
    CWt_err <- CWt_err |> group_by(year, stock, fishery, type) |> summarise(value = sum(value)) |> tidyr::pivot_wider(names_from = type, values_from = value)
    
    ## Order the facets by most landings to least landings
    fac_ord <- CWt %>% group_by(fishery) %>%
      summarise(value = sum(value))
    flt_ord <- c(fac_ord[order(-fac_ord$value),"fishery"] %>% as.data.frame())$fishery
    
    print(ggplot(data = CWt[CWt$name %in% c("land_q50", "disc_q50"),], aes(x = year, y = value)) + geom_col(aes(fill = name), alpha = 0.7)  + 
            geom_errorbar(data = CWt_err, aes(ymin = low, ymax = high)) +  
            facet_wrap(factor(fishery, levels = c(flt_ord))~stock, scale = "free") + theme_bw() + 
            scale_fill_manual(values = c("lightblue", "darkblue"), labels = c("discards", "landings"))  + 
            ylab("tonnes (000)") + theme(legend.title = element_blank()))
  }
  
  }
  
  if(values=="numbers") {
    
    if(type == "combined") {
      CWt <- CWt |> group_by(year, stock, age, iter) |> summarise(landings = sum(landings.n, na.rm =TRUE), discards = sum(discards.n, na.rm =TRUE)) |>
        group_by(year, age, stock) |> summarise(land_q50 = quantile(landings, 0.5, na.rm = TRUE),
                                           land_q05 = quantile(landings, 0.05, na.rm = TRUE),
                                           land_q95 = quantile(landings, 0.95, na.rm = TRUE),
                                           disc_q50 = quantile(discards, 0.5, na.rm = TRUE),
                                           disc_q05 = quantile(discards, 0.05, na.rm = TRUE),
                                           disc_q95 = quantile(discards, 0.95, na.rm = TRUE)) |>
        tidyr::pivot_longer(cols = c("land_q50","land_q05", "land_q95",
                                     "disc_q50", "disc_q05", "disc_q95"))
      
      ## Get the upper and lower quantiles of the catches for the error bars
      CWt_err      <- CWt 
      CWt_err$type <- ifelse(grepl("q05", CWt_err$name), "low", ifelse(grepl("q50", CWt_err$name), "value", "high")) 
      CWt_err <- CWt_err |> group_by(year, stock, age, type) |> summarise(value = sum(value)) |> 
        tidyr::pivot_wider(names_from = type, values_from = value)
      
      ## We want to have the years along the y-axis but not as facets...
      ## We can convert these to densities
      
      print(ggplot(data = CWt[CWt$name %in% c("land_q50", "disc_q50"),], aes(x = age, y = value/1000)) +
              geom_col(aes(fill = name), alpha = 0.7) + 
              geom_errorbar(data = CWt_err, aes(ymin = low/1000, ymax = high/1000)) +
              facet_grid(year~stock, scale = "free") + theme_bw() + 
              scale_fill_manual(values = c("lightblue", "darkblue"),labels = c("discards", "landings"))  + 
              ylab("numbers (m)") + theme(legend.title = element_blank()))
      
   
    }
    
    if(type == "individual") {
      CWt <- CWt |> group_by(year, stock, fishery, age, iter) |> summarise(landings = sum(landings), discards = sum(discards)) |>
        group_by(year, fishery, stock, age) |> summarise(land_q50 = quantile(landings, 0.5, na.rm = TRUE),
                                                    land_q05 = quantile(landings, 0.05, na.rm = TRUE),
                                                    land_q95 = quantile(landings, 0.95, na.rm = TRUE),
                                                    disc_q50 = quantile(discards, 0.5, na.rm = TRUE),
                                                    disc_q05 = quantile(discards, 0.05, na.rm = TRUE),
                                                    disc_q95 = quantile(discards, 0.95, na.rm = TRUE)) |>
        tidyr::pivot_longer(cols = c("land_q50","land_q05", "land_q95",
                                     "disc_q50", "disc_q05", "disc_q95"))
      
      ## Get the upper and lower quantiles of the catches for the error bars
      CWt_err      <- CWt 
      CWt_err$type <- ifelse(grepl("q05", CWt_err$name), "low", ifelse(grepl("q50", CWt_err$name), "value", "high")) 
      CWt_err <- CWt_err |> group_by(year, stock, fishery, age, type) |> summarise(value = sum(value, na.rm = TRUE)) |> 
        tidyr::pivot_wider(names_from = type, values_from = value)
      
      ## Order the facets by most landings to least landings
      fac_ord <- CWt %>% group_by(fishery) %>%
        summarise(value = sum(value))
      flt_ord <- c(fac_ord[order(-fac_ord$value),"fishery"] %>% as.data.frame())$fishery

      
      print(ggplot(data = CWt[CWt$name %in% c("land_q50", "disc_q50"),], aes(x = age, y = value/1000)) +
              geom_col(aes(fill = name), alpha = 0.7) + 
              geom_errorbar(data = CWt_err, aes(ymin = low/1000, ymax = high/1000)) +
              facet_wrap(factor(fishery, levels = c(flt_ord))~stock+year, scale = "free") + theme_bw() + 
              scale_fill_manual(values = c("lightblue", "darkblue"),labels = c("discards", "landings"))  + 
              ylab("numbers (m)") + theme(legend.title = element_blank()))
    }
    
    
  }
  
  return(CWt)
  
}
