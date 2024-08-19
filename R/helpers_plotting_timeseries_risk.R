# ----------------------
# Risk
# ----------------------

plot_risk_MixME <- function(res, 
                            iy = NULL,
                            add = NULL,
                            colour = "black") {
  
  ## build plot
  if (is.null(add)) {
    plot_out <- ggplot2::ggplot() +
      facet_wrap(~stk, scales = "free_y") +
      scale_y_continuous("Risk (P(SSB < Blim))", limits = c(0,NA)) +
      theme_bw()
  } else {
    plot_out <- add
  }
  
  ## (Optional) Add start line
  if(!is.null(iy)){
    plot_out <- plot_out +
      geom_vline(xintercept = iy, linetype = 2)
  }
  
  plot_out <- plot_out +
    geom_line(aes(x = year, y = risk), colour = colour, data = res) +
    geom_hline(yintercept = 0.05, linetype = 2)
  
  return(plot_out)
  
}