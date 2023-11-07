# ----------------------
# Risk
# ----------------------

plot_risk_MixME <- function(res, 
                            iy = NULL) {
  
  ## build plot
  plot_out <- ggplot2::ggplot(data = res,
                              aes(x = year)) +
    facet_wrap(~stk, scales = "free_y") +
    scale_y_continuous("Risk (P(SSB < Blim))", limits = c(0,NA)) +
    theme_bw()
  
  ## (Optional) Add start line
  if(!is.null(iy)){
    plot_out <- plot_out +
      geom_vline(aes(xintercept = iy), linetype = 2)
  }
  
  plot_out <- plot_out +
    geom_line(aes(y = risk)) +
    geom_hline(aes(yintercept = 0.05), linetype = 2)
  
  return(plot_out)
  
}