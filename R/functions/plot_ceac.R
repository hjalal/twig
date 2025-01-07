plot_ceac <- function(rewards_sim, wtp_range, col_names = c("cost", "utility")){
nmb_proportions_long <- compute_nmb_proportions(rewards_sim, wtp_range, col_names)

ceac <- ggplot2::ggplot(data = nmb_proportions_long) +
    ggplot2::geom_line(ggplot2::aes(x = wtp, y = prob_cost_effective, color = decision)) +
    ggplot2::labs(title = "Cost-Effectiveness Acceptibility Curve", x = "Willingness to Pay Threshold", y = "Probability cost-effective") +
    ggplot2::theme_bw() 
  return(ceac)
}

