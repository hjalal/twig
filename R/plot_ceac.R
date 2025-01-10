#' Plot Cost-Effectiveness Acceptability Curve (CEAC)
#'
#' This function plots the Cost-Effectiveness Acceptability Curve (CEAC) for a set of strategies based on their costs and utilities.
#'
#' @param payoffs_sim A 3D array containing the simulated payoffs. The dimensions should be decision, payoff (cost and utility), and simulation.
#' @param wtp_range A numeric vector specifying the range of willingness to pay (WTP) thresholds.
#' @param col_names A character vector specifying the names of the columns for cost and utility. Default is c("cost", "utility").
#' @return A ggplot object representing the CEAC.
#' @export
#' @examples
#' # Example payoffs simulation array
#' payoffs_sim <- array(
#'   data = c(1000, 2000, 1500, 0.8, 0.85, 0.82, 1000, 2000, 1500, 0.8, 0.85, 0.82),
#'   dim = c(3, 2, 2),
#'   dimnames = list(c("StrategyA", "StrategyB", "StrategyC"), c("cost", "utility"), NULL)
#' )
#' 
#' # Define WTP range
#' wtp_range <- seq(0, 100000, by = 1000)
#' 
#' # Plot CEAC
#' ceac_plot <- plot_ceac(payoffs_sim, wtp_range)
#' print(ceac_plot)
plot_ceac <- function(payoffs_sim, wtp_range, col_names = c("cost", "utility")){
  # Check if payoffs_sim is an array
  if(!is.array(payoffs_sim)){
    stop("payoffs_sim should be an array. If you have used verbose = TRUE in run_twig, make sure to rerun run_twig with verbose = FALSE before calling plot_ceac.")
  }
  # if number of dimensions is not 3, return an error
  if(length(dim(payoffs_sim)) != 3){
    stop("payoffs_sim should be a 3D array with dimensions decision, payoff (cost and utility), and simulation (sim).")
  }
  
  nmb_proportions_long <- compute_nmb_proportions(payoffs_sim, wtp_range, col_names)

  ceac <- ggplot2::ggplot(data = nmb_proportions_long) +
    ggplot2::geom_line(ggplot2::aes(x = .data$wtp, y = .data$prob_cost_effective, color = .data$decision)) +
    ggplot2::labs(title = "Cost-Effectiveness Acceptability Curve", x = "Willingness to Pay Threshold", y = "Probability cost-effective") +
    ggplot2::theme_bw() 
  return(ceac)
}

