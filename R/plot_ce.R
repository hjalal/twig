#' Plot the cost-effectiveness plane
#'
#' Draws the cost-effectiveness (CE) plane from the simulated expected values
#' returned by [run_twig()]. Each probabilistic-sensitivity-analysis (PSA) draw
#' is shown as a point (the uncertainty cloud), the mean of each strategy is
#' highlighted, and the efficiency frontier connecting the non-dominated
#' strategies can be overlaid.
#'
#' @param sim_ev A 3D array of simulated expected values with dimensions
#'   decision, payoff (cost and effect), and simulation, as returned in
#'   `run_twig()$sim_ev`.
#' @param mean_ev An optional decision-by-payoff matrix of mean expected values
#'   (`run_twig()$mean_ev`). If `NULL`, the means are computed from `sim_ev`.
#' @param col_names A length-2 character vector naming the cost and effect
#'   payoffs. Default `c("cost", "utility")`.
#' @param frontier Logical; if `TRUE` (default) the efficiency frontier through
#'   the non-dominated strategies (from [calculate_icers()]) is drawn.
#' @param show_points Logical; if `TRUE` (default) the per-simulation PSA cloud
#'   is shown. Set to `FALSE` to plot only the strategy means.
#' @return A `ggplot` object.
#' @seealso [plot_ceac()], [calculate_icers()]
#' @export
#' @examples
#' set.seed(1)
#' sim_ev <- array(
#'   c(rnorm(50, 1000, 50), rnorm(50, 2000, 80),   # costs (A, B)
#'     rnorm(50, 0.8, 0.02), rnorm(50, 0.9, 0.02)), # effects (A, B)
#'   dim = c(2, 2, 50),
#'   dimnames = list(c("A", "B"), c("cost", "utility"), NULL))
#' plot_ce(sim_ev)
plot_ce <- function(sim_ev, mean_ev = NULL, col_names = c("cost", "utility"),
                    frontier = TRUE, show_points = TRUE) {

  if (!is.array(sim_ev) || length(dim(sim_ev)) != 3) {
    stop("sim_ev should be a 3D array with dimensions decision, payoff, and simulation.")
  }
  payoff_names <- dimnames(sim_ev)[[2]]
  if (!all(col_names %in% payoff_names)) {
    stop("col_names (", paste(col_names, collapse = ", "),
         ") must be among the payoff names: ", paste(payoff_names, collapse = ", "), ".")
  }

  decisions <- dimnames(sim_ev)[[1]]
  if (is.null(decisions)) decisions <- paste0("decision_", seq_len(dim(sim_ev)[1]))
  n_sim <- dim(sim_ev)[3]

  cost_mat <- sim_ev[, col_names[1], , drop = FALSE]
  eff_mat  <- sim_ev[, col_names[2], , drop = FALSE]
  psa <- data.frame(
    decision = rep(decisions, times = n_sim),
    cost     = as.vector(cost_mat),
    effect   = as.vector(eff_mat),
    stringsAsFactors = FALSE
  )

  if (is.null(mean_ev)) {
    mean_ev <- apply(sim_ev, c(1, 2), mean)
  }
  means <- data.frame(
    decision = decisions,
    cost     = mean_ev[, col_names[1]],
    effect   = mean_ev[, col_names[2]],
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot()
  if (show_points) {
    p <- p + ggplot2::geom_point(
      data = psa,
      ggplot2::aes(x = .data$effect, y = .data$cost, color = .data$decision),
      alpha = 0.25, size = 1)
  }

  if (frontier) {
    icers <- calculate_icers(mean_ev, col_names = col_names)
    nd <- icers[icers$status == "ND", ]
    nd <- nd[order(nd[[col_names[2]]]), ]
    frontier_df <- data.frame(effect = nd[[col_names[2]]], cost = nd[[col_names[1]]])
    p <- p + ggplot2::geom_line(
      data = frontier_df,
      ggplot2::aes(x = .data$effect, y = .data$cost),
      linetype = "dashed", color = "grey40")
  }

  p +
    ggplot2::geom_point(
      data = means,
      ggplot2::aes(x = .data$effect, y = .data$cost, fill = .data$decision),
      shape = 21, color = "black", size = 4) +
    ggplot2::labs(title = "Cost-Effectiveness Plane",
                  x = "Effect", y = "Cost",
                  color = "Strategy", fill = "Strategy") +
    ggplot2::theme_bw()
}
