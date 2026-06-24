#' Plot the Markov cohort trace
#'
#' Plots the proportion of the cohort occupying each Markov state over time, one
#' panel per decision. The trace is produced by running a Markov twig with
#' `verbose = TRUE` (see [run_twig()]), which returns the `markov_trace` array.
#'
#' @param results Either a verbose result from [run_twig()] (a list containing a
#'   `markov_trace` element) or a `markov_trace` array directly. The array has
#'   dimensions cycle, state, and decision.
#' @param decisions An optional character vector selecting which decisions to
#'   show. Defaults to all decisions.
#' @param states An optional character vector selecting which states to show.
#'   Defaults to all states.
#' @return A `ggplot` object.
#' @seealso [run_twig()]
#' @export
#' @examples
#' # A small cohort trace array (cycle x state x decision)
#' trace <- array(
#'   c(1, 0.9, 0.8, 0, 0.1, 0.2,     # decision A: Alive, then Dead
#'     1, 0.8, 0.6, 0, 0.2, 0.4),    # decision B
#'   dim = c(3, 2, 2),
#'   dimnames = list(cycle = c(0, 1, 2),
#'                   state = c("Alive", "Dead"),
#'                   decision = c("A", "B")))
#' plot_trace(trace)
plot_trace <- function(results, decisions = NULL, states = NULL) {

  trace <- if (is.list(results) && !is.null(results$markov_trace)) {
    results$markov_trace
  } else {
    results
  }

  if (!is.array(trace) || length(dim(trace)) != 3) {
    stop("Could not find a 3D 'markov_trace' array. Run a Markov model with ",
         "run_twig(..., verbose = TRUE) and pass the result (or its ",
         "$markov_trace element) to plot_trace().")
  }

  trace_long <- reshape2::melt(trace, value.name = "proportion")
  names(trace_long)[1:3] <- c("cycle", "state", "decision")
  trace_long$cycle <- as.numeric(as.character(trace_long$cycle))

  if (!is.null(decisions)) {
    trace_long <- trace_long[trace_long$decision %in% decisions, , drop = FALSE]
  }
  if (!is.null(states)) {
    trace_long <- trace_long[trace_long$state %in% states, , drop = FALSE]
  }

  ggplot2::ggplot(
    data = trace_long,
    ggplot2::aes(x = .data$cycle, y = .data$proportion, color = .data$state)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ .data$decision) +
    ggplot2::labs(title = "Markov Cohort Trace",
                  x = "Cycle", y = "Proportion of cohort", color = "State") +
    ggplot2::theme_bw()
}
