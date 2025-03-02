#' Calculate Incremental Cost-Effectiveness Ratios (ICERs)
#'
#' This function calculates the Incremental Cost-Effectiveness Ratios (ICERs) for a set of strategies based on their costs and effects.
#'
#' @param payoffs_summary A matrix or data frame containing the summary statistics of the payoffs. It must have columns for cost and utility.
#' @param col_names A character vector specifying the names of the columns for cost and utility. Default is c("cost", "utility").
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{decision}: The name of the strategy.
#'   \item \code{cost}: The cost of the strategy.
#'   \item \code{utility}: The effect (utility) of the strategy.
#'   \item \code{inc_cost}: The incremental cost compared to the next less effective strategy.
#'   \item \code{inc_utility}: The incremental effect compared to the next less effective strategy.
#'   \item \code{ICER}: The Incremental Cost-Effectiveness Ratio.
#'   \item \code{status}: The dominance status of the strategy (ND = non-dominated, D = dominated, ED = extendedly dominated).
#' }
#' @export
#' @examples
#' payoffs_summary <- matrix(c(100, 200, 0.5, 0.7), ncol = 2, 
#'                           dimnames = list(c("Strategy A", "Strategy B"), 
#'                                           c("cost", "utility")))
#' calculate_icers(payoffs_summary)

calculate_icers <- function (payoffs_summary, col_names = c("cost", "utility")) {
cost <- payoffs_summary[, col_names[1]]
effect <- payoffs_summary[, col_names[2]]
strategies <- rownames(payoffs_summary)

  n_cost <- length(cost)
  n_eff <- length(effect)
  n_strat <- length(strategies)

  if (n_cost != n_eff | n_eff != n_strat) {
    stop("cost, effect, and strategies must all be vectors of the same length", 
         call. = FALSE)
  }
  char_strat <- as.character(strategies)
  df <- data.frame(Strategy = char_strat, Cost = cost, Effect = effect, 
                   stringsAsFactors = FALSE)
  nstrat <- nrow(df)
  if (nstrat == 1) {
    df[, c("ICER", "Inc_Cost", "Inc_Effect")] <- NA
    return(df)
  }
  d <- NULL
  df <- df[order(df$Cost, -df$Effect), ]
  for (i in 1:(nstrat - 1)) {
    ith_effect <- df[i, "Effect"]
    for (j in (i + 1):nstrat) {
      jth_effect <- df[j, "Effect"]
      if (jth_effect <= ith_effect) {
        d <- c(d, df[j, "Strategy"])
      }
    }
  }
  ed <- vector()
  continue <- TRUE
  while (continue) {
    dom <- union(d, ed)
    nd <- setdiff(strategies, dom)
    nd_df <- compute_icers(df[df$Strategy %in% nd, ])
    n_non_d <- nrow(nd_df)
    if (n_non_d <= 2) {
      break
    }
    nd_strat <- nd_df$Strategy
    new_ed <- 0
    for (i in 2:(n_non_d - 1)) {
      if (nd_df[i, "ICER"] > nd_df[i + 1, "ICER"]) {
        ed <- c(ed, nd_strat[i])
        new_ed <- new_ed + 1
      }
    }
    if (new_ed == 0) {
      continue <- FALSE
    }
  }

  results <- compute_icers(transform(nd_df[!(nd_df$Strategy %in% dom), ], Status = "ND"))
if (length(d) > 0){
    d_df <- transform(df[df$Strategy %in% d, ], Inc_Cost = NA, Inc_Effect = NA, ICER = NA, Status = "D")
    results <- rbind(results, d_df)
  }
  if (length(ed) > 0) {
    ed_df <- transform(df[df$Strategy %in% ed, ], Inc_Cost = NA, Inc_Effect = NA, ICER = NA, Status = "ED")
    results <- rbind(results, ed_df)
  }
  results <- results[order(results$Cost, -results$Effect), ]
  results <- results[, c("Strategy", "Cost", "Effect", "Inc_Cost", "Inc_Effect", "ICER", "Status")]
  colnames(results) <- c("decision", col_names[1], col_names[2], paste0("inc_", col_names[1]), paste0("inc_", col_names[2]), "ICER", "status")
  class(results) <- c("icers", "data.frame")
  return(results)
}

