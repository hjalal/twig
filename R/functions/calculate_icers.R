#' Calculate Incremental Cost-Effectiveness Ratios (ICERs)
#'
#' This function calculates the Incremental Cost-Effectiveness Ratios (ICERs) for a set of strategies based on their costs and effects.
#'
#' @param rewards_summary A matrix containing the summary statistics of the rewards. It should have columns for cost and utility.
#' @param col_names A character vector specifying the names of the columns for cost and utility. Default is c("cost", "utility").
#' @return A data frame with the following columns:
#' \itemize{
#'   \item{Strategy}{The name of the strategy.}
#'   \item{Cost}{The cost of the strategy.}
#'   \item{Effect}{The effect (utility) of the strategy.}
#'   \item{Inc_Cost}{The incremental cost compared to the next less effective strategy.}
#'   \item{Inc_Effect}{The incremental effect compared to the next less effective strategy.}
#'   \item{ICER}{The Incremental Cost-Effectiveness Ratio.}
#'   \item{Status}{The dominance status of the strategy (ND = non-dominated, D = dominated, ED = extendedly dominated).}
#' }
#' @export
#' @examples
#' # Example rewards summary matrix
#' rewards_summary <- matrix(
#'   c(1000, 2000, 1500, 0.8, 0.85, 0.82),
#'   nrow = 3,
#'   dimnames = list(c("StrategyA", "StrategyB", "StrategyC"), c("cost", "utility"))
#' )
#' 
#' # Calculate ICERs
#' icer_results <- calculate_icers(rewards_summary)
#' 
calculate_icers <- function (rewards_summary, col_names = c("cost", "utility")) {
cost <- rewards_summary[, col_names[1]]
effect <- rewards_summary[, col_names[2]]
strategies <- rownames(rewards_summary)

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

