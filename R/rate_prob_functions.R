#' Convert Rate to Probability
#'
#' This function converts a rate to a probability using the formula `1 - exp(-rate)`.
#'
#' @param rate A numeric value (or vector) representing the rate. Rates must be
#'   non-negative.
#' @return A numeric value representing the probability.
#' @export
#' @examples
#' rate <- 0.1
#' prob <- rate2prob(rate)
#' print(prob)
rate2prob <- function(rate){
  if (!is.numeric(rate)) {
    stop("rate must be numeric.")
  }
  if (any(rate < 0, na.rm = TRUE)) {
    stop("rate must be non-negative.")
  }
  1 - exp(-rate)
}

#' Convert Probability to Rate
#'
#' This function converts a probability to a rate using the formula `-log(1 - prob)`.
#'
#' @param prob A numeric value (or vector) representing the probability. Must be
#'   in the interval `[0, 1]`. A probability of exactly 1 returns `Inf`.
#' @return A numeric value representing the rate.
#' @export
#' @examples
#' prob <- 0.1
#' rate <- prob2rate(prob)
#' print(rate)
prob2rate <- function(prob){
  if (!is.numeric(prob)) {
    stop("prob must be numeric.")
  }
  if (any(prob < 0 | prob > 1, na.rm = TRUE)) {
    stop("prob must be in the interval [0, 1].")
  }
  -log(1 - prob)
}
