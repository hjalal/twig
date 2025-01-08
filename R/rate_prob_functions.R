#' Convert Rate to Probability
#'
#' This function converts a rate to a probability using the formula `1 - exp(-rate)`.
#'
#' @param rate A numeric value representing the rate.
#' @return A numeric value representing the probability.
#' @export
#' @examples
#' rate <- 0.1
#' prob <- rate2prob(rate)
#' print(prob)
rate2prob <- function(rate){
  1 - exp(-rate)
}

#' Convert Probability to Rate
#'
#' This function converts a probability to a rate using the formula `-log(1 - prob)`.
#'
#' @param prob A numeric value representing the probability.
#' @return A numeric value representing the rate.
#' @export
#' @examples
#' prob <- 0.1
#' rate <- prob2rate(prob)
#' print(rate)
prob2rate <- function(prob){
  -log(1 - prob)
}
