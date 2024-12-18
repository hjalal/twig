
#' Title
#'
#' @param rate 
#' @description converts rate to probability using prob = 1-exp(-rate)
#' @return probability 
#' @export
#'
#' @examples
#' rate2prob(rate = 0.3)
rate2prob <- function(rate){
  1 - exp(-rate)
}

#' Title
#'
#' @param prob 
#' @description converts prob to rate using rate = -log(1-prob)
#' @return rate 
#' @export
#'
#' @examples
#' prob2rate(prob = 0.5)
prob2rate <- function(prob){
  -log(1-prob)
}
