









rate2prob <- function(rate){
  1 - exp(-rate)
}










prob2rate <- function(prob){
  -log(1-prob)
}
