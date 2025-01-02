# test advanced markov model
#library(twig)
#library(magrittr)

# Define the number of samples

twig_obj <- twig() + # for illustration it is 75 in the tutorial 
  decisions("A", "B") + 
  states(names=c("H", "D"), 
         init_probs=c(1,0),
         tunnel_lengths=c(1,1)) + 
  event(name = "die",  
        options = c("yes","none"), 
        probabilities = c("pDie", leftover), 
        transitions = c("D", "curr_state")) +  
  payoffs(names = c("utility"))

params <- list(
  pDieA = 0.5,
  pDieB = 0.3
)
pDie <- function(decision, pDieA, pDieB){
  ifelse(decision == "A", pDieA, pDieB)
}

utility <- function(state){
  ifelse(state=="H", 1, 0)
}
