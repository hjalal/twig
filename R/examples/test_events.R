# test advanced markov model
library(twig)
library(magrittr)
library(data.table)

# Define the number of samples

twig_obj <- twig() + 
  decisions("A") + 
  states(names=c("H", "D"), 
         init_probs=c(1,0)) + 
  event(name = "get_sick",  
        options = c("yes","none"), 
        probs = c("pGetSick", leftover), 
        transitions = c("die", "die")) +  
  event(name = "die",  
        options = c("yes", "none"), 
        probs = c("pDie", leftover), 
        transitions = c("D", "stay")) +  
  payoffs(names = c("cost", "utility"))



# probs

pGetSick <- function(state, p_get_sick_H){
  ifelse(state=="H", p_get_sick_H, 0)
}

pDie <- function(state, get_sick){
  ifelse(state=="D", 0, ifelse(get_sick=="yes", 0.5, 0.1))
}



# payoffs 
utility <- function(state, get_sick){
  ifelse(state=="H", ifelse(get_sick=="yes", 0.8, 1), 0) 
}

cost <- function(decision){
  1000
}
