
## -----------------------------------------------------------------------------
twig_obj <- twig() + 
  decisions("Amputate", "Antibiotics") + 
  event(name = "die",  
        options = c("yes", "none"), 
        probabilities = c("pDie", leftover), 
        transitions = c("Dead", "Alive")) + 
  payoffs(names = "utility")

## -----------------------------------------------------------------------------
# probability of death is 0.99 if the leg amputated and 0.8 if antibiotics given instead.
pDie <- function(decision, pDieAmputate, pDieAntibiotics){
  ifelse(decision == "Amputate", 0.6, 0.4)
}

#The utility is assumed to be 0.7 if the leg is amputated and 0.9 if antibiotics are given. 
utility <- function(outcome, utilityAmputate,  utilityAntibiotics){
    ifelse(outcome=="Alive", 1, 0.1)
}

params <- list(
  pDieAmputate = rbeta(n_sims, 1, 100),
  pDieAntibiotics = rbeta(n_sims, 2, 10),
  utilityAmputate = rbeta(n_sims, 7, 10), 
  utilityAntibiotics = rbeta(n_sims, 9, 10)
)
