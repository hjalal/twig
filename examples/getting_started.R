library(twig)
mytwig <- twig() + 
  decisions(names = c("A", "B")) +  # Decision alternatives
  states(names = c("Alive", "Dead"),  # Markov state names
         init_probs = c(1, 0)) +  # The cohort starts healthy
  event(name = "death_event",  # A death event can   
        options = c("yes", "none"),  # have two options: "yes" and "none",
        probs = c(pDie, leftover),  # occur with probabilities: pDie and leftover = 1 - pDie
        transitions=c(Dead,stay)) + # can lead to death state otherwise stay in their current state, respectively.
  payoffs(names = c(cost, utility))  # Payoff function names

# 1. probability of death is a function of the state, decision and relative risk of mortality given treatment A
pDie <- function(state, decision, rrMortA){
  rDie <- 0.01*(state=="Alive") * rrMortA^(decision=="A") # rate of death is 20% if alive, 0 otherwise. This rate is multiplied by rrMortA for A, otherwise 1.
  rate2prob(rDie) # convert the rate into probability
}

# 2. cost is a function of the decision
cost <- function(decision, cA, cB){
  cA * (decision=="A") + 
  cB * (decision=="B")
}

# 3. utility is uAlive if alive, otherwise 0
utility <- function(state, uAlive){
  uAlive * (state=="Alive")
}

n_sims <- 1000 # number of simulations

psa_params <- data.frame(
  rrMortA = rnorm(n_sims, 0.9, 0.1), # Normal: relative risk of mortality
  cA = rlnorm(n_sims, 6, 1), # Log-normal: cost of A
  cB = rlnorm(n_sims, 5, 1), # Log-normal: cost of B
  uAlive = rbeta(n_sims, 0.8, 0.2)) # Beta: utility of being alive

head(psa_params) # examining the first 6 samples

results <- run_twig(twig_obj = mytwig, params = psa_params, n_cycles = 50, verbose = F, parallel = F)

print(results$mean_ev)

print(calculate_icers(results$mean_ev))

print(plot_ceac(results$sim_ev, wtp_range = seq(0, 100000, by = 1000)))
