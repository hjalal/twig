rm(list = ls())

run_r_in_folder <- function(functions_folder){

    # List all .R files in the folder
    function_files <- list.files(functions_folder, pattern = "\\.R$", full.names = TRUE)

    # Source each file
    sapply(function_files, source)
    print(function_files)
}
run_r_in_folder("R/functions")


mytwig <- twig() + 
  decisions(names = c(A,B)) + # decision alternatives
  states(names=c(Alive,Dead), # Markov state names
         init_probs=c(1,0)) + # The cohort starts healthy
  event(name=death_event, # A death event can occur with 
      options=c(yes,none),  # 2 options "yes" and "none",
      probs=c(pDie,leftover), # that can occur with probabilities pDie and 1-pDie, 
      transitions=c(Dead,stay)) + # can lead to death state otherwise stay in their current state
  payoffs(names = c(cost, utility))  # we measure the cost and utility of the cohort


pDie <- function(state, decision, rrMortA){
  rDie <- 0.2 * (state=="Alive") * rrMortA ^ (decision=="A") # rate of probability increases at 0.01 per cycle (year)
  rate2prob(rDie) # convert the rate into probability
}

# cost is a function of the decision
cost <- function(decision, cA, cB){
  cA * (decision=="A") + 
  cB * (decision=="B")
}

# utility is uAlive if alive, otherwise 0
utility <- function(state, uAlive){
  uAlive * (state=="Alive")
}

n_sims <- 5000

psa_params <- data.frame(
  rrMortA = rnorm(n_sims, 0.95, 0.001),
  cA = rlnorm(n_sims, 5, 1),
  cB = rlnorm(n_sims, 4, 1),
  uAlive = rbeta(n_sims, 0.8, 0.2)
  )

head(psa_params)

results <- run_twig(twig_obj = mytwig, params = psa_params, n_cycles = 50, parallel = TRUE)

results$Rewards_summary

calculate_icers(results$Rewards_summary)


plot_ceac(results$Rewards_sim, wtp_range = seq(0, 100000, by = 1000))
head(nmb_proportions_df)

