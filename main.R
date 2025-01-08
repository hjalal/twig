
# nolint start
#remove.packages("twig")
#Sys.unsetenv("GITHUB_PAT")
#remotes::install_github("hjalal/twig", build_vignettes = FALSE, force = TRUE)

# library(data.table)
# setDTthreads(9)
# getDTthreads()
# library(progress)
# ==============
#remove.packages("twig")
# Or more practically, set to your terminal width
#options(width = 1000)  # adjust number as needed

#devtools::document(roclets = c('rd', 'collate', 'namespace'))
#devtools::build(vignettes = FALSE)
# "/Users/hjalal/github/twig_0.0.1.0.tar.gz"

# global parameter 

# # ========= Doubilet tree example =========== 
rm(list = ls())

run_r_in_folder <- function(functions_folder){

    # List all .R files in the folder
    function_files <- list.files(functions_folder, pattern = "\\.R$", full.names = TRUE)

    # Source each file
    sapply(function_files, source)
    print(function_files)
}
run_r_in_folder("R/functions")

n_sims <- 1

#source("R/examples/doubilet.R")
source("examples/D3_dec_tree_Doubilet_1985_example.R")
results <- run_twig(twig_obj, params, verbose = TRUE, parallel = FALSE)
results$Rewards_sim

str(results)
results$Rewards_summary
results$Rewards_sim
head(results$Event_options)
head(results$TransitionProb_array)
results$Prob_Function_Values
results$Outcomes

# decision        utility
#   BrainBiopsy 0.5580326
#   TreatAll    0.5660208
#   TreatNone   0.4940027



# # ========= Decision tree example ===========

rm(list = ls())

run_r_in_folder <- function(functions_folder){

    # List all .R files in the folder
    function_files <- list.files(functions_folder, pattern = "\\.R$", full.names = TRUE)

    # Source each file
    sapply(function_files, source)
    print(function_files)
}
run_r_in_folder("R/functions")
#library(data.table)
#source("R/twig_internal_functions.R")

n_sims <- 1

source("examples/D1_decision_tree_DARTH_HVE_example.R")

results <- run_twig(mytwig, params, verbose = TRUE, parallel = FALSE)

results$Rewards_sim

calculate_icers(results$Rewards_sim)


str(results)
results$Rewards_summary
results$Rewards_sim
head(results$Event_options)
head(results$TransitionProb_array)

results$Outcomes


my_decision_model(params)
#>                cost effectiveness
#> Biopsy     32599.41      19.69896
#> DoNotTreat  4117.20      19.62600
#> Treat      12908.96      19.71680



# Markov model example =================
rm(list = ls())
options(width = 200)
run_r_in_folder <- function(functions_folder){

    # List all .R files in the folder
    function_files <- list.files(functions_folder, pattern = "\\.R$", full.names = TRUE)

    # Source each file
    sapply(function_files, source)
    print(function_files)
}
run_r_in_folder("R/functions")
n_sims <- 5000
n_cycles <- 75


source("examples/test_markov.R")
#source("R/evaluate_prob_reward_functions.R")
twig_obj


results <- run_twig(twig_obj, params, n_cycles, verbose = FALSE, parallel = TRUE,  offset_trace_cycle = 0)

results$Rewards_summary

#                    cost  utility
# StandardOfCare 12938.81 4.756735
# StrategyA      22979.69 4.911393
# StrategyB      23475.64 4.764146
# StrategyAB     33457.56 4.922687

head(psa_params)

results <- run_twig(twig_obj = mytwig, params = psa_params, n_cycles = 50, parallel = TRUE)

results$Rewards_summary

calculate_icers(results$Rewards_summary)


plot_ceac(results$Rewards_sim, wtp_range = seq(0, 100000, by = 1000))
head(nmb_proportions_df)




str(results)
results$Trace_array
results$TransitionProb_array
results$Rewards_summary
results$Rewards_sim
head(results$Event_options)
head(results$TransitionProb_array)

results$Rewards_array_cycle

results$Outcomes
head(results$Function_Values)
View(results$Function_Values)

# # 1. evaluate functions ----------------
# # all functions in the twig
# source("R/steps/step_1_evaluate_functions.R")

# # 2. IDX = matrix[(D,S,C,E1,E2,...etc), prob_funs] the indices of each vector harmonized to the core-args
# # apply this to each simulation sim
# # initialize F0 and dimnames and sizes
# source("R/steps/step_2_get_function_arrays.R")

# # Prep steps
# # Initialize the F0 matrix
# source("R/prep/step_3_initialize_F0.R")
# source("R/prep/step_4_initialize_E0.R")
# source("R/prep/step_5_initialize_A0.R")
# source("R/prep/step_6_initialize_P0.R")
# source("R/prep/step_9_prepare_R_idx_R0_array.R")


# print(R_array_cycle)
# print(R_sim)
# print(R_summary)

# time-independent Markov model example =================
rm(list = ls())
options(width = 200)
run_r_in_folder <- function(functions_folder){

    # List all .R files in the folder
    function_files <- list.files(functions_folder, pattern = "\\.R$", full.names = TRUE)

    # Source each file
    sapply(function_files, source)
    print(function_files)
}
run_r_in_folder("R/functions")

mytwig <- twig() + 
  decisions(names=c(A,B)) + # decision alternatives
  states(names=c(Alive,Dead), # Markov state names
         init_probs=c(1,0)) + # The cohort starts healthy
  event(name=death_event, # A death event can occur with 
      options=c(yes,none),  # 2 options "yes" and "none",
      probs=c(pDie,leftover), # that can occur with probabilities pDie and leftover = 1-pDie, 
      transitions=c(Dead,stay)) + # can lead to death state otherwise stay in their current state
  payoffs(names = c(cost, utility))  # will capture the cost and utility

pDie <- function(state, rrMortA){
  rDie <- rrMortA * (state=="Alive") # rate of probability increases at 0.01 per cycle (year)
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
  rrMortA = rnorm(n_sims, 0.01, 0.001), # relative risk of mortality
  cA = rlnorm(n_sims, 10, 1), # cost of A
  cB = rlnorm(n_sims, 12, 1), # cost of B
  uAlive = rbeta(n_sims, 0.8, 0.2)) # utility of being alive

head(psa_params) # examining the first 6 samples

results <- run_twig(twig_obj = mytwig, params = psa_params, n_cycles = 50, verbose = F, parallel = T)

results$Rewards_summary

