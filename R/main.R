
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


# # ========= Decision tree example =========== 
n_sims <- 1

source("R/examples/D1_decision_tree_DARTH_HVE_example.R")

results <- run_twig(twig_obj, params, n_cycles, verbose = TRUE, parallel = FALSE)

str(results)
results$Rewards_summary
results$Rewards_sim
head(results$Event_Scenarios)
head(results$TransitionProb_array)

results$Outcomes

# Markov model example =================
n_sims <- 3
n_cycles <- 50
source("R/examples/test_markov.R")
#source("R/evaluate_prob_reward_functions.R")
twig_obj


results <- run_twig(twig_obj, params, n_cycles, verbose = TRUE, parallel = TRUE)

str(results)
results$Rewards_summary
results$Rewards_sim
head(results$Event_Scenarios)
head(results$TransitionProb_array)

results$Outcomes
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

    



