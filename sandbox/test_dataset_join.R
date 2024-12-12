
#remove.packages("twig")
#Sys.unsetenv("GITHUB_PAT")
#remotes::install_github("hjalal/twig", build_vignettes = FALSE, force = TRUE)

library(data.table)
setDTthreads(20)
getDTthreads()
library(progress)
# ==============
#remove.packages("twig")

devtools::document(roclets = c('rd', 'collate', 'namespace'))
devtools::build(vignettes = FALSE)
# "/Users/hjalal/github/twig_0.0.1.0.tar.gz"

# HIV model testing ======
rm(list = ls())
n_sims= 1


#library(data.table)
source("~/github/HIVmodel/HIV_modelv11_additions_9November2024.R")
#source("sandbox/test_dataset_join_funs.R")
twig_env
start_time <- Sys.time()
results <- run_twig(twig_env, 
                    params = params_psa, 
                    n_cycles = n_cycles, 
                    return_prob = T, 
                    return_trace = T, 
                    return_function_evaluations = T,
                    return_total_payoff = T, 
                    check_prob_add_to_one = T)

print(results)
print(paste("It took:", Sys.time() - start_time))






# Markov model example ===========
rm(list = ls())
n_sims <- 1000
n_cycles <- 75

#library(data.table)
source("sandbox/test_markov.R")
#source("sandbox/test_dataset_join_funs.R")
twig_env

results <- run_twig(twig_env, 
                    params = params, 
                    n_cycles = n_cycles) #, 
                    return_prob = T, return_trace = T, return_function_evaluations = T,
                    return_total_payoff = T, check_prob_add_to_one = T)

print(results)




# Basic Markov ===========
rm(list = ls())
n_sims <- 1
n_cycles <- 1

#library(data.table)
source("sandbox/basic_markov.R")
#source("sandbox/test_dataset_join_funs.R")
twig_env

results <- run_twig(twig_env, 
                    params = params, 
                    n_cycles = n_cycles, 
                    return_prob = T, return_trace = T, return_function_evaluations = T,
                    return_total_payoff = T, check_prob_add_to_one = T)

print(results)


# Doubilet example ========
n_sims <- 1
source("sandbox/D3_dec_tree_Doubilet_1985_example.R")
#source("sandbox/test_dataset_join_funs.R")

twig_env

results <- run_twig(twig_env, 
                    params = params, 
                    return_prob = T, return_total_payoff = T, 
                    return_function_evaluations = T)

print(results)




# DARTH HVE example ========
n_sims <- 1
source("sandbox/D1_decision_tree_DARTH_HVE_example.R")
#source("sandbox/test_dataset_join_funs.R")

twig_env

results <- run_twig(twig_env, 
                    params = params, 
                    return_prob = T, return_total_payoff = T, 
                    return_function_evaluations = T)

print(results)



# Decision tree example ========
n_sims <- 1
source("sandbox/test_decision_tree.R")
#source("sandbox/test_dataset_join_funs.R")

twig_env

results <- run_twig(twig_env, 
                    params = params, 
                    return_prob = T, return_total_payoff = T)

print(results)



