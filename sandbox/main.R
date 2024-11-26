
#remove.packages("twig")
#Sys.unsetenv("GITHUB_PAT")
#remotes::install_github("hjalal/twig", build_vignettes = FALSE, force = TRUE)

library(data.table)
setDTthreads(9)
getDTthreads()
library(progress)
# ==============
#remove.packages("twig")

devtools::document(roclets = c('rd', 'collate', 'namespace'))
devtools::build(vignettes = FALSE)
# "/Users/hjalal/github/twig_0.0.1.0.tar.gz"

# global parameter 
rm(list = ls())


# Decision tree example ========
n_sims <- 1
source("sandbox/test_decision_tree.R")
#source("sandbox/test_dataset_join_funs.R")

mytwig

results <- run_twig(mytwig, 
                    params = params, 
                    return_prob = T, return_total_payoff = T)

print(results)


# Markov model example ===========
n_sims <- 5
#n_cycles <- 75

#library(data.table)
source("sandbox/test_dataset_join_example.R")
#source("sandbox/test_dataset_join_funs.R")
mytwig

results <- run_twig(mytwig, 
                    params = params, 
                    n_cycles = 75) #, 
                    #return_prob = T, return_trace = T, return_total_payoff = T, check_prob_add_to_one = T)

print(results)

