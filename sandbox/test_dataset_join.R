
#remove.packages("twig")
#Sys.unsetenv("GITHUB_PAT")
#remotes::install_github("hjalal/twig", build_vignettes = FALSE, force = TRUE)

library(data.table)
library(progress)
# ==============
#remove.packages("twig")

devtools::document(roclets = c('rd', 'collate', 'namespace'))
devtools::build(vignettes = FALSE)
# "/Users/hjalal/github/twig_0.0.1.0.tar.gz"

# global parameter 
rm(list = ls())

n_sims <- 1
n_cycles <- 75


#library(data.table)
source("sandbox/test_dataset_join_example.R")
#source("sandbox/test_dataset_join_funs.R")
setDTthreads(9)
getDTthreads()
mytwig

results <- run_twig(mytwig, params, n_cycles, return_prob = T, return_trace = T, return_total_payoff = T, check_prob_add_to_one = T)

results
