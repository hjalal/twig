
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

# Markov model example ===========
n_sims <- 2
n_cycles <- 3

# Specify the path to the "functions" folder
functions_folder <- "sandbox/functions"

# List all .R files in the folder
function_files <- list.files(functions_folder, pattern = "\\.R$", full.names = TRUE)

# Source each file
sapply(function_files, source)

#library(data.table)
#source("sandbox/twig_internal_functions.R")
source("sandbox/test_markov.R")
#source("sandbox/evaluate_prob_reward_functions.R")

twig_obj
# 1. evaluate functions ----------------
# all functions in the twig
prob_funs <- get_prob_funs(twig_obj)
reward_funs <- get_reward_funs(twig_obj)
p0_funs <- get_p0_funs(twig_obj)

twig_funs <- c(prob_funs, reward_funs, p0_funs)

# used prob and reward arguments in the twig
fun_args <- get_function_arguments(twig_funs)

# unique arguments in twig functions
all_args <- unique(unlist(fun_args))

# used core arguments D, S, C, E(s), O
core_args <- get_core_args(twig_obj, all_args)

# use psa arguments from the parameters column names
sim_args <- get_sim_args(params, all_args)

# get used argument values 
arg_values <- get_arg_values(twig_obj, core_args, sim_args, n_cycles)

# get argument value sizes
arg_value_sizes <- get_arg_value_sizes(arg_values, core_args, sim_args)

# evaluate all functions in the twig and generate a vector for each function
eval_funs <- evaluate_function(twig_funs, fun_args, core_args, sim_args, arg_values, params)
eval_funs

# 0. F: expand functions and create an individual function array for probs and rewards ---------------
# this includes function dimensions, and n_sim for PSA inputs
# harmonize probs, if any is cycle dependent, dims = D, S, +/-C, E1, E2, ..., sim


# 1. sample a single sim -------------------------------------------------
# parallellize
# add an option to store and output intermediate matrices with a warning about matrix sizes

# 3. E: Create a single event array  -------------------------------------------------
# harmonize probs, if any is cycle dependent, dims = j=D, S, +/-C, j=event_id
# for complement probs # = 1 - sum other probs 


# 4. A[,,,k]: Create a single path array ---------------------------------------------
# product of all E[,,,j] that are in on each path k=path_id


# 5. P: Create transition probs ----------------------------------------
# sum of all A[,,,k] that lead to the same destination Y


# 6. P0: Expand initial prob ----------------------------------------
# can be numeric, global variable or a function of D, sim.


# 6. T: Create trace ---------------------------------------------------------
# iteratively multiply state distribution by P


# 7. R0: create a single array for all event-dep rewards by path k --------------
# rewards are by evnets, but have to be made dependent on path k


# 8. multiply event-dep rewards and event arrays---------------------------
# for each reward doing a redim so it is [DSC * K] and then doing an element 
# wise multiplication, and then colSum, and then redimming to D, S, +/-C.


# 9. R: create a single array of all rewards ------------------------------
# iteratre through each reward, and fill in a single array D,S,C,r=reward_id


# 10. RC: multiply rewards and trace R * T * discount -------------------------------------------


# 11. RS: create summary payoffs ----------------------------------------------
# consider if count_initial_cycle = "yes", "no", or "half_cycle_correction"
# output an alert, saying, you specified ... 



