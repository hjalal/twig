
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

devtools::document(roclets = c('rd', 'collate', 'namespace'))
devtools::build(vignettes = FALSE)
# "/Users/hjalal/github/twig_0.0.1.0.tar.gz"

# global parameter 
rm(list = ls())

# Markov model example ===========
n_sims <- 5
n_cycles <- 10


#library(data.table)
source("sandbox/twig_internal_functions.R")
source("sandbox/test_markov.R")
source("sandbox/get_function_arrays.R")

str(twig_env)
twig_env$layers

# get the function arrays
get_function_arrays(twig_env = twig_env, n_cycles = n_cycles, n_sims = n_sims, params = params)

twig_env$fun_eval_list
twig_env$twig_funs
twig_env$str_fun_array_list
twig_env$fun_args
twig_env$fun_arg_values
twig_env$fun_arg_value_sizes

# events_df should be created and replaced with arrays and events set



events_df <- get_events_df(twig_env)
events_df

# instead of running the functions, we can just use the function arrays

# for each core function, get a string consisting of the arguments










twig_expand_functions(mytwig, 
                        "markov", 
                        params = params, 
                        n_cycles = n_cycles)


results <- run_twig(mytwig, 
                    params = params, 
                    n_cycles = 75) #, 
                    #return_prob = T, return_trace = T, return_total_payoff = T, check_prob_add_to_one = T)

print(results)
probs <- c(0.2, "x", "#", "fun1")
rep.int()
x <- c(1, 2, 3)


result <- to_strings(c(0.2, x, complement, fun1))
print(result)
print(result)

# Original vector
vec <- c(1, 2, 3)
rep.int(vec, each = 2)
# Repeat each element twice and the entire result three times
result <- rep(vec, each = 2, times = 3)

# Output the result
print(result)




# Decision tree example ========
n_sims <- 1
source("sandbox/test_decision_tree.R")
#source("sandbox/test_dataset_join_funs.R")

mytwig

results <- run_twig(mytwig, 
                    params = params, 
                    return_prob = T, return_total_payoff = T)

print(results)


