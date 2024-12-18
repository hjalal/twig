
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
functions_folder <- "R/functions"

# List all .R files in the folder
function_files <- list.files(functions_folder, pattern = "\\.R$", full.names = TRUE)

# Source each file
sapply(function_files, source)

#library(data.table)
#source("R/twig_internal_functions.R")
source("R/test_markov.R")
#source("R/evaluate_prob_reward_functions.R")

twig_obj
# 1. evaluate functions ----------------
# all functions in the twig
source("R/step_1_evaluate_functions.R")

# 2. IDX = matrix[(D,S,C,E1,E2,...etc), prob_funs] the indices of each vector harmonized to the core-args
# apply this to each simulation sim
# initialize F0 and dimnames and sizes
source("R/step_2_get_function_arrays.R")

# Prep steps
# Initialize the F0 matrix
source("R/prep/step_3_initialize_F0.R")
source("R/prep/step_4_initialize_E0.R")
source("R/prep/step_5_initialize_A0.R")
source("R/prep/step_6_initialize_P0.R")
source("R/prep/step_9_prepare_R_idx_R0_array.R")

# For each sim: 
sim <- 1
sim_offset <- lapply(prob_reward_funs, function(x) (sim - 1) * sim_offset0[[x]])
names(sim_offset) <- prob_reward_funs
# 3. F(sim) = same as IDX. Harmonize probs sim -------------------------------------------------
# parallellize
# add an option to store and output intermediate matrices with a warning about matrix sizes

source("R/step_3_harmonize_probs.R")

# 4. E: Create a single event array  -------------------------------------------------
# if any is cycle dependent, dims = j=D, S, +/-C, j=event_id
# for complement probs # = 1 - sum other probs 

source("R/step_4_event_array.R")
print(E)

# 5. A[,,,k]: Create a single path array ---------------------------------------------
# product of all E[,,,j] that are in on each path k=path_id

source("R/step_5_path_array.R")
print(A)

# 6. P: Create transition probs ----------------------------------------
# sum of all A[,,,k] that lead to the same destination Y

source("R/step_6_transition_probs.R")
print(P_array)

# 7. P0: Expand initial prob ----------------------------------------
# can be numeric, global variable or a function of D, sim.
source("R/step_7_expand_initial_prob.R")
print(p0_array)

# 8. T: Create trace ---------------------------------------------------------
# iteratively multiply state distribution by P
source("R/step_8_create_trace.R")
print(T_array)

# 9. R0: create a single array for all event-dep rewards by path k --------------
# rewards are by evnets, but have to be made dependent on path k
# 10. multiply event-dep rewards and event arrays---------------------------
# for each reward doing a redim so it is [DSC * K] and then doing an element 
# wise multiplication, and then colSum, and then redimming to D, S, +/-C.


# 11. R: create a single array of all rewards ------------------------------
# iteratre through each reward, and fill in a single array D,S,C,r=reward_id
source("R/step_9_reward_event_dep_array.R")
print(R_array)

# 10. RC: multiply rewards and trace R * T * discount -------------------------------------------

# 11. RS: create summary payoffs ----------------------------------------------
# consider if count_initial_cycle = "yes", "no", or "half_cycle_correction"
# output an alert, saying, you specified ... 
print(R_array_cycle)
print(R_sim)
print(R_summary)


