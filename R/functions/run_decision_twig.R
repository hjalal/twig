run_decision_twig <- function(twig_obj, params, verbose = FALSE, parallel = TRUE, hash_string = "leftover"){

check_param_results <- check_params(params, verbose, parallel)
params <- check_param_results$params
n_sims <- check_param_results$n_sims
parallel <- check_param_results$parallel

# a flag to indicate if the reward functions are dependent on the outcome
# is_reward_outcome_dep <- FALSE

# prep 1 ------------------------------------------------ 
# get the number of expanded states and their sizes.
# also get state_layer

decision_layer <- retrieve_layer_by_type(twig_obj, type = "decisions")
decision_names <- decision_layer$decisions
n_decisions <- length(decision_names)

payoff_layer <- retrieve_layer_by_type(twig_obj, type = "payoffs")
discount_rates <- payoff_layer$discount_rates

# steps to evaluate a function
prob_funs <- get_prob_funs(twig_obj)
reward_funs <- get_reward_funs(twig_obj)


n_prob_funs <- length(prob_funs)

twig_funs <- c(prob_funs, reward_funs)
prob_reward_funs <- c(prob_funs, reward_funs)
# used prob and reward arguments in the twig
fun_args <- get_function_args(twig_funs)


# both probabiliteis and rewards --------------------------------
# unique arguments in twig functions
all_args <- unique(unlist(fun_args))

# used core arguments D, S, C, E(s), O
core_args <- get_core_args(twig_obj, all_args)

# core non-event arguments 
core_non_event_args <- get_core_non_event_args(all_args)

# event arguments
event_args <- get_event_args(twig_obj)

# use psa arguments from the parameters column names
sim_args <- get_sim_args(params, all_args)

# get used argument values 
arg_values <- get_arg_values(twig_obj, core_args, sim_args)

# get argument value sizes
arg_value_sizes <- get_arg_value_sizes(arg_values, core_args, sim_args)

# get a dataframe for the combinatorials of the core arguments
fun_core_df <- get_fun_core_df(twig_funs, fun_args, core_args, arg_values)

# get teh functions simulation arguments
fun_sim_args <- get_fun_sim_args(twig_funs, fun_args, sim_args) 


# if outcome is one of the arguments - it has to be only in rewards, and not probabilities 
# will need a special treatment
# if ("outcome" %in% core_args){
#   core_args <- core_args[core_args != "outcome"]
#   core_non_event_args <- core_non_event_args[core_non_event_args != "outcome"]
#   is_reward_outcome_dep <- TRUE
# }


# probabilities only --------------------------------

core_non_event_outcome_args <- core_non_event_args[!core_non_event_args %in% "outcome"]
size_core_non_event_outcome_args <- arg_value_sizes[core_non_event_outcome_args]
total_size_core_non_event_outcome_args <- prod(size_core_non_event_outcome_args)

# Get the IDX of indices of the arguments in the sorted arguments
core_prob_args <- core_args[!core_args %in% "outcome"]
core_prob_arg_value_sizes <- arg_value_sizes[core_prob_args]
size_core_prob_arg_values <- prod(core_prob_arg_value_sizes)



# Rewards only --------------------------------
size_core_non_event_args <- arg_value_sizes[core_non_event_args]
total_size_core_non_event_args <- prod(size_core_non_event_args)

# Get the IDX of indices of the arguments in the sorted arguments
core_arg_value_sizes <- arg_value_sizes[core_args]
size_core_arg_values <- prod(core_arg_value_sizes)

R_core_non_event_args <- core_non_event_args
size_R_core_non_event_args <- arg_value_sizes[R_core_non_event_args]
total_size_R_core_non_event_args <- prod(size_R_core_non_event_args)
# evaluate all functions in the twig and generate a vector for each function



# dims and dimnames:tunneled_states
dimnames_R0 <- arg_values[R_core_non_event_args]
dimnames_R0$reward <- reward_funs


# prep 2: get function arrays --------------------------------
IDX <- create_fun_array(prob_funs, fun_args, arg_value_sizes, core_prob_args, size_core_prob_arg_values)

# prep 3: initialisation of the F0 matrix -------------------
# and the dimension of the F array and its dimnames
F0 <- matrix(0, nrow = size_core_prob_arg_values, ncol = n_prob_funs)
dim_F <- c(core_prob_arg_value_sizes, prob_funs = n_prob_funs)
dimnames_F <- arg_values[core_prob_args]
dimnames_F$prob_funs <- prob_funs

# prep 4: initialisation of the E0 matrix -------------------
# mapping part that would be pre-determined for all sims
events_df <- get_events_df(twig_obj)
event_options <- paste0(events_df$event, "_", events_df$values)
n_events <- nrow(events_df)
event_probs <- events_df$probabilities
event_ids <- events_df$event_id

# browser()
event_prob_link <- match(event_probs, prob_funs)
non_compl_id <- which(!is.na(event_prob_link))
hash_id <- which(is.na(event_prob_link))
compl_id <- get_compl_event_ids(events_df, hash_string)


E0 <- matrix(NA, nrow = prod(core_prob_arg_value_sizes), ncol = n_events)

# path array initialization
# and the dimension of the F array and its dimnames

# get path df

# Build paths from the niitial event
# intitial event must be a single event
initial_event <- unique(events_df$event[!events_df$event %in% events_df$transitions])
if (length(initial_event) > 1) {
  stop(sprintf("There were multiple initial events: %s. There should be a single initial event.", paste(initial_event, collapse = ", ")))
}
# generate paths pointing from each event to a final state (could be duplicated)
paths <- build_lineage(initial_event, events_df)

# Print the lineages
n_paths <- length(paths)


# a place holder for the indices for the paths matrix 
A0_idx <- matrix(NA, nrow = total_size_core_non_event_outcome_args, ncol = n_paths)

#list of indices of the event array
# dim_E <- c(arg_value_sizes[core_args], event_id = n_events)
dimnames_E <- arg_values[core_prob_args]
#dimnames_E$event_id <- 1:n_events

# the idea is to create a logical array that will be used to filter the E0 array
# one event at a time.
E0_df <- expand.grid(dimnames_E)
n_rows <- nrow(E0_df)
E_idx <- 1:n_rows
E0_logical <- rep(TRUE, n_rows)

# allow for all possible events (no/NA = "none")
n_event_args <- length(event_args)

path_event_values <- get_path_event_values(n_paths, n_event_args, event_args, paths, events_df)

A_idx <- get_A_idx(A0_idx, n_paths, E0_logical, E0_df, event_args, path_event_values, E_idx)


# IDX_path_dep <- get_IDX_path_dep(A0_idx, n_paths, E0_logical, E0_df, event_args, path_event_values, E_idx)

# prep 6: initialisation of the P0 matrix -------------------
# Prep Transition probabilities:

# cross walk between dest and path_ids
# get unique dest names

# get dest names


outcome_names <- get_outcome_names(events_df)
dest_names <- get_dest_names(paths, events_df, outcome_names)
unique_dest_names <- unique(dest_names)
n_dest <- length(unique_dest_names)


# dest_paths <- get_dest_paths(dest_names, unique_dest_names, outcome_names)
dest_paths <- get_dest_paths_decision(dest_names, unique_dest_names)


# get_path_name(7, dest_paths)
# for each sim get the transition probabilities
#  Transition probabilities logic for each sim

# prep 7: initialisation of the T0 matrix -------------------
# 9. R0: create a single array for all event-dep rewards by path k --------------
# rewards are by evnets, but have to be made dependent on path k

#reward_funs
n_rewards <- length(reward_funs)
reward_fun_args <- fun_args[reward_funs]
#reward_fun_values <- eval_funs[reward_funs]
#reward_fun_arg_sizes <- arg_value_sizes[reward_fun_args]

# preparation ----------------
# get ids 
# get reward names that are event dependent
# is_reward_event_dep <- get_event_dep_rewards(reward_funs, fun_args, event_args)

# event_dep_rewards <- reward_funs[is_reward_event_dep]
# event_indep_rewards <- reward_funs[!is_reward_event_dep]
# n_event_dep_rewards <- length(event_dep_rewards)

# for event independent rewards, just expand their dimensions to match the core arguments ----------------


# R_idx <- create_fun_array(event_indep_rewards, 
#                                     fun_args, 
#                                     arg_value_sizes, 
#                                     R_core_non_event_args, 
#                                     total_size_R_core_non_event_args)
# R_idx
# #core_non_event_args



# for event denpendent rewards, mulitply by the path probabilities and sum ----------------

# one event at a time.
dimnames_rewards <- arg_values[core_args]
E0_rewards_df <- expand.grid(dimnames_rewards)
n_rows_rewards <- nrow(E0_rewards_df)
E_rewards_idx <- 1:n_rows_rewards
E0_logical_rewards <- rep(TRUE, n_rows_rewards)

# similar to A_idx, but also accoutns for outcomes in rewards
# browser()
A_idx_rewards <- get_A_idx_decision(A0_idx, n_paths, E0_logical_rewards, E0_rewards_df, 
  event_args, path_event_values, E_rewards_idx, dest_paths, core_args)


# get reward function array of indices for a single simulation
IDX_R <- create_fun_array_decision_reward(funs = reward_funs, 
  fun_args = fun_args, 
  arg_value_sizes = arg_value_sizes, 
  core_args = core_args, 
  size_arg_values = size_core_arg_values)

IDX_path_dep <- get_IDX_path_dep(A_idx_rewards, 
                                IDX_R, 
                                n_paths, 
                                n_rewards, 
                                total_size_core_non_event_outcome_args, 
                                reward_funs)

# combine with the event independent rewards to get a single array for all rewards


# get the reward values for each event

R0_array <- matrix(NA, nrow = total_size_core_non_event_args, ncol = n_rewards, 
            dimnames = list(NULL, reward_funs))


# summary array dimensions and names 
R_sim <- initialize_R_sim(n_decisions, 
                        n_rewards, 
                        n_sims, 
                        decision_names,
                        reward_funs)

# define additional intermediate objects to be returned when verbose = TRUE --------------
# get path keys 

path_events <- get_path_events(paths, events_df, n_paths, event_args, dest_paths)

# browser()

# for each simulation harmonize teh probabilities ----------------
# For each sim: 
twig_list <- list(
  A0_idx = A0_idx,
  A_idx = A_idx,
  E0 = E0,
  IDX = IDX,
  IDX_path_dep = IDX_path_dep,
  # P0_mat = P0_mat,
  # p0_array = p0_array,
  R0_array = R0_array,
  #R_non_event_dep_idx = R_non_event_dep_idx,
  #array_discount = array_discount,
  arg_value_sizes = arg_value_sizes,
  arg_values = arg_values,
  compl_id = compl_id,
  core_args = core_args,
  decision_names = decision_names,
  dest_paths = dest_paths,
  # dim_P = dim_P,
  # dimnames_P = dimnames_P,
  dimnames_R0 = dimnames_R0,
  #event_dep_rewards = event_dep_rewards,
  #event_indep_rewards = event_indep_rewards,
  event_prob_link = event_prob_link,
  F0 = F0,
  fun_args = fun_args,
  fun_core_df = fun_core_df,
  fun_sim_args = fun_sim_args,

  hash_id = hash_id,
  #is_cycle_dep = is_cycle_dep,
  #n_cycles = n_cycles,
  n_decisions = n_decisions,
  n_dest = n_dest,
  n_paths = n_paths,
  n_rewards = n_rewards,
  n_sims = n_sims,
  non_compl_id = non_compl_id,
  #p_stay = p_stay,
  params = params,
  paths = paths,
  #p0_funs = p0_funs,
  prob_funs = prob_funs,
  prob_reward_funs = prob_reward_funs,
  reward_funs = reward_funs,
  sim_args = sim_args,
  size_R_core_non_event_args = size_R_core_non_event_args,
  #total_size_core_non_event_args = total_size_core_non_event_args,
  #state_layer = state_layer,
  # twig_funs = twig_funs,
  unique_dest_names = unique_dest_names,
  #unique_non_current_dest = unique_non_current_dest,
  verbose = verbose
)

# Convert the list to an environment
#twig_env <- list2env(twig_list, envir = new.env())

# Set the environment of the function to twig_env
library(progress)
if (parallel){
  library(parallel)
  library(doParallel)
  library(abind)
  ncore <- detectCores() - 1
  cl <- makeCluster(ncore, outfile = "")
  registerDoParallel(cl)
  #clusterExport(cl, varlist = ls(twig_env), envir = twig_env)

  #clusterExport(cl, varlist = ls(twig_env), envir = twig_env)
  #clusterExport(cl, varlist = c("run_decision_simulation", "retrieve_layer_by_type", "get_prob_funs", "get_reward_funs", "get_p0_funs", "get_function_args", "get_core_args", "get_core_non_event_args", "get_event_args", "get_sim_args", "get_arg_values", "get_arg_value_sizes", "evaluate_function", "get_fun_idx_offset", "compute_sim_offset", "create_fun_array", "get_events_df", "get_compl_event_ids", "build_lineage", "get_path_event_values", "get_A_idx", "get_dest_names", "expand_dest_state", "get_dest_paths", "get_stay_indices", "get_event_dep_rewards", "initialize_R_sim", "get_array_discount"), envir = .GlobalEnv)
  # Export all objects from the global environment to each worker
  clusterExport(cl, varlist = ls(globalenv()), envir = .GlobalEnv)
  
  # R_sim <- foreach(sim = seq_len(n_sims), .combine = 'c') %dopar% {
  #   run_decision_simulation(sim, twig_list, verbose = FALSE)
  # }
  pb <- txtProgressBar(0, n_sims, style = 3)
  start_time <- Sys.time()

  R_sim <- foreach(sim = seq_len(n_sims), 
        .combine = function(...) abind(..., along = 3),  
        .multicombine = TRUE, 
        .verbose = FALSE) %dopar% {
    setTxtProgressBar(pb, sim) # update the progress bar

    run_decision_simulation(sim, twig_list, verbose = FALSE)
    #flush.console()  # Force the update to display in VS Code
  }
  total_time <- Sys.time() - start_time
  # Stop the parallel backend
  stopCluster(cl)
  # print the top simulations
  cat(sprintf("\nTotal time: %s\n", format(total_time, digits = 2)))
  close(pb)
  dim(R_sim) <- c(decision = n_decisions, reward = n_rewards, sim = n_sims)
  dimnames(R_sim) <- list(decision = decision_names, reward = reward_funs, sim = 1:n_sims)
} else { # sequential
  if (verbose){ # return simulation details
    results <- run_decision_simulation(1, twig_list, verbose = TRUE)
  } else {
  #environment(run_decision_simulation) <- twig_env
  
  R_sim <- array(NA, dim = c(decision = n_decisions, reward = n_rewards, sim = n_sims), 
      dimnames = list(decision = decision_names, reward = reward_funs, sim = 1:n_sims))
  pb <- txtProgressBar(0, n_sims, style = 3)
  start_time <- Sys.time()

  for (sim in seq_len(n_sims)) {
    R_sim[,,sim] <- run_decision_simulation(sim, twig_list, verbose = FALSE)
    setTxtProgressBar(pb, sim) # update the progress bar
  }   # end sim loop

  total_time <- Sys.time() - start_time
  # print the top simulations
  cat(sprintf("\nTotal time: %s\n", format(total_time, digits = 2)))
  close(pb)
  }

}
  # if verbose, return the detailed results, otherwise, return the summary
  # and supplement of other intermediate objects
  if (verbose){
      Event_options_temp <- data.frame(results$Event_options)
      colnames(Event_options_temp) <- event_options
      Function_Values_temp <- data.frame(results$Function_Values)
      # browser()
      colnames(Function_Values_temp) <- prob_funs
      results$path_events <- path_events
      dimnames_A <- arg_values[core_non_event_args]
      #A_df <- expand.grid(dimnames_A)
      results$Event_options <- cbind(E0_df, Event_options_temp)
      results$Prob_Function_Values <- cbind(E0_df, Function_Values_temp)
      #browser()
      #results$Reward_Function_Values <- cbind(E0_rewards_df, results$Reward_Function_Values)
      #browser()
      #results$Paths <- cbind(A_df, results$Paths)
  } else {
    results <- list() 
    results$Rewards_summary <- apply(R_sim, c(1,2), mean)
    results$Rewards_sim <- R_sim
  }

  return(results)
}
