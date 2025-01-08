#' @importFrom foreach %dopar%

run_decision_twig <- function(twig_obj, params, verbose = FALSE, parallel = TRUE, hash_string = "leftover", ncore = NULL){

  message("Preprocessing started ...")

check_param_results <- check_params(params, verbose, parallel)
params <- check_param_results$params
n_sims <- check_param_results$n_sims
parallel <- check_param_results$parallel

decision_layer <- retrieve_layer_by_type(twig_obj, type = "decisions")
decision_names <- decision_layer$decisions
n_decisions <- length(decision_names)

payoff_layer <- retrieve_layer_by_type(twig_obj, type = "payoffs")
discount_rates <- payoff_layer$discount_rates

prob_funs <- get_prob_funs(twig_obj)
reward_funs <- get_reward_funs(twig_obj)

n_prob_funs <- length(prob_funs)

twig_funs <- c(prob_funs, reward_funs)
prob_reward_funs <- c(prob_funs, reward_funs)

fun_args <- get_function_args(twig_funs)

all_args <- unique(unlist(fun_args))

core_args <- get_core_args(twig_obj, all_args)

core_non_event_args <- get_core_non_event_args(all_args, twig_type = class(twig_obj))

event_args <- get_event_args(twig_obj, all_args)

sim_args <- get_sim_args(params, all_args)

arg_values <- get_arg_values(twig_obj, core_args, sim_args, n_sims)

arg_value_sizes <- get_arg_value_sizes(arg_values, core_args, sim_args, n_sims)

fun_core_df <- get_fun_core_df(twig_funs, fun_args, core_args, arg_values)

fun_sim_args <- get_fun_sim_args(twig_funs, fun_args, sim_args) 

check_function_arguments_decision(twig_funs, fun_args, core_args, sim_args)

core_non_event_outcome_args <- core_non_event_args[!core_non_event_args %in% "outcome"]
size_core_non_event_outcome_args <- arg_value_sizes[core_non_event_outcome_args]
total_size_core_non_event_outcome_args <- prod(size_core_non_event_outcome_args)

core_prob_args <- core_args[!core_args %in% "outcome"]
core_prob_arg_value_sizes <- arg_value_sizes[core_prob_args]
size_core_prob_arg_values <- prod(core_prob_arg_value_sizes)

size_core_non_event_args <- arg_value_sizes[core_non_event_args]
total_size_core_non_event_args <- prod(size_core_non_event_args)

core_arg_value_sizes <- arg_value_sizes[core_args]
size_core_arg_values <- prod(core_arg_value_sizes)

R_core_non_event_args <- core_non_event_args
size_R_core_non_event_args <- arg_value_sizes[R_core_non_event_args]
total_size_R_core_non_event_args <- prod(size_R_core_non_event_args)

dimnames_R0 <- arg_values[R_core_non_event_args]
dimnames_R0$reward <- reward_funs

IDX <- create_fun_array(prob_funs, fun_args, arg_value_sizes, core_prob_args, size_core_prob_arg_values)

F0 <- matrix(0, nrow = size_core_prob_arg_values, ncol = n_prob_funs)
dim_F <- c(core_prob_arg_value_sizes, prob_funs = n_prob_funs)
dimnames_F <- arg_values[core_prob_args]
dimnames_F$prob_funs <- prob_funs

events_df <- get_events_df(twig_obj)
event_options <- paste0(events_df$event, "_", events_df$options)
n_events <- nrow(events_df)
event_probs <- events_df$probs
event_ids <- events_df$event_id

event_prob_link <- match(event_probs, prob_funs)
non_compl_id <- which(!is.na(event_prob_link))
hash_id <- which(is.na(event_prob_link))
compl_id <- get_compl_event_ids(events_df, hash_string)

E0 <- matrix(NA, nrow = prod(core_prob_arg_value_sizes), ncol = n_events)

initial_event <- unique(events_df$event[!events_df$event %in% events_df$transitions])
if (length(initial_event) > 1) {
  stop(sprintf("There were multiple initial events: %s. There should be a single initial event.", paste(initial_event, collapse = ", ")))
}

paths <- build_lineage(initial_event, events_df)

n_paths <- length(paths)

A0_idx <- matrix(NA, nrow = total_size_core_non_event_outcome_args, ncol = n_paths)

dimnames_E <- arg_values[core_prob_args]

E0_df <- expand.grid(dimnames_E)
n_rows <- nrow(E0_df)
E_idx <- 1:n_rows
E0_logical <- rep(TRUE, n_rows)

all_event_args <- get_event_args(twig_obj, all_args, all_events = TRUE)

n_all_event_args <- length(all_event_args)

path_event_values <- get_path_event_values(n_paths, n_all_event_args, all_event_args, paths, events_df)

A_idx <- get_A_idx(A0_idx, n_paths, E0_logical, E0_df, event_args, path_event_values, E_idx)

outcome_names <- get_outcome_names(events_df)
dest_names <- get_dest_names(paths, events_df, outcome_names)
unique_dest_names <- unique(dest_names)
n_dest <- length(unique_dest_names)

dest_paths <- get_dest_paths_decision(dest_names, unique_dest_names)

n_rewards <- length(reward_funs)
reward_fun_args <- fun_args[reward_funs]

dimnames_rewards <- arg_values[core_args]
E0_rewards_df <- expand.grid(dimnames_rewards)
n_rows_rewards <- nrow(E0_rewards_df)
E_rewards_idx <- 1:n_rows_rewards
E0_logical_rewards <- rep(TRUE, n_rows_rewards)

A_idx_rewards <- get_A_idx_decision(A0_idx, n_paths, E0_logical_rewards, E0_rewards_df, 
  event_args, path_event_values, E_rewards_idx, dest_paths, core_args)

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

R0_array <- matrix(NA, nrow = total_size_core_non_event_args, ncol = n_rewards, 
            dimnames = list(NULL, reward_funs))

R_sim <- initialize_R_sim(n_decisions, 
                        n_rewards, 
                        n_sims, 
                        decision_names,
                        reward_funs)

path_events <- get_path_events(paths, events_df, n_paths, all_event_args, dest_paths)

twig_list <- list(
  A0_idx = A0_idx,
  A_idx = A_idx,
  E0 = E0,
  IDX = IDX,
  IDX_path_dep = IDX_path_dep,

  R0_array = R0_array,

  arg_value_sizes = arg_value_sizes,
  arg_values = arg_values,
  compl_id = compl_id,
  core_args = core_args,
  decision_names = decision_names,
  dest_paths = dest_paths,

  dimnames_R0 = dimnames_R0,

  event_prob_link = event_prob_link,
  F0 = F0,
  fun_args = fun_args,
  fun_core_df = fun_core_df,
  fun_sim_args = fun_sim_args,

  hash_id = hash_id,

  n_decisions = n_decisions,
  n_dest = n_dest,
  n_paths = n_paths,
  n_rewards = n_rewards,
  n_sims = n_sims,
  non_compl_id = non_compl_id,

  params = params,
  paths = paths,

  prob_funs = prob_funs,
  prob_reward_funs = prob_reward_funs,
  reward_funs = reward_funs,
  sim_args = sim_args,
  size_R_core_non_event_args = size_R_core_non_event_args,

  twig_funs = twig_funs,
  unique_dest_names = unique_dest_names,

  verbose = verbose
)

message("Preprocessing completed. Starting simulation...")

if (parallel){

  if (is.null(ncore)){
    ncore <- parallel::detectCores() - 1
  }
  cl <- parallel::makeCluster(ncore, outfile = "")
  doParallel::registerDoParallel(cl)

  parallel::clusterExport(cl, varlist = ls(globalenv()), envir = .GlobalEnv)

  pb <- utils::txtProgressBar(0, n_sims, style = 3)
  start_time <- Sys.time()

  R_sim <- foreach::foreach(sim = seq_len(n_sims), 
        .inorder = TRUE,
        .combine = function(...) abind::abind(..., along = 3),  
        .multicombine = TRUE, 
        .verbose = FALSE) %dopar% {
          utils::setTxtProgressBar(pb, sim) 

    run_decision_simulation(sim, twig_list, verbose = FALSE)

  }
  total_time <- Sys.time() - start_time

  parallel::stopCluster(cl)

  cat(sprintf("\nTotal time: %s\n", format(total_time, digits = 2)))
  close(pb)
  dim(R_sim) <- c(decision = n_decisions, reward = n_rewards, sim = n_sims)
  dimnames(R_sim) <- list(decision = decision_names, reward = reward_funs, sim = 1:n_sims)
} else { 
  if (verbose){ 
    results <- run_decision_simulation(1, twig_list, verbose = TRUE)
  } else {

  R_sim <- array(NA, dim = c(decision = n_decisions, reward = n_rewards, sim = n_sims), 
      dimnames = list(decision = decision_names, reward = reward_funs, sim = 1:n_sims))
  pb <- utils::txtProgressBar(0, n_sims, style = 3)
  start_time <- Sys.time()

  for (sim in seq_len(n_sims)) {
    R_sim[,,sim] <- run_decision_simulation(sim, twig_list, verbose = FALSE)
    utils::setTxtProgressBar(pb, sim) 
  }   

  total_time <- Sys.time() - start_time

  cat(sprintf("\nTotal time: %s\n", format(total_time, digits = 2)))
  close(pb)
  }

}

  if (verbose){
      Event_options_temp <- data.frame(results$Event_options)
      colnames(Event_options_temp) <- event_options
      Function_Values_temp <- data.frame(results$Function_Values)

      colnames(Function_Values_temp) <- prob_funs
      results$path_events <- path_events
      dimnames_A <- arg_values[core_non_event_args]

      results$Event_options <- cbind(E0_df, Event_options_temp)
      results$Prob_Function_Values <- cbind(E0_df, Function_Values_temp)

  } else {
    results <- list() 
    results$Rewards_summary <- apply(R_sim, c(1,2), mean)
    results$Rewards_sim <- R_sim
  }

  return(results)
}
