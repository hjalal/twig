#' @importFrom foreach %dopar%

run_markov_twig <- function(twig_obj, params, n_cycles, verbose = FALSE, parallel = TRUE, 
                            hash_string = "leftover", offset_trace_cycle = 1, ncore = NULL, progress_bar = TRUE){

  message("Preprocessing started...")

    check_param_results <- check_params(params, verbose, parallel)
  params <- check_param_results$params
  n_sims <- check_param_results$n_sims
  parallel <- check_param_results$parallel

state_layer <- retrieve_layer_by_type(twig_obj, type = "states")
expanded_states <- state_layer$tunneled_states
n_expanded_states <- length(expanded_states)

decision_layer <- retrieve_layer_by_type(twig_obj, type = "decisions")
decision_names <- decision_layer$decisions
n_decisions <- length(decision_names)

payoff_layer <- retrieve_layer_by_type(twig_obj, type = "payoffs")
discount_rates <- payoff_layer$discount_rates

prob_funs <- get_prob_funs(twig_obj)
payoff_funs <- get_payoff_funs(twig_obj)
p0_funs <- get_p0_funs(twig_obj)

n_prob_funs <- length(prob_funs)

twig_funs <- c(prob_funs, payoff_funs, p0_funs)
prob_payoff_funs <- c(prob_funs, payoff_funs)

fun_args <- get_function_args(twig_funs)

all_args <- unique(unlist(fun_args))

is_cycle_dep <- "cycle" %in% all_args
cycles <- 1:n_cycles

core_args <- get_core_args(twig_obj, all_args)

core_non_event_args <- get_core_non_event_args(all_args, twig_type = class(twig_obj))

event_args <- get_event_args(twig_obj, all_args)

sim_args <- get_sim_args(params, all_args)

arg_values <- get_arg_values(twig_obj, core_args, sim_args, n_sims, n_cycles)

arg_value_sizes <- get_arg_value_sizes(arg_values, core_args, sim_args, n_sims)

size_core_non_event_args <- arg_value_sizes[core_non_event_args]
total_size_core_non_event_args <- prod(size_core_non_event_args)

check_function_arguments(twig_funs, fun_args, core_args, sim_args)

core_arg_value_sizes <- arg_value_sizes[core_args]
size_core_arg_values <- prod(core_arg_value_sizes)

R_core_non_event_args <- core_non_event_args 
size_R_core_non_event_args <- arg_value_sizes[R_core_non_event_args]
total_size_R_core_non_event_args <- prod(size_R_core_non_event_args)

fun_core_df <- get_fun_core_df(twig_funs, fun_args, core_args, arg_values)

fun_sim_args <- get_fun_sim_args(twig_funs, fun_args, sim_args) 

dimnames_R0 <- arg_values[R_core_non_event_args]
dimnames_R0$payoff <- payoff_funs

IDX <- create_fun_array(prob_funs, fun_args, arg_value_sizes, core_args, size_core_arg_values)

F0 <- matrix(0, nrow = size_core_arg_values, ncol = n_prob_funs)
dim_F <- c(core_arg_value_sizes, prob_funs = n_prob_funs)
dimnames_F <- arg_values[core_args]
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

E0 <- matrix(NA, nrow = prod(core_arg_value_sizes), ncol = n_events)

initial_event <- unique(events_df$event[!events_df$event %in% events_df$transitions])
if (length(initial_event) > 1) {
  stop(sprintf("There were multiple initial events: %s. There should be a single initial event.", paste(initial_event, collapse = ", ")))
}

paths <- build_lineage(initial_event, events_df)

n_paths <- length(paths)

A0_idx <- matrix(NA, nrow = total_size_core_non_event_args, ncol = n_paths)

dimnames_E <- arg_values[core_args]

E0_df <- expand.grid(dimnames_E)
n_rows <- nrow(E0_df)
E_idx <- 1:n_rows
E0_logical <- rep(TRUE, n_rows)

all_event_args <- get_event_args(twig_obj, all_args, all_events = TRUE)

n_all_event_args <- length(all_event_args)

path_event_values <- get_path_event_values(n_paths, n_all_event_args, all_event_args, paths, events_df)

A_idx <- get_A_idx(A0_idx, n_paths, E0_logical, E0_df, event_args, path_event_values, E_idx)

state_names <- c(state_layer$names, "stay")

dest_names <- get_dest_names(paths, events_df, state_names)

unique_dest_names <- unique(dest_names)

expand_dest_states <- expand_dest_state(unique_dest_names, state_layer)

unique_non_current_dest <- expand_dest_states[expand_dest_states != "stay"]

dest_paths <- get_dest_paths(dest_names, unique_dest_names, expand_dest_states)

P0_mat <- matrix(0, nrow = total_size_core_non_event_args, ncol = n_expanded_states)
colnames(P0_mat) <- expanded_states
P0_mat

eval_funs_p0 <- evaluate_p0_functions(fun_core_df, fun_sim_args, p0_funs, params)
p0_array <- expand_initial_prob(p0_funs, fun_args, eval_funs_p0, sim_args, arg_values, core_args, state_layer, n_sims, arg_value_sizes, hash_string)

dim_P <- c(size_core_non_event_args, dest = n_expanded_states)
dimnames_P <- arg_values[core_non_event_args]
dimnames_P$dest <- expanded_states

p_stay <- get_stay_indices(state_layer, n_expanded_states, arg_values, core_non_event_args, 
            size_core_non_event_args, expanded_states, is_cycle_dep, 
            dim_P, dimnames_P, total_size_core_non_event_args)

n_payoffs <- length(payoff_funs)
payoff_fun_args <- fun_args[payoff_funs]

is_payoff_event_dep <- get_event_dep_payoffs(payoff_funs, fun_args, event_args)

event_dep_payoffs <- payoff_funs[is_payoff_event_dep]
event_indep_payoffs <- payoff_funs[!is_payoff_event_dep]
n_event_dep_payoffs <- length(event_dep_payoffs)

R_non_event_dep_idx <- create_fun_array(event_indep_payoffs, 
                                    fun_args, 
                                    arg_value_sizes, 
                                    R_core_non_event_args, 
                                    total_size_R_core_non_event_args)
R_non_event_dep_idx

IDX_R <- create_fun_array(event_dep_payoffs, fun_args, arg_value_sizes, core_args, size_core_arg_values)

IDX_path_dep <- get_IDX_path_dep(A_idx, 
                                IDX_R, 
                                n_paths, 
                                n_event_dep_payoffs, 
                                total_size_core_non_event_args, 
                                event_dep_payoffs)

R0_array <- matrix(NA, nrow = total_size_core_non_event_args, ncol = n_payoffs, 
            dimnames = list(NULL, payoff_funs))

array_discount <- get_array_discount(size_R_core_non_event_args, 
                                cycles, 
                                dimnames_R0,
                                payoff_funs, 
                                n_payoffs,
                                discount_rates,
                                n_cycles)

R_sim <- initialize_R_sim(n_decisions, 
                        n_payoffs, 
                        n_sims, 
                        decision_names,
                        payoff_funs)

path_event_options <- get_path_events(paths, events_df, n_paths, all_event_args, dest_paths)

twig_list <- list(
  A0_idx = A0_idx,
  A_idx = A_idx,
  E0 = E0,
  IDX = IDX,
  IDX_path_dep = IDX_path_dep,
  P0_mat = P0_mat,
  p0_array = p0_array,
  R0_array = R0_array,
  R_non_event_dep_idx = R_non_event_dep_idx,
  array_discount = array_discount,
  arg_value_sizes = arg_value_sizes,
  arg_values = arg_values,
  compl_id = compl_id,
  core_args = core_args,
  dest_paths = dest_paths,
  dim_P = dim_P,
  dimnames_P = dimnames_P,
  dimnames_R0 = dimnames_R0,
  event_dep_payoffs = event_dep_payoffs,
  event_indep_payoffs = event_indep_payoffs,
  event_prob_link = event_prob_link,
  F0 = F0,
  fun_args = fun_args,
  fun_core_df = fun_core_df,
  fun_sim_args = fun_sim_args,

  hash_id = hash_id,
  is_cycle_dep = is_cycle_dep,
  n_cycles = n_cycles,
  n_decisions = n_decisions,
  n_paths = n_paths,
  n_sims = n_sims,
  non_compl_id = non_compl_id,
  p_stay = p_stay,
  params = params,
  paths = paths,
  p0_funs = p0_funs,
  prob_funs = prob_funs,
  prob_payoff_funs = prob_payoff_funs,
  payoff_funs = payoff_funs,
  sim_args = sim_args,
  size_core_non_event_args = size_core_non_event_args,
  size_R_core_non_event_args = size_R_core_non_event_args,
  state_layer = state_layer,
  twig_funs = twig_funs,
  unique_dest_names = unique_dest_names,
  unique_non_current_dest = unique_non_current_dest,
  progress_bar = progress_bar, 
  verbose = verbose
)
gc()
message("Preprocessing completed. Starting simulation...")

if (parallel){
  if (is.null(ncore)){
    ncore <- parallel::detectCores() - 1
  }
  cl <- parallel::makeCluster(ncore, outfile = "")
  doParallel::registerDoParallel(cl)

  parallel::clusterExport(cl, varlist = ls(globalenv()), envir = .GlobalEnv)

  if (progress_bar) pb <- utils::txtProgressBar(0, n_sims, style = 3)
  start_time <- Sys.time()

  R_sim <- foreach::foreach(sim = seq_len(n_sims), 
                   .inorder = TRUE,
        .combine = function(...) abind::abind(..., along = 3),  
        .multicombine = TRUE, 
        .verbose = FALSE) %dopar% {
        if (progress_bar)   utils::setTxtProgressBar(pb, sim) 

    run_markov_simulation(sim, twig_list, verbose = FALSE)

  }
  total_time <- Sys.time() - start_time

  parallel::stopCluster(cl)

  message(sprintf("\nTotal time: %s\n", format(total_time, digits = 2)))
  if (progress_bar) close(pb)
  dim(R_sim) <- c(decision = n_decisions, payoff = n_payoffs, sim = n_sims)
  dimnames(R_sim) <- list(decision = decision_names, payoff = payoff_funs, sim = 1:n_sims)
} else { 
  if (verbose){ 
    results <- run_markov_simulation(1, twig_list, verbose = TRUE, offset_trace_cycle = offset_trace_cycle)
  } else {

  R_sim <- array(NA, dim = c(decision = n_decisions, payoff = n_payoffs, sim = n_sims), 
      dimnames = list(decision = decision_names, payoff = payoff_funs, sim = 1:n_sims))
  if (progress_bar) pb <- utils::txtProgressBar(0, n_sims, style = 3)
  start_time <- Sys.time()

  for (sim in seq_len(n_sims)) {
    R_sim[,,sim] <- run_markov_simulation(sim, twig_list, verbose = FALSE, offset_trace_cycle = offset_trace_cycle)
    if (progress_bar) utils::setTxtProgressBar(pb, sim) 
  }   

  total_time <- Sys.time() - start_time

  message(sprintf("\nTotal time: %s\n", format(total_time, digits = 2)))
  if (progress_bar) close(pb)
  }

}

  if (verbose){

      Event_options_temp <- data.frame(results$event_probs)
      colnames(Event_options_temp) <- event_options
      Function_Values_temp <- data.frame(results$evaluated_prob_funs_combined)
      colnames(Function_Values_temp) <- prob_funs

      dimnames_A <- arg_values[core_non_event_args]
      A_df <- expand.grid(dimnames_A)
      results$path_event_options <- path_event_options
      results$event_probs <- cbind(E0_df, Event_options_temp)
      results$evaluated_prob_funs_combined <- cbind(E0_df, Function_Values_temp)
      results$path_probs <- cbind(A_df, results$path_probs)
  } else {
    results <- list() 
    results$mean_ev <- apply(R_sim, c(1,2), mean)
    results$sim_ev <- R_sim
  }

  return(results)
}
