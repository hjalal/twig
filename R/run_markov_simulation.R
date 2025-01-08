
run_markov_simulation <- function(sim, twig_list, verbose = FALSE, offset_trace_cycle = 1){

    with(twig_list, {

        eval_funs <- evaluate_functions(sim, fun_core_df, fun_sim_args, prob_reward_funs, params, arg_value_sizes, fun_args)

        F <- evaluate_fun_sim(F0, IDX, prob_funs, eval_funs)

        E <- get_E(E0, F, non_compl_id, event_prob_link, hash_id, compl_id)

        A <- get_A(A0_idx, E, A_idx, paths, n_paths)

        P_array <- calculate_transition_probs(P0_mat, A, dest_paths, unique_non_current_dest, dim_P, dimnames_P, is_cycle_dep, unique_dest_names, p_stay)

        T_array <- create_trace_array(arg_value_sizes, arg_values, p0_array[,,sim], 
            P_array, sim, is_cycle_dep, n_decisions, n_cycles)

    R_array <- calculate_rewards(sim, R0_array, event_indep_rewards, eval_funs, R_non_event_dep_idx, 
        IDX_path_dep, event_dep_rewards, A, reward_funs, dimnames_R0, size_core_non_event_args,
    n_cycles, is_cycle_dep)

    R_array_cycle <- return_R_array_cycle(R_array, reward_funs, T_array, array_discount, n_cycles, offset_trace_cycle = offset_trace_cycle)

  R_sim <- apply(R_array_cycle, c(3,4), sum)
  if (verbose){

    evaluated_funs <- get_eval_funs_list(eval_funs, fun_core_df, twig_funs) 

    sim_results <- list(sim = sim,
    Rewards_sim = R_sim, 
    Rewards_array = R_array,
    Rewards_array_cycle = R_array_cycle, 
    Trace_array = T_array, 
    TransitionProb_array = P_array, 
    Paths = A, 
    Event_options = E, 
    Function_Values = F, 
    evaluated_funs = evaluated_funs)
    return(sim_results)
  } else {
    return(R_sim)
  }

  gc()
  }) 

  } 
