
run_decision_simulation <- function(sim, twig_list, verbose = FALSE){

    with(twig_list, {

        eval_funs <- evaluate_functions(sim, fun_core_df, fun_sim_args, prob_payoff_funs, params, arg_value_sizes, fun_args)

        F_mat <- evaluate_fun_sim(F0, IDX, prob_funs, eval_funs)

        E <- get_E(E0, F_mat, non_compl_id, event_prob_link, hash_id, compl_id)

        A <- get_A(A0_idx, E, A_idx, paths, n_paths)
        dimnames(A) <- list(decision = decision_names, paths = NULL)

        O <- get_O(n_decisions, n_dest, A, dest_paths, decision_names, unique_dest_names)

        path_payoffs_weighted <- path_payoffs <- array(NA, dim = c(n_decisions, n_paths, n_payoffs), 
            dimnames = list(decision = decision_names, paths = NULL, payoffs = payoff_funs))

        for (fun in payoff_funs){

          path_payoffs[,,fun] <- eval_funs[[fun]][IDX_path_dep[,,fun]]

          path_payoffs_weighted[,,fun] <- A * path_payoffs[,,fun]
        }

        R_sim <- apply(path_payoffs_weighted, c(1,3), sum)

   if (verbose){
    evaluated_funs <- get_eval_funs_list(eval_funs, fun_core_df, twig_funs)

    sim_results <- list(sim = sim,
                        
                        evaluated_funs = evaluated_funs,
                        evaluated_prob_funs_combined = F_mat,
                        event_probs = E, 
                        outcome_probs = O, 

                        path_event_options = NULL,
                        path_probs = A, 
                        path_payoffs = path_payoffs, 
                        path_ev = path_payoffs_weighted, 
                        
                        sim_ev = R_sim,
                        mean_ev = R_sim
        )
    return(sim_results)
  } else {
    return(R_sim)
  }

  gc()
  }) 

  } 
