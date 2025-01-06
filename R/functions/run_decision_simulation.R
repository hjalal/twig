  
  
run_decision_simulation <- function(sim, twig_list, verbose = FALSE){
    
    # Assuming twig_read_only_env is a list or environment containing the necessary variables
    with(twig_list, {
      # 
        # sim_offset <- compute_sim_offset(sim, prob_reward_funs, sim_offset0)

        # 3. F(sim) = same as IDX. Harmonize probs sim -------------------------------------------------
        # parallellize
        # add an option to store and output intermediate matrices with a warning about matrix sizes
        
        eval_funs <- evaluate_functions(sim, fun_core_df, fun_sim_args, prob_reward_funs, params, arg_value_sizes, fun_args)
    

        F_mat <- evaluate_fun_sim(F0, IDX, prob_funs, eval_funs)

        # source("R/steps/step_3_harmonize_probs.R")

        # # 4. E: Create a single event array  -------------------------------------------------
        # # if any is cycle dependent, dims = j=D, S, +/-C, j=event_id
        # # for complement probs # = 1 - sum other probs 
        # E[D,S,C,E(s),j=event_id]. so similar to the F array, 
        # but with the event_id instead of the prob_funs, which 
        # involves computing the complement of the probs.


        # adjustments of the event array for each sim
        #dim(F) <- c(prod(core_arg_value_sizes), n_prob_funs)

        E <- get_E(E0, F_mat, non_compl_id, event_prob_link, hash_id, compl_id)

        # source("R/steps/step_4_event_array.R")
        # print(E)

        # # 5. A[,,,k]: Create a single path array ---------------------------------------------
        # # product of all E[,,,j] that are in on each path k=path_id
        A <- get_A(A0_idx, E, A_idx, paths, n_paths)
        dimnames(A) <- list(decision = decision_names, paths = NULL)
        # 
        # get outcomes by decision, this is similar to the trace in Markov
        O <- get_O(n_decisions, n_dest, A, dest_paths, decision_names, unique_dest_names)
        # harmonize all rewards so they are a function of D, paths and rewards
        path_rewards_weighted <- path_rewards <- array(NA, dim = c(n_decisions, n_paths, n_rewards), 
            dimnames = list(decision = decision_names, paths = NULL, rewards = reward_funs))
        
        for (fun in reward_funs){
          # get the rewards for each path unweighted yet by the path probs
          path_rewards[,,fun] <- eval_funs[[fun]][IDX_path_dep[,,fun]]
          # multiply rewards by the paths 
          path_rewards_weighted[,,fun] <- A * path_rewards[,,fun]
        }

        # summarize the results by decision
        R_sim <- apply(path_rewards_weighted, c(1,3), sum)

   if (verbose){

    sim_results <- list(sim = sim,
    Rewards_sim = R_sim, 
    Path_rewards = path_rewards, 
    Path_rewards_weighted = path_rewards_weighted, 
    Paths = A, 
    Outcomes = O, 
    Event_options = E, 
    Function_Values = F_mat)
    return(sim_results)
  } else {
    return(R_sim)
  }

  gc()
  }) # end with statement

  } # end function
