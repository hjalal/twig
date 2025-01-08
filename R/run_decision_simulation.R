  
  
run_decision_simulation <- function(sim, twig_list, verbose = FALSE){
    
    
    with(twig_list, {
      
        

        
        
        
        
        eval_funs <- evaluate_functions(sim, fun_core_df, fun_sim_args, prob_reward_funs, params, arg_value_sizes, fun_args)
    
        
        
        

        F_mat <- evaluate_fun_sim(F0, IDX, prob_funs, eval_funs)

        

        
        
        
        
        
        


        
        

        E <- get_E(E0, F_mat, non_compl_id, event_prob_link, hash_id, compl_id)

        
        

        
        
        A <- get_A(A0_idx, E, A_idx, paths, n_paths)
        dimnames(A) <- list(decision = decision_names, paths = NULL)
        
        
        O <- get_O(n_decisions, n_dest, A, dest_paths, decision_names, unique_dest_names)
        
        path_rewards_weighted <- path_rewards <- array(NA, dim = c(n_decisions, n_paths, n_rewards), 
            dimnames = list(decision = decision_names, paths = NULL, rewards = reward_funs))
        
        for (fun in reward_funs){
          
          path_rewards[,,fun] <- eval_funs[[fun]][IDX_path_dep[,,fun]]
          
          path_rewards_weighted[,,fun] <- A * path_rewards[,,fun]
        }

        
        R_sim <- apply(path_rewards_weighted, c(1,3), sum)

   if (verbose){
    evaluated_funs <- get_eval_funs_list(eval_funs, fun_core_df, twig_funs)

    sim_results <- list(sim = sim,
    Rewards_sim = R_sim, 
    Path_rewards = path_rewards, 
    Path_rewards_weighted = path_rewards_weighted, 
    Paths = A, 
    Outcomes = O, 
    Event_options = E, 
    Function_Values = F_mat,
    evaluated_funs = evaluated_funs)
    return(sim_results)
  } else {
    return(R_sim)
  }

  gc()
  }) 

  } 
