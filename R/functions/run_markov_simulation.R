  
  
run_markov_simulation <- function(sim, twig_list, verbose = FALSE){
    # Assuming twig_read_only_env is a list or environment containing the necessary variables
    with(twig_list, {
        # sim_offset <- compute_sim_offset(sim, prob_reward_funs, sim_offset0)

        # 3. F(sim) = same as IDX. Harmonize probs sim -------------------------------------------------
        # parallellize
        # add an option to store and output intermediate matrices with a warning about matrix sizes
        
        eval_funs <- evaluate_functions(sim, fun_core_df, fun_sim_args, prob_reward_funs, params)
    
        # browser()


        F <- evaluate_fun_sim(F0, IDX, prob_funs, eval_funs)

        # source("R/steps/step_3_harmonize_probs.R")

        # # 4. E: Create a single event array  -------------------------------------------------
        # # if any is cycle dependent, dims = j=D, S, +/-C, j=event_id
        # # for complement probs # = 1 - sum other probs 
        # E[D,S,C,E(s),j=event_id]. so similar to the F array, 
        # but with the event_id instead of the prob_funs, which 
        # involves computing the complement of the probabilities.


        # adjustments of the event array for each sim
        #dim(F) <- c(prod(core_arg_value_sizes), n_prob_funs)

        E <- get_E(E0, F, non_compl_id, event_prob_link, hash_id, compl_id)

        # source("R/steps/step_4_event_array.R")
        # print(E)

        # # 5. A[,,,k]: Create a single path array ---------------------------------------------
        # # product of all E[,,,j] that are in on each path k=path_id
        A <- get_A(A0_idx, E, A_idx, paths, n_paths)

        # source("R/steps/step_5_path_array.R")
        # print(A)

        # # 6. P: Create transition probs ----------------------------------------
        # # sum of all A[,,,k] that lead to the same destination Y
        P_array <- calculate_transition_probs(P0_mat, A, dest_paths, unique_non_current_dest, dim_P, dimnames_P, is_cycle_dep, unique_dest_names, p_stay)

        # source("R/steps/step_6_transition_probs.R")
        # print(P_array)

        # # 7. P0: Expand initial prob ----------------------------------------
        # # can be numeric, global variable or a function of D, sim.
        # source("R/steps/step_7_expand_initial_prob.R")
        # print(p0_array)
        #browser()
        # # 8. T: Create trace ---------------------------------------------------------
        # # iteratively multiply state distribution by P
        # source("R/steps/step_8_create_trace.R")
        # print(T_array)
        T_array <- create_trace_array(arg_value_sizes, arg_values, p0_array[,,sim], 
            P_array, sim, is_cycle_dep, n_decisions, n_cycles)
        # # 9. R0: create a single array for all event-dep rewards by path k --------------
        # # rewards are by evnets, but have to be made dependent on path k
        # # 10. multiply event-dep rewards and event arrays---------------------------
        # # for each reward doing a redim so it is [DSC * K] and then doing an element 
        # # wise multiplication, and then colSum, and then redimming to D, S, +/-C.


        # # 11. R: create a single array of all rewards ------------------------------
        # # iteratre through each reward, and fill in a single array D,S,C,r=reward_id
        # source("R/steps/step_9_reward_event_dep_array.R")
        # print(R_array)

        # 10. RC: multiply rewards and trace R * T * discount -------------------------------------------

        # 11. RS: create summary payoffs ----------------------------------------------
        R_array_cycle <- calculate_rewards(sim, R0_array, event_indep_rewards, eval_funs, R_non_event_dep_idx, 
            #sim_offset[sim,], 
            IDX_path_dep, event_dep_rewards, A, size_R_core_non_event_args, reward_funs, dimnames_R0, 
            T_array, n_cycles, array_discount, verbose)

  R_sim <- apply(R_array_cycle, c(3,4), sum)
  if (verbose){
    #browser()

    sim_results <- list(sim = sim,
    Rewards_sim = R_sim, 
    Rewards_array_cycle = R_array_cycle, 
    Trace_array = T_array, 
    TransitionProb_array = P_array, 
    Paths = A, 
    Event_Scenarios = E, 
    Function_Values = F)
    return(sim_results)
  } else {
    return(R_sim)
  }

  gc()
  }) # end with statement

  } # end function
