

calculate_rewards <- function(sim, R0_array, event_indep_rewards, eval_funs, R_non_event_dep_idx, sim_offset, IDX_path_dep, event_dep_rewards, A, size_R_core_non_event_args, reward_funs, dimnames_R0, T_array, n_cycles, array_discount, R_sim) {
    # simulation dependent rewards ----------------
    
    # for each simulation
    
    # start with non-event dependent rewards
    R_array <- R0_array
    
    for (fun in event_indep_rewards){
        R_array[,fun] <- eval_funs[[fun]][R_non_event_dep_idx[,fun] + sim_offset[[fun]]]
    }
    
    # R_event_dep <- evaluate_fun_sim(R0_event_dep, IDX_R, reward_funs, eval_funs, sim_offset)
    # R_event_dep
    
    R_event_dep <- IDX_path_dep
    for (fun in event_dep_rewards){
        R_event_dep[,,fun] <- eval_funs[[fun]][IDX_path_dep[,,fun] + sim_offset[[fun]]]
    }
    R_event_dep
    
    # multiply path dependent rewards by path probabilities and sum and store the value in the R_array
    
    for (fun in event_dep_rewards){
        R_array[,fun] <- rowSums(A * R_event_dep[,,fun])
    }
    
    dim(R_array) <- c(size_R_core_non_event_args, length(reward_funs))
    dimnames(R_array) <- dimnames_R0
    
    #discount_array <- 
    R_array_cycle <- R_array
    for (reward in reward_funs){
        R_array_cycle[,,,reward] <- R_array[,,,reward] * T_array[1:n_cycles,,] * array_discount[,,,reward]
    }
    
    R_sim[,,sim] <- apply(R_array_cycle, c(3,4), sum)
    
    R_summary <- apply(R_sim, c(1,2), mean)
    
    return(R_summary)
}