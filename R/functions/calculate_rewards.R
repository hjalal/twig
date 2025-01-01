

calculate_rewards <- function(sim, R0_array, event_indep_rewards, eval_funs, R_non_event_dep_idx,
    IDX_path_dep, event_dep_rewards, A, reward_funs, dimnames_R0, size_core_non_event_args) {
    # simulation dependent rewards ----------------
    
    # for each simulation
    
    # start with non-event dependent rewards
    R_array <- R0_array
    
    for (fun in event_indep_rewards){
        R_array[,fun] <- eval_funs[[fun]][R_non_event_dep_idx[,fun]] # + sim_offset[fun]]
    }
    
    # R_event_dep <- evaluate_fun_sim(R0_event_dep, IDX_R, reward_funs, eval_funs, sim_offset)
    # R_event_dep
    
    R_event_dep <- IDX_path_dep
    for (fun in event_dep_rewards){
        R_event_dep[,,fun] <- eval_funs[[fun]][IDX_path_dep[,,fun]] # + sim_offset[fun]]
    }
    R_event_dep
    
    # multiply path dependent rewards by path probabilities and sum and store the value in the R_array
    
    for (fun in event_dep_rewards){
        R_array[,fun] <- rowSums(A * R_event_dep[,,fun])
    }
    
    # cbind(expand.grid(dimnames_R0[c(2,1,3)]), eval_funs[["cost"]])
    # resort the dimensions of the rewards array from S, C, D, R -> C, S, D, R
    dim(R_array) <- c(size_core_non_event_args, reward = length(reward_funs))
    R_array <- aperm(R_array, c(2, 1, 3, 4))
    #dim(R_array) <- c(size_R_core_non_event_args, reward = length(reward_funs))
    dimnames(R_array) <- dimnames_R0

    return(R_array)
}
