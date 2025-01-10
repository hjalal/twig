
calculate_payoffs <- function(sim, R0_array, event_indep_payoffs, eval_funs, R_non_event_dep_idx,
    IDX_path_dep, event_dep_payoffs, A, payoff_funs, dimnames_R0, size_core_non_event_args,
    n_cycles, is_cycle_dep) {

    R_array <- R0_array

    for (fun in event_indep_payoffs){
        R_array[,fun] <- eval_funs[[fun]][R_non_event_dep_idx[,fun]] 
    }

    R_event_dep <- IDX_path_dep
    for (fun in event_dep_payoffs){
        R_event_dep[,,fun] <- eval_funs[[fun]][IDX_path_dep[,,fun]] 
    }
    R_event_dep

    for (fun in event_dep_payoffs){
        R_array[,fun] <- rowSums(A * R_event_dep[,,fun])
    }

    if (is_cycle_dep){

        dim(R_array) <- c(size_core_non_event_args, payoff = length(payoff_funs))
        R_array <- aperm(R_array, c(2, 1, 3, 4))
        dimnames_R0 <- dimnames_R0[c(2, 1, 3, 4)]
    } else { 

        R_array <- rep(R_array, times = n_cycles)
        dim(R_array) <- c(size_core_non_event_args, payoff = length(payoff_funs), cycle = n_cycles)
        R_array <- aperm(R_array, c(4, 1, 2, 3))
        dimnames_R0 <- c(list(cycle = 1:n_cycles), dimnames_R0)
    }

    dimnames(R_array) <- dimnames_R0

    return(R_array)
}
