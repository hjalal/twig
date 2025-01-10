get_IDX_path_dep <- function(   A_idx, 
                                IDX_R, 
                                n_paths, 
                                n_event_dep_payoffs, 
                                total_size_core_non_event_args, 
                                event_dep_payoffs){

    IDX_path_dep <- array(NA, dim = c(total_size_core_non_event_args, n_paths, n_event_dep_payoffs), 
                        dimnames = list(NULL, path_id = NULL, event_dep_payoff_id = event_dep_payoffs))

    for (fun in event_dep_payoffs){
        IDX_path_dep[,,fun] <- IDX_R[A_idx, fun]
    }
    return(IDX_path_dep)
}