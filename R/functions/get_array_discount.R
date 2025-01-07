
get_array_discount <- function(size_R_core_non_event_args, 
                                discount_rate,
                                cycles, 
                                dimnames_R0,
                                reward_funs, 
                                n_rewards,
                                discount_rates,
                                n_cycles){
    
    dim_discount <- c(size_R_core_non_event_args, reward = n_rewards)
    dimnames_discount <- dimnames_R0
    if ("cycle" %out% names(size_R_core_non_event_args)){
        dim_discount <- c(cycle = n_cycles, dim_discount)
        dimnames_discount <- c(list(cycle = cycles), dimnames_discount)
    } else { # if cycle is already there.
        dim_discount <- dim_discount[c(2,1,3,4)]
        dimnames_discount <- dimnames_R0[c(2,1,3,4)]
    }
    array_discount <- array(NA, dim = dim_discount, dimnames = dimnames_discount)
    
    for (reward in reward_funs){
            array_discount[,,,reward] <- 1/(1+discount_rates[reward])^(cycles-1)
    }
    return(array_discount)
}

