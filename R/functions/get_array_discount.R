
get_array_discount <- function(size_R_core_non_event_args, 
                                discount_rate,
                                cycles, 
                                dimnames_R0,
                                reward_funs, 
                                n_rewards,
                                discount_rates){
    

    dim_discount <- c(size_R_core_non_event_args, reward = n_rewards)
    dimnames_discount <- dimnames_R0
    array_discount <- array(NA, dim = dim_discount, dimnames = dimnames_discount)
    for (reward in reward_funs){
        array_discount[,,,reward] <- 1/(1+discount_rates[reward])^(cycles-1)
    }
    return(array_discount)
}

