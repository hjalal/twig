
get_array_discount <- function(size_R_core_non_event_args, 
                                cycles, 
                                dimnames_R0,
                                payoff_funs, 
                                n_payoffs,
                                discount_rates,
                                n_cycles){

    dim_discount <- c(size_R_core_non_event_args, payoff = n_payoffs)
    dimnames_discount <- dimnames_R0
    if ("cycle" %out% names(size_R_core_non_event_args)){
        dim_discount <- c(cycle = n_cycles, dim_discount)
        dimnames_discount <- c(list(cycle = cycles), dimnames_discount)
    } else { 
        dim_discount <- dim_discount[c(2,1,3,4)]
        dimnames_discount <- dimnames_R0[c(2,1,3,4)]
    }
    array_discount <- array(NA, dim = dim_discount, dimnames = dimnames_discount)

    for (payoff in payoff_funs){
            array_discount[,,,payoff] <- 1/(1+discount_rates[payoff])^(cycles-1)
    }
    return(array_discount)
}

