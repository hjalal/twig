
return_R_array_cycle <- function(R_array, payoff_funs, T_array, array_discount, n_cycles, offset_trace_cycle = 1){
    R_array_cycle <- R_array
    for (payoff in payoff_funs){
        R_array_cycle[,,,payoff] <- R_array[,,,payoff] * 
                                    T_array[(1:n_cycles) + offset_trace_cycle,,] * 
                                    array_discount[,,,payoff]
    }

    return(R_array_cycle)
}
