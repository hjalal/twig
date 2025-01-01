
return_R_array_cycle <- function(R_array, reward_funs, T_array, array_discount, n_cycles, offset_trace_cycle = 1){

    R_array_cycle <- R_array
    for (reward in reward_funs){
        R_array_cycle[,,,reward] <- R_array[,,,reward] * 
                                    T_array[(1:n_cycles) + offset_trace_cycle,,] * 
                                    array_discount[,,,reward]
    }
    # browser()
    return(R_array_cycle)
}