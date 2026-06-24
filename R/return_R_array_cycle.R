
return_R_array_cycle <- function(R_array, payoff_funs, T_array, array_discount, n_cycles, offset_trace_cycle = 1, event_dep_payoffs = character(0)){
    R_array_cycle <- R_array
    for (payoff in payoff_funs){
        # Event-dependent (transition) payoffs are incurred by the cohort that
        # is in the origin state at the START of the cycle (their path
        # probabilities are indexed by origin state), so they must be weighted
        # by the start-of-cycle trace row (offset 0). Occupancy payoffs use the
        # configured offset (end-of-cycle by default).
        offset <- if (payoff %in% event_dep_payoffs) 0 else offset_trace_cycle
        R_array_cycle[,,,payoff] <- R_array[,,,payoff] *
                                    T_array[(1:n_cycles) + offset,,] *
                                    array_discount[,,,payoff]
    }

    return(R_array_cycle)
}
