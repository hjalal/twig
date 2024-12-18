create_trace_array <- function(arg_value_sizes, arg_values, p0_array, P_array, sim, is_cycle_dep, n_decisions, n_cycles) {
    # Create the Trace array

    # these should always be the same dimensions
    dim_T <- c("cycle", "state", "decision")

    # add + 1 to the size of cycle 
    dim_sizes_T <- arg_value_sizes[dim_T]
    dim_sizes_T["cycle"] <- dim_sizes_T["cycle"] + 1

    # get the dimnames for the trace
    dimnames_T <- arg_values[dim_T]

    # add cycle 0 to the trace 
    dimnames_T$cycle <- c(0, dimnames_T$cycle) 

    # intialize the trace
    T_array <- array(NA, dim = dim_sizes_T, dimnames = dimnames_T)

    # fill in the first cycle
    T_array[1, , ] <- p0_array[,,sim]

    # if cycle dependent, 
    if (is_cycle_dep) {
        for (decision in 1:n_decisions) {
            for (cycle in 1:n_cycles) {
                T_array[cycle+1, , ] <-  T_array[cycle, , decision] %*% P_array[ , , cycle, decision]
            }
        }
    } else { # not cycle dependent
        for (decision in 1:n_decisions) {
            for (cycle in 1:n_cycles) {
                T_array[cycle+1, , ] <-  T_array[cycle, , decision] %*% P_array[ , , decision]
            }
        }
    }

    return(T_array)
}
