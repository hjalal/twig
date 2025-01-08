create_trace_array <- function(arg_value_sizes, arg_values, p0_array_sim, P_array, sim, is_cycle_dep, n_decisions, n_cycles) {

    dim_T <- c("cycle", "state", "decision")
    if ("cycle" %out% names(arg_value_sizes)) {
        arg_value_sizes <- c(cycle = n_cycles, arg_value_sizes)
        arg_values <- c(list(cycle = 1:n_cycles), arg_values)
    }

    dim_sizes_T <- arg_value_sizes[dim_T]
    dim_sizes_T["cycle"] <- n_cycles + 1 

    dimnames_T <- arg_values[dim_T]

    dimnames_T$cycle <- c(0, dimnames_T$cycle) 

    T_array <- array(NA, dim = dim_sizes_T, dimnames = dimnames_T)

    T_array[1, , ] <- p0_array_sim

    if (is_cycle_dep) {
        for (decision in 1:n_decisions) {
            for (cycle in 1:n_cycles) {
                T_array[cycle+1, , decision] <-  T_array[cycle, , decision] %*% P_array[ , , cycle, decision]
            }
        }
    } else { 
        for (decision in 1:n_decisions) {
            for (cycle in 1:n_cycles) {
                T_array[cycle+1, , decision] <-  T_array[cycle, , decision] %*% P_array[ , , decision]
            }
        }
    }
    return(T_array)
}
