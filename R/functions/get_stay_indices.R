get_stay_indices <- function(state_layer, n_expanded_states, arg_values, core_non_event_args, 
            size_core_non_event_arguments, expanded_states, is_cycle_dep) {
    
    dim_P <- c(size_core_non_event_arguments, dest = n_expanded_states)
    dimnames_P <- arg_values[core_non_event_args]
    dimnames_P$dest <- expanded_states
    p_stay <- get_stay_indices(state_layer, dim_P, dimnames_P, size_core_non_event_arguments, expanded_states, is_cycle_dep)

return(p_stay)
}