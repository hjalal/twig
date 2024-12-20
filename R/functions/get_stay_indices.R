get_stay_indices <- function(state_layer, n_expanded_states, arg_values, core_non_event_args, 
            size_core_non_event_arguments, expanded_states, is_cycle_dep, dim_P, dimnames_P,
            total_size_core_non_event_arguments){

        # deal with current state
    size_p_stay <- total_size_core_non_event_arguments * n_expanded_states
    origin_state_name <- state_layer$tunneled_states
    stay_dest_names <- get_stay_dest_names(state_layer)

        # get indices of staying in the curent state
    P_df <- expand.grid(dimnames_P)
    p_stay <- logical(size_p_stay) 

    # ieterate through each row of the stay_df, and iteratively turn on the p_stay_idx
    # if the origin state is the same as the destination state for that row in the expanded
    # dimensions of the P_df
    for (i in 1:length(stay_dest_names)){
        p_stay <- p_stay | (P_df$dest == stay_dest_names[i] & 
                            P_df$state == origin_state_name[i])
    }

    dim(p_stay) <- dim_P
    dimnames(p_stay) <- dimnames_P

    if (is_cycle_dep){
        p_stay <- aperm(p_stay, c(1,4, 2, 3))
    } else {
        p_stay <- aperm(p_stay, c(1,3, 2))
    }
return(p_stay)
}

