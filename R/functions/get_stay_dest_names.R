get_stay_dest_names <- function(state_layer){
    stay_dest_names <- state_layer$repeated_states
    cycles_in_state <- state_layer$cycles_in_state
    stay_cycles_in_state <- cycles_in_state + 1
    tunnel_lengths <- state_layer$tunnel_lengths
    expanded_tunnel_lengths <- tunnel_lengths[match(state_layer$repeated_states, state_layer$names)]
    stay_cycles_in_state <- ifelse(stay_cycles_in_state > expanded_tunnel_lengths, expanded_tunnel_lengths, stay_cycles_in_state)
    
    for (i in 1:length(stay_cycles_in_state)){
        if (!is.na(stay_cycles_in_state[i])){
            stay_dest_names[i] <- paste0(stay_dest_names[i], "_tnl", stay_cycles_in_state[i])
    }
    }
    return(stay_dest_names)
}