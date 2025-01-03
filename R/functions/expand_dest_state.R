expand_dest_state <- function(dest_names, state_layer){
    expanded_dest_states <- dest_names

    for (i in seq_along(expanded_dest_states)){
        s <- expanded_dest_states[i]
        tunnel_length <- state_layer$max_cycle_in_states[state_layer$names == s]

        # only if the tunnel length is greater than 1 (doesn't applies to the current state)
        if (length(tunnel_length)>0){
            # only if the tunnel length is greater than 1
            if (tunnel_length > 1){
                # rename state by adding _tnl1 as the destination state
                expanded_dest_states[i] <- paste0(s, "_tnl1")
            }
        }
    }

    # get the expanded state for a given state
    return(expanded_dest_states)
}
