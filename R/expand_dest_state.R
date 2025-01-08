expand_dest_state <- function(dest_names, state_layer){
    expanded_dest_states <- dest_names

    for (i in seq_along(expanded_dest_states)){
        s <- expanded_dest_states[i]
        tunnel_length <- state_layer$max_cycles[state_layer$names == s]

        if (length(tunnel_length)>0){

            if (tunnel_length > 1){

                expanded_dest_states[i] <- paste0(s, "_tnl1")
            }
        }
    }

    return(expanded_dest_states)
}
