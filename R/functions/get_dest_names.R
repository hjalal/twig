# functions: 
# Transition prob matrix preparation functions





# get_stay_indices <- function(state_layer, dim_P, dimnames_P, size_core_non_event_args, Y, is_cycle_dep){
#     # deal with current state
#     size_p_stay <- total_size_core_non_event_args*length(Y)
#     origin_state_name <- state_layer$tunneled_states
#     stay_dest_names <- get_stay_dest_names(state_layer)

#         # get indices of staying in the curent state
#     P_df <- expand.grid(dimnames_P)
#     p_stay <- logical(size_p_stay) 

#     # ieterate through each row of the stay_df, and iteratively turn on the p_stay_idx
#     # if the origin state is the same as the destination state for that row in the expanded
#     # dimensions of the P_df
#     for (i in 1:length(stay_dest_names)){
#         p_stay <- p_stay | (P_df$dest == stay_dest_names[i] & 
#                             P_df$state == origin_state_name[i])
#     }

#     dim(p_stay) <- dim_P
#     dimnames(p_stay) <- dimnames_P

#     if (is_cycle_dep){
#         p_stay <- aperm(p_stay, c(1,4, 2, 3))
#     } else {
#         p_stay <- aperm(p_stay, c(1,3, 2))
#     }
#     return(p_stay)
# }

get_dest_names <- function(paths, events_df, state_names){
dest_names <- rep(NA, length(paths))

# crosswalk between state and destination ids / names

for (path_id in seq_along(paths)){
    # get event_ids for each path_id
    event_ids <- paths[[path_id]]
    # get the transitions state for each event_id
    transitions <- events_df$transitions[events_df$event_id %in% event_ids]
    # only keep those that are in state_names and current_state
    dest_names[path_id] <- transitions[transitions %in% state_names]
}
return(dest_names)
}

# # get destination paths list for each destination
# get_dest_paths <- function(expand_dest_states, paths, unique_dest_names, dest_names){
#     dest_paths <- list()
#     for (i in seq_along(expand_dest_states) ){
#         y <- expand_dest_states[i]
#         dest <- unique_dest_names[i]
#         dest_paths[[y]] <- which(dest_names == dest)
#     }
#     return(dest_paths)
# }

