get_A_idx <- function(A0_idx, n_paths, E0_logical, E0_df, event_args, path_event_values, E_idx){

    A_idx <- A0_idx

    for (path_id in 1:n_paths){

        E_logical <- E0_logical
        for (event in event_args){
            E_logical <- E_logical & (E0_df[,event] == path_event_values[path_id, event])
        }

        A_idx[,path_id] <- E_idx[E_logical]

    }
    A_idx
}