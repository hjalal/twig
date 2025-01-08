get_dest_paths <- function(dest_names, unique_dest_names, expand_dest_states){

    
    
    dest_paths <- list()
    for (i in seq_along(expand_dest_states) ){
        y <- expand_dest_states[i]
        dest <- unique_dest_names[i]
        dest_paths[[y]] <- which(dest_names == dest)
    }
    return(dest_paths)
}
