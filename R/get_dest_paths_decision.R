get_dest_paths_decision <- function(dest_names, unique_dest_names){

    dest_paths <- list()
    for (i in seq_along(unique_dest_names) ){
        y <- unique_dest_names[i]
        dest <- unique_dest_names[i]
        dest_paths[[y]] <- which(dest_names == dest)
    }
    return(dest_paths)
}
