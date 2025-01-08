
get_path_name <- function(path_id, dest_paths) {
  names(dest_paths)[sapply(dest_paths, function(x) path_id %in% x)]
}