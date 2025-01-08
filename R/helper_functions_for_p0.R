


is_numeric_string <- function(x) {
  grepl("^-?\\d*(\\.\\d+)?$", x)
}


is_global_variable <- function(var_name) {
  exists(var_name, envir = .GlobalEnv)
}


is_scalar <- function(x) {
  is.atomic(x) && length(x) == 1
}
