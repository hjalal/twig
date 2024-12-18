# helper functions for p0

# is a string a valid numeric string?
is_numeric_string <- function(x) {
  grepl("^-?\\d*(\\.\\d+)?$", x)
}

# is a string a valid global variable name?
is_global_variable <- function(var_name) {
  exists(var_name, envir = .GlobalEnv)
}

# Function to check if a variable is a scalar
is_scalar <- function(x) {
  is.atomic(x) && length(x) == 1
}
