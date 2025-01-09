
get_function_args <- function(fun_name) {

    arguments <- lapply(fun_name, function(fname) {
      fun <- get(fname)
      names(formals(fun))

    })
    names(arguments) <- fun_name

  return(arguments)
}