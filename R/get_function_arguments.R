
get_function_args <- function(fun_name, envir = parent.frame()) {

    arguments <- lapply(fun_name, function(fname) {
      fun <- get(fname, envir = envir, mode = "function")
      args <- names(formals(fun))
      # A function without parameters returns NULL from names(formals());
      # normalize it to character(0) so it is treated as a constant that
      # broadcasts across every core-argument combination.
      if (is.null(args)) character(0) else args
    })
    names(arguments) <- fun_name

  return(arguments)
}