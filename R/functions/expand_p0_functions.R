expand_p0_funs <- function(p0_funs, fun_args, eval_funs, sim_args, arg_values, p0_allowable_args, n_sims){
    
    # get the function values
    p0_fun_values <- eval_funs[p0_funs]

    # intialize the expanded function values
    p0_fun_values_expanded <- p0_fun_values

    # loop through the functions and expand them
    for (f in seq_along(p0_funs)){
        # get the function's dimensions
        sel_fun <- p0_funs[f]
        fun_arg <- fun_args[[sel_fun]]

        # if there are sim_args = parameters, remove them and add sim at the end 
        if (any(fun_arg %in% sim_args)){
            fun_arg <- fun_arg[!fun_arg %in% sim_args]
            fun_arg <- c(fun_arg, "sim")
        }

        # if there are arguments other than "decision" and "sim" in the arguments, stop
        if (any(fun_arg[!fun_arg %in% c("decision", "sim")])){
            stop(sel_fun, "uses arguments other than decision and simulation parameters")
        }

        # get what is missing from the allowable possible arguments
        fun_arg_missing <- p0_allowable_args[!p0_allowable_args %in% fun_arg]
        if (length(fun_arg_missing) > 0){
            if (length(fun_arg_missing) == 2){
                # both missing, it must be a scalar with no arguments
                p0_fun_values_expanded[[sel_fun]] <- p0_fun_values[[sel_fun]]
                # expand the function results by the missing values
                # pay attention to the position of the missing values
            } else if (fun_arg_missing == "decision"){
                n_decisions <- length(arg_values$decision)
                p0_fun_values_expanded[[sel_fun]] <- rep(p0_fun_values[[sel_fun]], each = n_decisions)
            } else if (fun_arg_missing == "sim"){
                p0_fun_values_expanded[[sel_fun]] <- rep(p0_fun_values[[sel_fun]], times = n_sims)
            } else {
                stop(sel_fun, ": Something went wrong. Check the function arguments")
            }
        }

    }
    return(p0_fun_values_expanded)
}

