expand_p0_funs <- function(p0_funs, fun_args, eval_funs, sim_args, arg_values, p0_allowable_args, n_sims){
    
    
    p0_fun_values <- eval_funs[p0_funs]

    
    p0_fun_values_expanded <- p0_fun_values

    
    for (f in seq_along(p0_funs)){
        
        sel_fun <- p0_funs[f]
        fun_arg <- fun_args[[sel_fun]]

        
        if (any(fun_arg %in% sim_args)){
            fun_arg <- fun_arg[!fun_arg %in% sim_args]
            fun_arg <- c(fun_arg, "sim")
        }

        
        if (any(fun_arg[!fun_arg %in% c("decision", "sim")])){
            stop(sel_fun, "uses arguments other than decision and simulation parameters")
        }

        
        fun_arg_missing <- p0_allowable_args[!p0_allowable_args %in% fun_arg]
        if (length(fun_arg_missing) > 0){
            if (length(fun_arg_missing) == 2){
                
                p0_fun_values_expanded[[sel_fun]] <- p0_fun_values[[sel_fun]]
                
                
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

