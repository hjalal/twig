
get_fun_sim_args <- function(twig_funs, fun_args, sim_args) {

    fun_sim_args <- list()

    for (fun in twig_funs){
      sel_fun_args <- fun_args[[fun]]

      fun_sim_args[[fun]] <- sim_args[sim_args %in% sel_fun_args]
    }

    return(fun_sim_args)
}
