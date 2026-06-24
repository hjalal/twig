get_payoff_funs <- function(twig_obj, envir = parent.frame()) {

    payoffs <- retrieve_layer_by_type(twig_obj, type = "payoffs")
    filter_functions(payoffs$payoffs, envir = envir)
}
