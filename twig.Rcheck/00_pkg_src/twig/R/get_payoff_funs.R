get_payoff_funs <- function(twig_obj) {

    payoffs <- retrieve_layer_by_type(twig_obj, type = "payoffs")
    filter_functions(payoffs$payoffs)
}
