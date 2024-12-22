get_reward_funs <- function(twig_obj) {
    # get payoffs
    payoffs <- retrieve_layer_by_type(twig_obj, type = "payoffs")
    filter_functions(payoffs$payoffs)
}
