get_event_dep_payoffs <- function(payoff_funs, fun_args, event_args){
    is_payoff_event_dep <- rep(FALSE, length(payoff_funs))
    for (i in seq_along(payoff_funs)){
        payoff <- payoff_funs[i]
        payoff_args <- fun_args[[payoff]]
        is_payoff_event_dep[i] <- any(event_args %in% payoff_args)
    }
    return(is_payoff_event_dep)
}