
# nolint start

# fun_in_twig <- function(twig_obj) {
#     # Retrieve initial probabilities functions
#     states <- retrieve_layer_by_type(twig_obj, type = "states")
#     init_probs <- states$init_probs

#     # Retrieve events and extract all probability values
#     events <- retrieve_layer_by_type(twig_obj, type = "event")
#     all_probs <- unlist(lapply(events, function(x) x$probabilities))
#     unique_probs <- unique(all_probs[!all_probs %in% c('"#"', "complement")])

#     # Retrieve payoffs
#     payoffs <- retrieve_layer_by_type(twig_obj, type = "payoffs")
#     unique_payoffs <- payoffs$payoffs

#     # Combine all unique functions
#     unique_funs <- unique(c(init_probs, unique_probs, unique_payoffs))

#     # Filter to keep only elements that are functions
#     unique_funs <- Filter(function(fun_name) exists(fun_name, mode = "function"), unique_funs)

#     return(unique_funs)
# }
