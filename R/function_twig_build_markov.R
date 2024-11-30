

add_markov_eqns <- function(twig_env, events_df, simplify = FALSE){
  states <- twig_env$states
  #n_states <- twig_env$n_states
  states_expanded <- twig_env$states_expanded
  n_states_expanded <- length(states_expanded)
  tunnel_states <- twig_env$tunnel_states
  #n_cycles <- twig_env$n_cycles
  # get expanded states 

  # final_outcomes <- twig_env$final_outcomes
  # n_final_outcomes <- twig_env$n_final_outcomes
  events <- twig_env$events
  n_events <- twig_env$n_events
  events_df <- get_events_df(twig_env)
  first_event <- get_first_event(events_df)
  
  payoffs <- twig_env$payoffs
  payoff_names <- names(payoffs)
  
  twig_env$path_id <- 0
  twig_env$path_df_list <- list()
  
  for (dest in states){
    # get probabililites of transitioning to each state
    twig_env <- get_prob_chain_markov(twig_env, events_df, end_state = dest)
  }
  twig_env <- get_prob_chain_markov(twig_env, events_df, end_state = "curr_state")
  
  path_df <- dplyr::bind_rows(twig_env$path_df_list) %>% 
    dplyr::inner_join(events_df, by = c("chain_id" = "id")) 
  
  # collapse chain probs and create list of all events and values
  path_df1 <- path_df %>% 
    tidyr::pivot_wider(names_from = event, values_from = values) %>% #, values_fill = "FALSE")
    dplyr::group_by(dest, path_id) %>% 
    dplyr::summarize(probs = paste0("(",probs, ")",collapse = "*"), 
                     dplyr::across(events, ~ event_value(.x)))
  
  # add payoffs ====
  for (payoff_name in payoff_names){
    path_df1[[payoff_name]] <- deparse(twig_env$payoffs[[payoff_name]])
  }
  
  # replace events with their values 
  path_df1$probs <- replace_event_with_value(x = path_df1$probs, input_df = path_df1, events = events)
  for (payoff_name in payoff_names){
    path_df1[[payoff_name]] <- replace_event_with_value(x = path_df1[[payoff_name]], input_df = path_df1, events = events)
  }
  
  # aggregate over destination states 
  # try without aggregation - aggregate probs in the twig_gen function
  # keeps curr_state 
  path_df2 <- path_df1 %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(dplyr::across(payoff_names, ~ paste0(probs, "*", .x))) %>% 
    dplyr::group_by(dest) %>% 
    dplyr::summarize(dplyr::across(c(probs, payoff_names), ~ paste0(.x, collapse="+")))
    # 
  twig_env$model_equations <- path_df2
  return(twig_env)
}


#' Check if model is cycle dependent
#' @description Checks if any of the functions used in the model take cycle as an argument
#' @param twig_env The twig environment object
#' @return Boolean indicating if model is cycle dependent
is_cycle_dep <- function(twig_env){
  return("cycle" %in% twig_env$fun_args)
}




# add_markov_info <- function(twig_env){
#   # retrieve states layer
#   states_layer <- retrieve_layer_by_type(twig_env, type = "states")
#   states <- states_layer$states# no tunnel states
#   n_cycles <- twig_env$n_cycles
  
#   tunnel_states <- twig_env$tunnel_states
#   tunnel_lengths <- twig_env$tunnel_lengths
  
#   states_expanded <- character(0)
#   for (state in states){
#     if (state %in% tunnel_states){
#       tunnel_length <- tunnel_lengths[tunnel_states == state]
#       for (j in 1:tunnel_length){
#         states_expanded <- c(states_expanded, paste0(state, "_tnl", j))
#       }
#     } else {
#       states_expanded <- c(states_expanded, state)
#     }
#   }
  
#   twig_env$states <- states
#   twig_env$n_states <- length(states)
#   twig_env$states_expanded <- states_expanded
#   twig_env$n_states_expanded <- length(states_expanded)    
#   return(twig_env)
# }

# add_markov_initial_probs <- function(twig_env){
#   # all prob are 0, just replace the ones provided with their values
#   markov_p0 <- retrieve_layer_by_type(twig_env, type = "initial_prob")
#   init_p0 <- markov_p0$probs
#   init_states <- markov_p0$states
#   tunnel_states <- twig_env$tunnel_states
#   p0 <- rep(0, twig_env$n_states_expanded)  # empty vector
#   names(p0) <- twig_env$states_expanded
  
#   for (d in twig_env$decisions){
#     twig_env$p0[[d]] <- p0
#     if (length(init_p0)>0){
#       for (i in 1:length(init_p0)){
#         state <- init_states[i]
#         if (state %in% tunnel_states){
#           state <- paste0(state, "_tnl1")
#         }
#         twig_env$p0[[d]][state] <- init_p0[i]
#       }
#     } else {
#       stop("Initial probabilities must be specified using initial_probs() function. See vignettes.")
#     }

#     twig_env$p0[[d]] <- check_prob_vector(twig_env$p0[[d]])
#   }
#   return(twig_env)
# }



# twig 

# add_tunnels <- function(twig_env){
#   tunnel_layer <- retrieve_layer_by_type(twig_env, type = "tunnels")
#   states <- tunnel_layer$states
#   lengths <- tunnel_layer$lengths
#   # if (is.null(lengths)){
#   #   lengths <- rep(twig_env$n_cycles, length(states))
#   # } else if(any(is.na(lengths))){
#   #   na_id <- is.na(lengths)
#   #   lengths[na_id] <- twig_env$n_cycles
#   # } 
#   twig_env$tunnel_states <- states
#   twig_env$tunnel_lengths <- lengths
#   names(twig_env$tunnel_lengths) <- states
#   return(twig_env)
# }
