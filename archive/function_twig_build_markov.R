add_markov_eqns <- function(twig_obj){
  
  # states <- twig_obj$states
  # #n_states <- twig_obj$n_states
  # states_expanded <- twig_obj$states_expanded
  # n_states_expanded <- length(states_expanded)
  # tunnel_states <- twig_obj$tunnel_states
  # #n_cycles <- twig_obj$n_cycles
  # # get expanded states 

  # # final_outcomes <- twig_obj$final_outcomes
  # # n_final_outcomes <- twig_obj$n_final_outcomes
  # events <- twig_obj$events
  # n_events <- twig_obj$n_events
  # events_df <- get_events_df(twig_obj)

  # first_event <- get_first_event(events_df)
  
  # payoffs <- twig_obj$payoffs
  # payoff_names <- names(payoffs)
  


  path_id <- 0
  
  twig_obj$path_df_list <- list()
  # iterate through all destination states, including stay
  dest_states <- c(twig_obj$fun_arg_values$state, "stay")
  for (dest in dest_states){
    print(dest)
    # get probabililites of transitioning to each state
    path_id <- path_id + 1
    twig_obj$path_df_list[[path_id]] <- get_prob_chain(twig_obj, twig_obj$events_df, end_state = dest)
  }
  #twig_obj <- get_prob_chain_markov(twig_obj, events_df, end_state = "stay")
  #
  # for (dest in states){
  #   # get probabililites of transitioning to each state
  #   twig_obj <- get_prob_chain_markov(twig_obj, events_df, end_state = dest)
  # }
  # twig_obj <- get_prob_chain_markov(twig_obj, events_df, end_state = "stay")
  


  path_df <- do.call(rbind, twig_obj$path_df_list)
  path_df <- merge(path_df, twig_obj$events_df, by.x = "chain_id", by.y = "id")

  # path_df1 <- path_df %>% 
  #   tidyr::pivot_wider(names_from = event, values_from = values) %>% 
  #   dplyr::group_by(dest, path_id) %>% 
  #   dplyr::summarize(probs = paste0("(",probs, ")",collapse = "*"), 
  #                    dplyr::across(twig_obj$events, ~ event_value(.x))) %>% 
  #   dplyr::mutate(dplyr::across(twig_obj$events, ~ ifelse(.x=="FALSE", "none", .x)))

  # collapse chain probs and create list of all events and values


# Reshape the data
  path_df1 <- reshape(path_df[, !names(path_df) %in% c("chain_id", "transitions", "type", "probs")], 
  idvar = c("dest", "path_id"),
  timevar = "event",
  direction = "wide",
  v.names = "values"
)

# Concatenate "probs" in the correct order for each "path_id"
path_df1$probs <- sapply(
  unique(path_df$path_id),
  function(id) {
    paste(path_df$probs[path_df$path_id == id][order(path_df$event[path_df$path_id == id])], collapse = "*")
  }
)

# Replace <NA> in columns starting with "values" with "none"
cols_to_modify <- grep("^values", names(path_df1), value = TRUE)
path_df1[cols_to_modify] <- lapply(path_df1[cols_to_modify], function(x) ifelse(is.na(x), "none", x))
# Sort the reshaped data by path_id
path_df1 <- path_df1[order(path_df1$path_id), ]


# replace probs with their array syntax, and passing in the event values from this path table.
for (fun in twig_obj$twig_funs){
  path_df1$probs <- gsub(paste0("\\b", fun, "\\b"), twig_obj$str_fun_array_list[[fun]], path_df1$probs)
 }
path_df1



# what about the payoffs?
# add only payoffs to path_dt1 taht are event dependent 
# which payoffs are event dependent?
# Get payoff names that have events in their expanded arguments
# Get elements of fun_args_expanded that contain at least one event
fun_args_expanded_with_events <- twig_obj$fun_args_expanded[sapply(twig_obj$fun_args_expanded, function(args) {
  any(twig_obj$events %in% args)
})]
fun_names_with_events <- names(fun_args_expanded_with_events) 
payoff_names_with_events <- fun_names_with_events[fun_names_with_events %in% twig_obj$payoff_names]
payoff_names_wo_events <- twig_obj$payoff_names[!twig_obj$payoff_names %in% payoff_names_with_events]

  
  # add payoffs ====
  for (payoff_name in payoff_names_with_events){
    path_df1[[payoff_name]] <- twig_obj$str_fun_array_list[[payoff_name]]
  }
  
# for columns probs and payoffs, replace events with their values in path_df1
for (col in c("probs", payoff_names_with_events)) {
  path_df1[[col]] <- replace_event_with_value(col=col, input_df = path_df1, twig_obj = twig_obj)
}

# get dependencies for probs and payoffs_with_events for each path_id
probs_dependencies <- get_dependencies(path_df1$probs, path_df1$path_id)

payoff_w_event_add_dependencies <- list()
for (payoff_name in payoff_names_with_events){
  payoff_w_event_add_dependencies[[payoff_name]] <- get_dependencies(path_df1[[payoff_name]], path_df1$path_id, probs_dependencies)
}


 

  twig_obj$prob_and_event_arrays <- path_df1
  twig_obj$payoff_names_with_events <- payoff_names_with_events
  twig_obj$payoff_names_wo_events <- payoff_names_wo_events
  twig_obj$probs_dependencies <- probs_dependencies
  twig_obj$payoff_w_event_add_dependencies <- payoff_w_event_add_dependencies
  #return(twig_obj)
}

get_dependencies <- function(col, id, probs_dependencies = NULL){
  core_args <- c("decision", "state", "expanded_state", "cycle", "sim")
  
  # Initialize empty list to store dependencies for each path_id
  dependencies <- list()
  
  # For each observation in col
  for(i in seq_along(col)) {
    # Find which core_args exist in this observation
    exists <- sapply(core_args, function(arg) {
      grepl(paste0("\\b", arg, "\\b"), col[i])
    })
    
    # Store the names of core_args that exist for this path_id
    temp_dep <- core_args[exists] 
    if (is.null(probs_dependencies)){ # for probs, add all dependencies across all probs
      dependencies[[id[i]]] <- temp_dep
    } else { # for payoffs, only keep dependencies that are not in probs_dependencies
      dependencies[[id[i]]] <- temp_dep[!temp_dep %in% probs_dependencies[[i]]]
    }
  }
  
  return(dependencies)
}





# add_markov_info <- function(twig_obj){
#   # retrieve states layer
#   states_layer <- retrieve_layer_by_type(twig_obj, type = "states")
#   states <- states_layer$states# no tunnel states
#   n_cycles <- twig_obj$n_cycles
  
#   tunnel_states <- twig_obj$tunnel_states
#   max_cycles <- twig_obj$max_cycles
  
#   states_expanded <- character(0)
#   for (state in states){
#     if (state %in% tunnel_states){
#       tunnel_length <- max_cycles[tunnel_states == state]
#       for (j in 1:tunnel_length){
#         states_expanded <- c(states_expanded, paste0(state, "_tnl", j))
#       }
#     } else {
#       states_expanded <- c(states_expanded, state)
#     }
#   }
  
#   twig_obj$states <- states
#   twig_obj$n_states <- length(states)
#   twig_obj$states_expanded <- states_expanded
#   twig_obj$n_states_expanded <- length(states_expanded)    
#   return(twig_obj)
# }

# add_markov_initial_probs <- function(twig_obj){
#   # all prob are 0, just replace the ones provided with their values
#   markov_p0 <- retrieve_layer_by_type(twig_obj, type = "initial_prob")
#   init_p0 <- markov_p0$probs
#   init_states <- markov_p0$states
#   tunnel_states <- twig_obj$tunnel_states
#   p0 <- rep(0, twig_obj$n_states_expanded)  # empty vector
#   names(p0) <- twig_obj$states_expanded
  
#   for (d in twig_obj$decisions){
#     twig_obj$p0[[d]] <- p0
#     if (length(init_p0)>0){
#       for (i in 1:length(init_p0)){
#         state <- init_states[i]
#         if (state %in% tunnel_states){
#           state <- paste0(state, "_tnl1")
#         }
#         twig_obj$p0[[d]][state] <- init_p0[i]
#       }
#     } else {
#       stop("Initial probs must be specified using initial_probs() function. See vignettes.")
#     }

#     twig_obj$p0[[d]] <- check_prob_vector(twig_obj$p0[[d]])
#   }
#   return(twig_obj)
# }



# twig 

# add_tunnels <- function(twig_obj){
#   tunnel_layer <- retrieve_layer_by_type(twig_obj, type = "tunnels")
#   states <- tunnel_layer$states
#   lengths <- tunnel_layer$lengths
#   # if (is.null(lengths)){
#   #   lengths <- rep(twig_obj$n_cycles, length(states))
#   # } else if(any(is.na(lengths))){
#   #   na_id <- is.na(lengths)
#   #   lengths[na_id] <- twig_obj$n_cycles
#   # } 
#   twig_obj$tunnel_states <- states
#   twig_obj$max_cycles <- lengths
#   names(twig_obj$max_cycles) <- states
#   return(twig_obj)
# }
