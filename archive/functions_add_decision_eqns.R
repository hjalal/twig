# main decision model file to add all equations to object 
# adds all equations to object
add_decision_eqns <- function(twig_obj, simplify = FALSE){
  final_outcomes <- twig_obj$final_outcomes
  n_final_outcomes <- twig_obj$n_final_outcomes
  events <- twig_obj$events
  n_events <- twig_obj$n_events
  events_df <- get_events_df(twig_obj)
  first_event <- get_first_event(events_df)

  decisions <- data.frame(decision = twig_obj$decisions)
  
  payoffs <- twig_obj$payoffs
  payoff_names <- names(payoffs)
  
  
  twig_obj$path_id <- 0
  twig_obj$path_df_list <- list()
  
  for (final_outcome in final_outcomes){
    twig_obj <- get_prob_chain(twig_obj, events_df, end_state = final_outcome)
    #vec_p_stay[final_outcome] <- get_prob_chain(twig_obj, events_df, end_final_outcome = "curr_final_outcome")
  }
  path_df <- dplyr::bind_rows(twig_obj$path_df_list) %>% 
    dplyr::inner_join(events_df, by = c("chain_id" = "id"))
  

  path_df1 <- path_df %>% tidyr::crossing(decisions) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(probs = gsub("\\bdecision\\b", paste0("'",decision,"'"), probs))
  # deal with prev_event() 
  path_df1_1 <- path_df1 %>% 
    tidyr::pivot_wider(names_from = event, values_from = values)
  
  
  # collapse chain probs and create list of all events and values
  path_df2 <- path_df1_1 %>% 
    dplyr::ungroup() %>%
    dplyr::group_by(decision, final_outcome, path_id) %>% 
    dplyr::summarize(probs = paste0("(",probs, ")",collapse = "*"), 
              dplyr::across(events, ~ event_value(.x)))
  
  # replace event names in probs with event=value 
  path_df2$probs <- replace_event_with_value(x = path_df2$probs, input_df = path_df2, events = events)
  
  # add payoff formula
  for (payoff_name in payoff_names){
    path_df2[[payoff_name]] <- paste0(deparse(payoffs[[payoff_name]]), collapse = "")
    for (i in 1:nrow(path_df2)){
      path_df2[[payoff_name]][i] <- gsub("\\bdecision\\b", paste0("'",path_df2$decision[i],"'"), path_df2[[payoff_name]][i])
      path_df2[[payoff_name]][i] <- gsub("\\bfinal_outcome\\b", paste0("'",path_df2$final_outcome[i],"'"), path_df2[[payoff_name]][i])
    }      
    path_df2[[payoff_name]] <- replace_event_with_value(x = path_df2[[payoff_name]], input_df = path_df2, events = events)
  }
  twig_obj$final_outcome_formulae <- path_df2
  
  
  # multiply final_outcomes by probs and aggregate by decision
  path_df3 <- path_df2 %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(dplyr::across(payoff_names, ~ paste0(probs, "*", .x)))
  
  if (simplify){
    path_df3 <- path_df3 %>%
      dplyr::rowwise() %>% 
      dplyr::mutate(prob_value = eval(parse(text = probs)))  %>%  # filter out probs that are evaluate to 0.
      dplyr::filter(prob_value != 0) %>% 
      dplyr::mutate(probs = ifelse(prob_value==1, "1", probs)) %>%
      dplyr::select(-prob_value)
  }
  
  # aggregate by decision
  path_df4 <- path_df3 %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(decision) %>% 
    dplyr::summarize(dplyr::across(payoff_names, ~ paste0(.x, collapse = "+"))) #\n\t")))
  
  twig_obj$summary_formulae <- path_df4
  return(twig_obj)
}


event_value <- function(x, default_na_value = "none"){
  if(all(is.na(x))){
    default_na_value
    #"NA" #returns NA if the event is missing
  } else {
    unique(x[!is.na(x)])
  }
}


replace_event_with_value <- function(col, input_df, twig_obj, use_event_idx = TRUE, use_cpp = FALSE){
  n <- nrow(input_df)
  x <- input_df[[col]]
  if (use_cpp) idx_offset <- 1 else idx_offset <- 0

  for (event in twig_obj$events){
    for (i in 1:n){
      #x[i] <- gsub(paste0("\\b", event, "\\b"), paste0(event, "=\"", input_df[i,event]), "\"", x[i])
      if (use_event_idx){
        replacement_value <- which(twig_obj$fun_arg_values[[event]]==input_df[i,paste0("values.", event)]) - idx_offset
      } else {
        replacement_value <- paste0(event, "=", input_df[i,paste0("values.", event)])
      }
      x[i] <- gsub(paste0("\\b", event, "\\b"), replacement_value, x[i])
    }
  }
  return(x)
}

# is_numeric_or_logical <- function(input_string){
#   if ((input_string %in% c("FALSE", "F", "TRUE", "T")) | grepl("^-?\\d*\\.?\\d+$", input_string)){
#     input_string
#   } else {
#     paste0("'", input_string, "'")
#   }
# }
