# mapping part that would be pre-determined for all sims
events_df <- get_events_df(twig_obj)
events_df
n_events <- nrow(events_df)
event_probs <- events_df$probs
event_probs
event_ids <- events_df$event_id
event_ids

prob_funs
event_prob_link <- match(event_probs, prob_funs)
non_compl_id <- which(!is.na(event_prob_link))
hash_id <- which(is.na(event_prob_link))
compl_id <- get_compl_event_ids(events_df)
compl_id


E0 <- matrix(NA, nrow = prod(core_arg_value_sizes), ncol = n_events)
