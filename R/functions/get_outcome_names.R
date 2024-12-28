
get_outcome_names <- function(events_df){
    outcome_names <- events_df$goto
    outcome_names <- outcome_names[!outcome_names %in% events_df$event]
    return(outcome_names)
}
