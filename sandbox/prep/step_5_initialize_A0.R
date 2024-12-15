# path array initialization
# and the dimension of the F array and its dimnames

# get path df

# Build paths from the niitial event
# intitial event must be a single event
initial_event <- unique(events_df$event[!events_df$event %in% events_df$goto])
if (length(initial_event) > 1) {
  stop("There were multiple initial events:", initial_event, "There should be a single initial event.")
}
# generate paths pointing from each event to a final state (could be duplicated)
paths <- build_lineage(initial_event, events_df)

# Print the lineages
n_paths <- length(paths)
size_core_non_event_arguments <- arg_value_sizes[core_non_event_args]

# a place holder for the indices for the paths matrix 
A0_idx <- matrix(NA, nrow = prod(size_core_non_event_arguments), ncol = n_paths)

#list of indices of the event array
# dim_E <- c(arg_value_sizes[core_args], event_id = n_events)
 dimnames_E <- arg_values[core_args]
#dimnames_E$event_id <- 1:n_events

# the idea is to create a logical array that will be used to filter the E0 array
# one event at a time.
E0_df <- expand.grid(dimnames_E)
n_rows <- nrow(E0_df)
E_idx <- 1:n_rows
E0_logical <- rep(TRUE, n_rows)

# allow for all possible events (no/NA = "none")
n_event_args <- length(event_args)

# a matrix where rows are paths, and columns are event arguments, 
# and the values are the event values
path_event_values <- matrix("none", nrow = n_paths, ncol = n_event_args, 
    dimnames = list(path_id = 1:n_paths, event_id = event_args))

# fill the path_event_values matrix using the events data frame
for (path_id in 1:n_paths){
    event_ids_on_path <- paths[[path_id]]
    for (event_id in event_ids_on_path){
        event_data <- events_df[events_df$event_id == event_id, ]
        path_event_values[path_id, event_data$event] <- event_data$values
    }
}


# iterate through all the event values and find the corresponding indices in the E0 array
# each path will have a unique set of indices that will retrieve the event values
# along the path
A_idx <- A0_idx

for (path_id in 1:n_paths){
    
    # select event_ids and the corresponding event values along the paths
    E_logical <- E0_logical
    for (event in event_args){
        E_logical <- E_logical & (E0_df[,event] == path_event_values[path_id, event])
    }
    
    # only select the indices that match the event values along the path
    A_idx[,path_id] <- E_idx[E_logical]
    # multiply the E array by the event values along the paths
}
A_idx

