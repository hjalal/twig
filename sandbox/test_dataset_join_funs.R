# Function to dynamically join events_df with corresponding data.table
join_events_dt <- function(events_dt) {
  result_list <- list()  # To store results of each join
  
  for (i in seq_len(nrow(events_dt))) {
    # Get the corresponding data.table based on the prob column
    dt_name <- dt_fun_list[[events_dt$prob[i]]]
    #df_to_join <- get(dt_name)
    
    # Take the current row of events_df
    current_row <- events_dt[i]
    #if (current_row$probs != "#"){
    # Add an identifier (like row number) to facilitate joining
    # Perform a Cartesian join by adding the current_row to each row of df_to_join
    joined_dt <- df_to_join[, c(current_row, .SD), .SDcols = names(df_to_join)]
    #} else {
    #  joined_dt <- 
    #}
    # Append the result to the list
    result_list[[i]] <- joined_dt
  }
  
  # Combine all the joined results into a single data.table
  result_dt <- rbindlist(result_list, fill = TRUE)
  return(result_dt)
}

# Function to find the common columns except 'p'
find_common_cols <- function(dt_list) {
  # Get common columns other than 'p' for joining
  common_cols <- Reduce(intersect, lapply(dt_list, function(dt) setdiff(names(dt), "p")))
  return(common_cols)
}

# Full join function that excludes 'p' from join
full_join_and_sum <- function(dt_list, complement = TRUE) {
  # Perform full join on all columns except 'p'
  result_dt <- Reduce(function(x, y) {
    join_cols <- intersect(setdiff(names(x), "x"), setdiff(names(y), "x")) # exclude 'p' from join columns
    merge(x, y, by = join_cols, all = TRUE) #, allow.cartesian=TRUE)
  }, dt_list)
  
  # Sum the 'p' columns after joining
  p_cols <- grep("^x", names(result_dt), value = TRUE) # find all 'p' columns
  if(complement){
    result_dt[, p_sum := 1-rowSums(.SD, na.rm = TRUE), .SDcols = p_cols]
  } else {
    result_dt[, p_sum := rowSums(.SD, na.rm = TRUE), .SDcols = p_cols]
  }
  # Remove the 'p' columns
  result_dt[, (p_cols) := NULL]
  setnames(result_dt, "p_sum", "x")
  return(result_dt)
}


return_complement <- function(compl_id, fun_outputs){
  event_i <- events_dt[id == compl_id]$event
  p_names <- events_dt[event == event_i & probs != "#"]$probs
  if (length(p_names) == 1){ # just compute complementary
    result_dt <- copy(fun_outputs[[p_names]])
    result_dt[,x := 1 - x]
  } else { # join, sum and compute complementary
    dt_list <- fun_outputs[p_names] 
    # Call the function with your list of data.tables
    result_dt <- full_join_and_sum(dt_list)
  }
  return(result_dt)
}

# get path_ids ========
full_join_and_mult <- function(dt_list) {
  # Perform full join on all columns except 'p'
  result_dt <- Reduce(function(x, y) {
    join_cols <- intersect(setdiff(names(x), "x"), setdiff(names(y), "x")) # exclude 'p' from join columns
    merge(x, y, by = join_cols, all = TRUE, allow.cartesian=TRUE)
  }, dt_list)
  
  # Sum the 'p' columns after joining
  p_cols <- grep("^x", names(result_dt), value = TRUE) # find all 'p' columns
  result_dt[, p_product := apply(.SD, 1, prod, na.rm = TRUE), .SDcols = p_cols]
  # Remove the 'p' columns
  result_dt[, (p_cols) := NULL]
  result_dt <- result_dt[p_product>0]
  setnames(result_dt, "p_product", "x")
  return(result_dt)
}

# takes is a list of data tables and column values and unifies them
expand_data_tables <- function(dt_list, col_values){
  
}

# idea to expand the arrays so they all have the same dimenions and 
# adding x
smart_sum <- function(sel_fun_outputs){
  # harmonize and join the objects
  # harmonize and join the objects
  res <- harmonize_and_join(sel_fun_outputs)
  joined_datasets <- res$joined_datasets
  unique_cols = res$unique_cols
  # stack them
  aggregated_data <- joined_datasets[, .(x = sum(x)), by = unique_cols]
  return(aggregated_data)
  # sum 
}

smart_prod <- function(sel_fun_outputs){
  # harmonize and join the objects
  res <- harmonize_and_join(sel_fun_outputs)
  joined_datasets <- res$joined_datasets
  unique_cols = res$unique_cols
  n_sel_fun_outputs <- length(sel_fun_outputs)
  # stack them
  # conditional product if # is == number of datasets
  # Aggregate to compute product of 'x' only when .N == n_sel_fun_outputs
  aggregated_data <- joined_datasets[, .(x = if (.N == n_sel_fun_outputs) 
    prod(x) else NA_real_), by = unique_cols]
  aggregated_data <- aggregated_data[!is.na(x)]
  return(aggregated_data)
  # sum 
}


# Function to extract unique components
get_unique_components <- function(sel_arg_values, component) {
  # Extract values for the specified component from all sub-lists
  values <- unlist(lapply(sel_arg_values, function(x) x[[component]]))
  # Get unique values
  unique_values <- unique(values)
  return(unique_values)
}

get_unique_values <- function(sel_fun_outputs){
  unique_values <- list()
  for (i in 1:length(sel_fun_outputs)) {
    dataset <- sel_fun_outputs[[i]]

    # Exclude the 'x' column
    relevant_columns <- setdiff(names(dataset), "x")

    for (col in relevant_columns) {
      unique_vals <- unique(dataset[[col]])

      if (!is.null(unique_values[[col]])) {
        unique_values[[col]] <- unique(c(unique_values[[col]], unique_vals))
      } else {
        unique_values[[col]] <- unique_vals
      }
    }

  }
  return(unique_values)
}

# harmonize and join twig_obj
harmonize_and_join <- function(sel_fun_outputs){
  # Initialize lists to hold unique values
  unique_values <- get_unique_values(sel_fun_outputs)
  
  # Initialize a list to store the joined datasets
  joined_dataset_list <- list()
  unique_cols <- names(unique_values)
  n_sel_fun_outputs <- length(sel_fun_outputs)
  # Iterate through each dataset to perform the cross join
  for (i in 1:n_sel_fun_outputs) {
    # only keep datasets with non-zeros
    dataset <- sel_fun_outputs[[i]][x>0,]
    
    # Determine the columns for the join
    x <- names(dataset)
    y <- c(names(unique_values), "x")
    relevant_columns <- setdiff(y, x)
    shared_columns <- y[y %in% x & y != "x"]
    if (length(relevant_columns) > 0){
      # Create a list to hold data tables for the cross join
      input_list <- unique_values[relevant_columns]
      
      join_data <- do.call(CJ, unique_values)
      join_data <- merge(join_data, dataset, by = shared_columns) #,allow.cartesian = TRUE)
    } else {
      join_data <- dataset
    }
    
    # Store the result in the list
    joined_dataset_list[[i]] <- join_data
  }
  joined_datasets <- rbindlist(joined_dataset_list, use.names = TRUE)
  return(list(joined_datasets=joined_datasets, unique_cols = unique_cols))
}



# get_unique_values <- function(sel_fun_outputs){
#   unique_values <- list()
#   
#   # Loop through each dataset in sel_fun_outputs
#   for (dataset_name in names(sel_fun_outputs)) {
#     dataset <- sel_fun_outputs[[dataset_name]]
#     
#     # Exclude the 'x' column
#     relevant_columns <- setdiff(names(dataset), "x")
#     
#     # Loop through each relevant column to collect unique values
#     for (col in relevant_columns) {
#       unique_vals <- unique(dataset[[col]])
#       
#       # Store unique values in the list, appending the dataset name
#       if (!is.null(unique_values[[col]])) {
#         unique_values[[col]] <- unique(c(unique_values[[col]], unique_vals))
#       } else {
#         unique_values[[col]] <- unique_vals
#       }
#     }
#   }
#   #print(unique_values)
#   return(unique_values)
#   
# }


# get segment probabilities
get_seg_probs <- function(evnets_dt, fun_outputs){
  dt_prob_list <- list() #as.list(rep(NA,n))
  # Event/Scenario level =======
  # iterate through each id in events_dt and assign htem to the dt_prob_list
  for (i in 1:nrow(events_dt)){
    # if probs == hash
    prob <- events_dt[i,probs]
    id <- events_dt[i]$id
    if(prob=="#"){ # compute and return the complement
      dt_prob_list_temp  <- return_complement(compl_id = id, fun_outputs = fun_outputs)
      # if it is a complement add origin state if not already there
      # if(events_dt[i]$outcomes == "curr_state"){
      #   dt_prob_list_temp[,dest := state]
      # }
      dt_prob_list[[id]] <- dt_prob_list_temp
    } else { # just get the dataset
      dt_prob_list[[id]] <- fun_outputs[[prob]]
    }
  }
  return(dt_prob_list)
}


get_prob_chain <- function(twig_obj, events_df, end_state){
  
  # get the row id sequences for for each event chain
  # convert to strings with * between each element and + between each chain
  for (event_chain in event_chains){
    twig_obj$path_id <- twig_obj$path_id + 1
    twig_obj$path_df_list[[twig_obj$path_id]] <- data.frame(
      path_id = rep(twig_obj$path_id, length(event_chain)),
      chain_id = event_chain,
      final_outcome = end_state)
  }
  #prob_chain <- build_prob_chain(events_df, event_chains)
  #if (prob_chain == "()"){prob_chain <- "0"}
  return(twig_obj)
}


get_path_probs <- function(events_dt, dt_prob_list, dt_curr_states){
  # compute product of prob by joining first then multiplying.
  origin_events <- unique(events_dt$event)
  dest <- unique(events_dt$goto) 
  dest_states <- dest[!dest %in% origin_events]
  
  dt_pathprob_list <- list()
  # for each path join & multiply ids 
  #path_ids <- unique(path_dt$path_id)
  path_i <- 0
  #dest <- dest_states[2]
  
  for (dest in dest_states){
    chain_ids <- get_event_chain_ids(events_dt, goto_id = dest)
    #chain_id <- chain_ids[[1]]
    for (chain_id in chain_ids){
      path_i <- path_i + 1
      chain_id2 <- chain_id[chain_id > 0]
      #id <- events_df[]
      #event_probs <- events_df$probs[events_df$id %in% chain_id2]
      if (length(chain_id2)==1){
        temp_dt <- dt_prob_list[[chain_id2]][x>0]
      } else {
        #dt_pathprob_list[[path_i]] <- full_join_and_mult(dt_prob_list[chain_id2])
        temp_dt <- smart_prod(dt_prob_list[chain_id2])
      }
      if (dest == "curr_state"){
        # Split 'expanded_state' into 'state' and 'cycle_in_state' based on "_tnl"
        temp_dt <- merge(temp_dt, dt_curr_states, by = "state")
        #temp_dt [,Y:=state]
      } else {
        if (dest %in% dt_curr_states$state){
          dest_state <- dest
        } else {
            dest_state <- paste0(dest, "_tnl1")
        }
        temp_dt[,Y:=dest_state]
      }
      dt_pathprob_list[[path_i]] <- temp_dt
    }
  }
  return(dt_pathprob_list)
}
