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



return_complement <- function(compl_id, fun_outputs){
  event_i <- events_dt[id == compl_id]$event
  p_names <- events_dt[event == event_i & probs != "#"]$probs
  
  if (length(p_names) == 1){ # just compute complementary
    result_dt <- copy(fun_outputs[[p_names]])
    
    # Dynamically identify columns starting with 'x_sim'
    x_cols <- grep("^x_sim", names(result_dt), value = TRUE)
    
    # Apply the transformation (1 - x) to each 'x_sim' column
    result_dt[, (x_cols) := lapply(.SD, function(x) 1 - x), .SDcols = x_cols]
    
    #result_dt[,x := 1 - x]
  } else { # join, sum and compute complementary
    dt_list <- fun_outputs[p_names] 
    # Call the function with your list of data.tables
    x_cols <- grep("^x_sim", names(dt_list[[1]]), value = TRUE)
    
    result_dt <- smart_sum(dt_list, complement = TRUE)
    
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
smart_sum <- function(sel_fun_outputs, complement = FALSE){
  # harmonize and join the objects
  # harmonize and join the objects
  x_cols <- grep("^x_sim", names(sel_fun_outputs[[1]]), value = TRUE)
  res <- harmonize_and_join(sel_fun_outputs, x_cols, remove_zeros = !complement)
  joined_datasets <- res$joined_datasets
  unique_cols = res$unique_cols
  # stack them
  if (complement){
    aggregated_data <- joined_datasets[, lapply(.SD, function(x) 1 - sum(x, na.rm = TRUE)), .SDcols = x_cols, by = unique_cols]
  } else {
    aggregated_data <- joined_datasets[, lapply(.SD, sum, na.rm = TRUE), .SDcols = x_cols, by = unique_cols]
  }
  
  return(aggregated_data)
  # sum 
}

smart_prod <- function(sel_fun_outputs){
  # harmonize and join the objects
  x_cols <- grep("^x_sim", names(sel_fun_outputs[[1]]), value = TRUE)
  res <- harmonize_and_join(sel_fun_outputs, x_cols, remove_zeros = TRUE)
  joined_datasets <- res$joined_datasets
  unique_cols = res$unique_cols
  n_sel_fun_outputs <- length(sel_fun_outputs)
  # stack them
  # conditional product if # is == number of datasets
  # Aggregate to compute product of 'x' only when .N == n_sel_fun_outputs
  #aggregated_data <- joined_datasets[, .(x = if (.N == n_sel_fun_outputs) 
  #  prod(x) else NA_real_), by = unique_cols]
  aggregated_data <- joined_datasets[, lapply(.SD, function(col) if (.N == n_sel_fun_outputs) prod(col) else NA_real_),
                                     .SDcols = x_cols, by = unique_cols]
  aggregated_data <- joined_datasets[, c(
    # Calculate the product for each x_sim column
    lapply(.SD, function(col) prod(col, na.rm = TRUE)),
    # Calculate the count of rows in each group
    count_col = .N
  ), .SDcols = x_cols, 
  by = unique_cols
  ]
  
  # Remove rows where all x_cols are NA
  aggregated_data <- aggregated_data[N == n_sel_fun_outputs]
  aggregated_data[, N := NULL]
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

get_unique_values <- function(sel_fun_outputs, x_cols){
  unique_values <- list()
  for (i in 1:length(sel_fun_outputs)) {
    dataset <- sel_fun_outputs[[i]]
    
    # Exclude the 'x' column
    relevant_columns <- setdiff(names(dataset), x_cols)
    
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
harmonize_and_join <- function(sel_fun_outputs, x_cols, remove_zeros = FALSE){
  # Initialize lists to hold unique values
  unique_values <- get_unique_values(sel_fun_outputs, x_cols)
  
  # Initialize a list to store the joined datasets
  joined_dataset_list <- list()
  unique_cols <- names(unique_values)
  n_sel_fun_outputs <- length(sel_fun_outputs)
  # Iterate through each dataset to perform the cross join
  for (i in 1:n_sel_fun_outputs) {
    # only keep datasets with non-zeros
    #dataset <- sel_fun_outputs[[i]][x>0,]
    # Filter rows where all values in x_cols are NOT equal to 0
    if (remove_zeros){ # complement doesn't need 0s removed.
      dt <- sel_fun_outputs[[i]]
      dataset <- dt[rowSums(dt[, ..x_cols] != 0, na.rm = TRUE) > 0]
    } else {
      dataset <- sel_fun_outputs[[i]]
    }
    
    # Determine the columns for the join
    x <- names(dataset)
    y <- c(names(unique_values), x_cols)
    relevant_columns <- setdiff(y, x)
    shared_columns <- y[y %in% x & !(y %in% x_cols)]
    if (length(relevant_columns) > 0){
      # Create a list to hold data tables for the cross join
      #input_list <- unique_values[relevant_columns]
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


# get segment probabilities
get_seg_probs <- function(events_dt, fun_outputs){
  dt_prob_list <- list() #as.list(rep(NA,n))
  # Event/Scenario level =======
  # iterate through each id in events_dt and assign htem to the dt_prob_list
  for (i in 1:nrow(events_dt)){
    # if probs == hash
    prob <- events_dt[i,probs]
    id <- events_dt[i]$id
    if(prob=="#"){ # compute and return the complement
      dt_prob_list_temp  <- return_complement(compl_id = id, fun_outputs = fun_outputs)
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

filter_prob_list <- function(dt_prob_list, sel_row){
  filtered_prob_list <- list()
  for (i in 1:length(dt_prob_list)){
    dt_prob <- dt_prob_list[[i]]
    event_cols <- intersect(names(dt_prob), names(sel_row))
    if (length(event_cols)>0){
      # Filter rows based on matching elements in sel_row, ignoring missing columns
      # Filter rows based on matching elements in set_row
      filtered_dt <- dt_prob[
        , .SD[Reduce(`&`, lapply(event_cols, function(col) get(col) == sel_row[[col]]))]
      ]
      # Remove columns in event_cols
      filtered_dt[, (event_cols) := NULL]
    } else {
      filtered_dt <- dt_prob
    }
    # remove event columns
    filtered_prob_list[[i]] <- filtered_dt
  }
  return(filtered_prob_list)
}

get_path_probs <- function(path_dt, dt_prob_list, dt_curr_states){
  
  dt_pathprob_list <- list()
  for (i in 1:nrow(path_dt)){
    sel_row <- as.list(path_dt[i])
    chain_id2 <- unlist(sel_row$seg_ids)
    dest <- sel_row$dest
    path_i <- sel_row$path_id
    
    
    if (length(chain_id2)==1){
      # remove zeros from teh single segment
      dt <- filter_prob_list(dt_prob_list[[chain_id2]], sel_row)
      x_cols <- grep("^x_sim", names(dt), value = TRUE)
      temp_dt <- dt[rowSums(dt[, ..x_cols] != 0, na.rm = TRUE) > 0]
    } else { # do a product removing zeros from all segments before multiplication
      temp_dt <- smart_prod(filter_prob_list(dt_prob_list[chain_id2], sel_row))
    }
    
    if (dest == "curr_state"){
      # Split 'expanded_state' into 'state' and 'cycle_in_state' based on "_tnl"
      temp_dt <- merge(temp_dt, dt_curr_states, by = "state")
    } else {
      
      if (dest %in% dt_curr_states$state){
        dest_state <- dest
      } else {
        dest_state <- paste0(dest, "_tnl1")
      }
      temp_dt[,state2:=dest_state]
    }
    
    dt_pathprob_list[[path_i]] <- temp_dt
  }
  
  return(dt_pathprob_list)
}


get_path_dt <- function(events_dt, dt_curr_states){
  # compute product of prob by joining first then multiplying.
  origin_events <- unique(events_dt$event)
  dest <- unique(events_dt$goto) 
  dest_states <- dest[!dest %in% origin_events]
  
  dt_path_list <- list()
  # for each path join & multiply ids 
  #path_ids <- unique(path_dt$path_id)
  path_i <- 0
  #dest <- dest_states[2]
  
  for (dest in dest_states){
    chain_ids <- get_event_chain_ids(events_dt, goto_id = dest)
    
    for (chain_id in chain_ids){
      path_i <- path_i + 1
      chain_id2 <- chain_id[chain_id > 0]
      filtered_events <- events_dt[chain_id2]
      # Create a named list with `get_sick` and `die` keys, setting each to "yes"
      other_events <- setdiff( all_events, filtered_events$event)
      other_event_values <- rep(default_event_scenario, length(other_events))
      event_list <- setNames(as.list(c(filtered_events$values, other_event_values)), 
                             c(filtered_events$event, other_events))
      # Convert each event_list item to a factor with levels from twig_dims
      event_factors <- lapply(names(event_list), function(name) {
        factor(event_list[[name]], levels = levels(twig_dims[[name]]))
      })
      # Set names for the resulting vector
      names(event_factors) <- names(event_list)
      

      # Convert to data.table and assign to a variable
      event_factors_dt <- as.data.table(event_factors)
      
      # Add columns using := syntax
      dt_path_list[[path_i]] <- event_factors_dt[, `:=`(path_id = path_i, dest = dest, seg_ids = list(chain_id2))]
      
    }
  }
  path_dt <- rbindlist(dt_path_list)
  return(path_dt)
}


get_dt_p0 <- function(states_layers, n_sims, x_cols){
  p0 <- states_layers$dt_p0
  # Repeat 'x' for the number of simulations
  # Create new columns 'x_sim1', 'x_sim2', ..., 'x_simN' by repeating 'x'
  p0[, (x_cols) := lapply(1:n_sims, function(i) rep(x, length.out = .N))]
  p0[, x := NULL]
  return(p0)
}

# Trace
get_trace <- function(trans_probs, p0, n_cycles, x_cols){
  list_dt_trace <- list()
  # Initialize the progress bar
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "[:bar] :current/:total (:percent); ETA::eta",
    total = n_cycles, 
    clear = FALSE, 
    width = 100
  )
  
  # Track start time
  start_time <- Sys.time()
  for (j in 1:n_cycles){
    P <- trans_probs[cycle==j,] 
    if (j == 1) p <- p0
    p <- smart_prod(list(P, p))
    # Remove the original 'state' column
    p[, state := NULL]
    # Rename 'state2' to 'state'
    setnames(p, "state2", "state")
    #p <- p[, .(x = sum(x, na.rm = TRUE)), by = setdiff(names(p), "x")]
    # Modify the code to sum each element of x_cols
    p <- p[, lapply(.SD, sum, na.rm = TRUE), 
           .SDcols = x_cols, 
           by = setdiff(names(p), x_cols)]
    
    list_dt_trace[[j]] <- copy(p)
    p[, cycle:=cycle+1]
    
    pb$tick()
    
  }
  Trace <- rbindlist(list_dt_trace)
  return(Trace)
}


# get decision, state, event(s), cycle and sim dimensions
get_twig_dims <- function(mytwig, events_dt, n_cycles, n_sims){
  decisions <- retrieve_layer_by_type(mytwig, type = "decisions")$decisions
  states <- retrieve_layer_by_type(mytwig, type = "states")$expanded_states
  list_dims <- list(
    decision = factor(decisions, levels = decisions),
    state = factor(states, levels = states),
    cycle = 1:n_cycles, 
    sim = 1:n_sims)
  events <- unique(events_dt$event)
  for (event in events){
    event_levels <- events_df$values[events_df$event == event]
    list_dims[[event]] <- factor(event_levels, levels = event_levels)
  }
  return(list_dims)
}
