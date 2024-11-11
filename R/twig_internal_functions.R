# Helper functions
.onAttach <- function(libname, pkgname) {
  packageStartupMessage('To get started with twig, please check the Articles section on https://hjalal.github.io/twig/')
}

return_complement <- function(compl_id, fun_outputs, x_cols, events_dt){
  event_i <- events_dt[id == compl_id]$event
  p_names <- events_dt[event == event_i & probs != "#"]$probs
  
  if (length(p_names) == 1){ # just compute complementary
    result_dt <- copy(fun_outputs[[p_names]])
    
    # Dynamically identify columns starting with 'sim_'
    #x_cols <- grep("^sim_", names(result_dt), value = TRUE)
    
    # Apply the transformation (1 - x) to each 'sim_' column
    result_dt[, (x_cols) := lapply(.SD, function(x) 1 - x), .SDcols = x_cols]
    
    #result_dt[,x := 1 - x]
  } else { # join, sum and compute complementary
    dt_list <- fun_outputs[p_names] 
    # Call the function with your list of data.tables
    #x_cols <- grep("^sim_", names(dt_list[[1]]), value = TRUE)
    
    result_dt <- smart_sum(dt_list, x_cols, complement = TRUE)
    
  }
  return(result_dt)
}


# idea to expand the arrays so they all have the same dimenions and 
# adding x
smart_sum <- function(sel_fun_outputs, x_cols, complement = FALSE){
  # harmonize and join the objects
  # harmonize and join the objects
  # x_cols <- grep("^sim_", names(sel_fun_outputs[[1]]), value = TRUE)
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

smart_prod <- function(sel_fun_outputs, x_cols){
  # harmonize and join the objects
  # x_cols <- grep("^sim_", names(sel_fun_outputs[[1]]), value = TRUE)
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
    # Calculate the product for each sim_ column
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
get_seg_probs <- function(events_dt, fun_outputs, x_cols){
  dt_prob_list <- list() #as.list(rep(NA,n))
  # Event/Scenario level =======
  # iterate through each id in events_dt and assign htem to the dt_prob_list
  for (i in 1:nrow(events_dt)){
    # if probs == hash
    prob <- events_dt[i,probs]
    id <- events_dt[i]$id
    if(prob=="#"){ # compute and return the complement
      dt_prob_list_temp  <- return_complement(compl_id = id, fun_outputs = fun_outputs, x_cols, events_dt)
      dt_prob_list[[id]] <- dt_prob_list_temp
    } else { # just get the dataset
      dt_prob_list[[id]] <- fun_outputs[[prob]]
    }
  }
  return(dt_prob_list)
}


filter_prob_list <- function(dt_prob_list, sel_row){
  
  if (is.data.table(dt_prob_list)){ # if a single data.table wrap it with a list
    dt_prob_list <- list(dt_prob_list)
  } 
  
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
  if (length(filtered_prob_list)==1){
    return(filtered_prob_list[[1]])
  } else {
    return(filtered_prob_list)
  }
}

get_path_probs <- function(path_dt, dt_prob_list, dt_curr_states, x_cols){
  
  dt_pathprob_list <- list()
  for (i in 1:nrow(path_dt)){
    sel_row <- as.list(path_dt[i])
    chain_id2 <- unlist(sel_row$seg_ids)
    dest <- sel_row$dest
    path_i <- sel_row$path_id
    
    
    if (length(chain_id2)==1){
      # remove zeros from teh single segment
      dt <- filter_prob_list(dt_prob_list[[chain_id2]], sel_row)
      #x_cols <- grep("^sim_", names(dt), value = TRUE)
      temp_dt <- dt[rowSums(dt[, ..x_cols] != 0, na.rm = TRUE) > 0]
    } else { # do a product removing zeros from all segments before multiplication
      temp_dt <- smart_prod(filter_prob_list(dt_prob_list[chain_id2], sel_row), x_cols)
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


get_path_dt <- function(events_dt, dt_curr_states, all_events, twig_dims){
  default_event_scenario <- "none" 
  
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
        factor(event_list[[name]], 
               levels = levels(twig_dims[[name]]))
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
  p0 <- copy(states_layers$dt_p0)
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
  pb <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent); ETA::eta",
    total = n_cycles, 
    clear = FALSE, 
    width = 100
  )
  
  # Track start time
  start_time <- Sys.time()
  for (j in 1:n_cycles){
    if ("cycle" %in% names(trans_probs)){ # transition prob is cycle dependent
      P <- trans_probs[cycle==j,] 
    } else {
      P <- trans_probs
    }
    
    if (j == 1) p <- p0[, cycle:=1]
    p <- smart_prod(list(P, p), x_cols)
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
    event_levels <- events_dt$values[events_dt$event == event]
    list_dims[[event]] <- factor(event_levels, levels = event_levels)
  }
  return(list_dims)
}

weight_payoff_by_path_prob <- function(payoff_dt, payoff_event_cols, path_dt, dt_pathprob_list, x_cols){
  # iterate through each row of path_dt and multiply weights by payoffs
  weighted_payoff_list <- list()
  for (i in 1:nrow(path_dt)){
    sel_row <- as.list(path_dt[i])
    path_i <- sel_row$path_id
    filtered_payoff_dt <- payoff_dt[
        , .SD[Reduce(`&`, lapply(payoff_event_cols, function(col) get(col) == sel_row[[col]]))]
      ]
    
    # Remove columns in event_cols
    filtered_payoff_dt[, (payoff_event_cols) := NULL]
    dt_prob <- copy(dt_pathprob_list[[i]])
    # remove state2 
    dt_prob[, state2 := NULL] 
    weighted_payoff_list[[i]] <- smart_prod(list(dt_prob, filtered_payoff_dt), x_cols)
  }
  
  # 
  weighted_payoff <- smart_sum(weighted_payoff_list, x_cols)
  return(weighted_payoff)
}


compute_payoffs <- function(mytwig, event_cols, fun_outputs, path_dt, dt_pathprob_list, Trace, x_cols){
  payoff_layer <- retrieve_layer_by_type(mytwig, type = "payoffs") 
  payoff_names <- payoff_layer$payoffs
  
  # weight payoffs by paths if they have events
  payoff_trace_list <- list()
  total_payoff_list <- list()
  for (payoff in payoff_names){
    
    payoff_dt <- fun_outputs[[payoff]]
    payoff_event_cols <- intersect(names(payoff_dt), event_cols)
    
    # weight payoffs for each path if they are functions of events
    if (length(payoff_event_cols)>0){
      weighted_payoff <- weight_payoff_by_path_prob(payoff_dt, payoff_event_cols, path_dt, dt_pathprob_list, x_cols)
    } else {
      weighted_payoff <- payoff_dt
    }
    
    # apply discount 
    # multiply payoffs by the trace
    payoff_trace <- smart_prod(list(weighted_payoff, Trace), x_cols)
    
    # Apply the discount formula to each sim_* column
    discount_rate <- payoff_layer$discount_rates[payoff]
    payoff_trace[, (x_cols) := lapply(.SD, function(col) col / (1 + discount_rate)^cycle), .SDcols = x_cols]
    
    payoff_trace_list[[payoff]] <- payoff_trace
    
    # Sum for each column in x_cols without grouping
    column_sums <- c(list(payoff = payoff), unlist(lapply(payoff_trace[, ..x_cols], sum)))
    # Convert the result to a data.table for cleaner output
    total_payoff_list[[payoff]] <- as.data.table(column_sums)
  }
  
  payoff_trace_list
  
  total_payoff <- rbindlist(total_payoff_list)
  total_payoff
  
  # Calculate row means across x_cols
  mean_payoff <- copy(total_payoff)
  mean_payoff[, x_mean := rowMeans(.SD), .SDcols = x_cols]
  mean_payoff[, (x_cols) := NULL]

  return(list(mean_payoff = mean_payoff,
              total_payoff = total_payoff,
              payoff_trace_list = payoff_trace_list))
}




retrieve_layer_by_type <- function(twig_obj, type){
  # Use lapply to filter the list based on the condition
  outcome <- lapply(twig_obj$layers, function(x) if (x$type == type) x else NULL)
  # Remove NULL elements from the list
  lyr<-Filter(Negate(is.null), outcome)
  if (length(lyr)==1 & type != "event"){
    lyr <- lyr[[1]] #only select the first element if there is no more
  }
  lyr
}

twig_expand_functions <- function(twig_obj, 
                                  params = NULL,
                                  #fixed_params = NULL,
                                  fun_names = NULL, 
                                  excel_file_name=NULL){
  # add fixed parameters to the function environment
  # if (!is.null(fixed_params)){
  #   list2env(fixed_params)
  # }  
  if (!is.null(params)){
    if (!is.list(params)){
      stop("param has to be either a list of parameters scalars or a data.table with rows as simualtions and columns as parameters")
    }
    if (!is.data.table(params)){
      params <- as.data.table(params)
    }  
    params[,sim := .I]
    sim <- params$sim
    param_names <- names(params)
  } else {
    sim <- 1
  }
  
  # for each function get the function arguments
  if (is.null(fun_names)){
    fun_names <- fun_in_twig(twig_obj)
  }
  #fun_name <- fun_names
  if (!is.null(excel_file_name)){
    wb <- openxlsx::createWorkbook()
  }
  fun_outputs <- list()
  arg_values <- list()
  for (fun_name in fun_names){
    arg_all <- get_function_arguments(fun_name)
    # get the values for these arguments
    arg_core <- arg_all[arg_all!="cycle_in_state"] # function arguments other than cycle_in_state and PSA params
    if (!is.null(params)){
      arg_core <- arg_core[!(arg_core %in% param_names)]
      arg_params <- c("sim", param_names[param_names %in% arg_all]) # psa params + sim
      #sim_range <- params$sim
    } else {
      #sim_range <- NULL
    }
    for (arg_name in arg_core){ #arg_name <- arg_all[1]
      #if (arg_name == "state"){arg_name <- "expanded_state"}
      arg_values[[fun_name]][[arg_name]] <- fun_get_arg_values(twig_obj, arg_name)
    }
    values_dt <- do.call(CJ, c(arg_values[[fun_name]], list(sim = sim)))
    
    
    # Rename the 'state' column to 'expanded_state'
    setnames(values_dt, "state", "expanded_state")
    
    # Split 'expanded_state' into 'state' and 'cycle_in_state' based on "_tnl"
    values_dt[, c("state", "cycle_in_state") := tstrsplit(expanded_state, "_tnl", fixed = TRUE)]
    
    # Convert 'cycle_in_state' to integer, if necessary
    #values_dt[, cycle_in_state := as.integer(cycle_in_state)]
    suppressWarnings(values_dt[, cycle_in_state := as.integer(cycle_in_state)])
    
    # create the combinatorials of the arguments
    if (!is.null(params)){
      values_dt <- merge(values_dt, params[, ..arg_params], by = "sim")
    }
    
    # value of the function evaluation
    #values_dt[, x:=do.call(fun_name, lapply(.SD, as.character)), .SDcols = arg_all]
    # Use lapply with a condition to convert factors to character, and leave numeric columns unchanged
    values_dt[, x := do.call(fun_name, lapply(.SD, function(col) {
      if (is.factor(col)) {
        as.character(col)  # Convert factor to character
      } else {
        col  # Leave numeric column unchanged
      }
    })), .SDcols = arg_all]
    #pRecover("H")
    
    #variable_name <- paste0("dt_", fun_name)
    #cat("Note: The dataset ", variable_name, " created for function ", fun_name, ".\n", sep = "")
    #assign(variable_name, values_dt, envir = .GlobalEnv)
    # add sheets to workbook
    if (!is.null(excel_file_name)){
      openxlsx::addWorksheet(wb, fun_name)
      openxlsx::writeData(wb, sheet = fun_name, values_dt, startCol = 1, startRow = 1)
    }
    
    # Rename 'expanded_state' back to 'state'
    setnames(values_dt, "expanded_state", "state")
    
    # Remove the 'state' and 'cycle_in_state' columns
    #values_dt[, c("state", "cycle_in_state") := NULL]
    values_dt <- values_dt[, c("sim", arg_core, "x"), with = FALSE]
    # Dynamically identify the grouping columns (all columns except for 'sim' and 'x')
    group_cols <- setdiff(names(values_dt), c("sim", "x"))
    
    # Pivot wider by `sim` using the dynamic group columns
    values_dt_wide <- dcast(
      values_dt, 
      as.formula(paste(paste(group_cols, collapse = " + "), "~ sim")), 
      value.var = "x"
    )
    # Rename columns to include prefix "sim_"
    setnames(values_dt_wide, old = as.character(unique(values_dt$sim)), new = paste0("sim_", unique(values_dt$sim)))
    
    
    fun_outputs[[fun_name]] <- values_dt_wide
    
  }
  if (!is.null(excel_file_name)){
    openxlsx::saveWorkbook(wb, excel_file_name, overwrite = TRUE)
  }
  return(list(fun_names = fun_names, 
              arg_values = arg_values,
              fun_outputs = fun_outputs ))
}


fun_get_arg_values <- function(twig_obj, arg_name){
  if (arg_name %in% c("decision", "state", "cycle", "cycle_in_state")){
    arg_name <- paste0(arg_name, "s")
    if (arg_name %in% c("states")){
      # Use lapply to filter the list based on the condition
      index <- which(sapply(twig_obj$layers, function(x) "states" %in% x$type))
      # Remove NULL elements from the list
      lyr <- twig_obj$layers[[index]]
      
      arg_values <- factor(lyr[["expanded_states"]])
      
    } else if (arg_name %in% c("decisions")){
      # Use lapply to filter the list based on the condition
      index <- which(sapply(twig_obj$layers, function(x) arg_name %in% x$type))
      # Remove NULL elements from the list
      lyr <- twig_obj$layers[[index]]
      arg_values <- factor(lyr[[arg_name]])
    } else if (arg_name %in% c("cycles","cycle_in_states")){
      arg_values <- 1:n_cycles
    }
  } else if (arg_name %in% c("final_outcome")){
    # only for decision trees
    events_df <- get_event_df(twig_obj)
    arg_values <- factor(get_final_outcomes(events_df))
    
  } else { #event name
    # Use lapply to filter the list based on the condition
    index <- which(sapply(twig_obj$layers, function(x) arg_name %in% x$event))
    lyr <- twig_obj$layers[[index]]
    arg_values <- factor(lyr$values)
  }
  return(arg_values)
}

get_function_arguments <- function(func_name) {
  # Get the function object
  func <- get(func_name)
  
  # Get the formal arguments of the function
  arguments <- names(formals(func))
  
  return(arguments)
}



fun_in_twig <- function(twig_obj){
  # Get all objects in the global environment
  all_objects <- ls(envir = .GlobalEnv)
  
  # Filter out functions
  function_names <- all_objects[sapply(all_objects, function(x) is.function(get(x)))]
  
  twig_text <- paste(nested_list_to_string(twig_obj$layers), collapse=" ")
  # iterate through the functions and only returns that occur
  keep_fun <- c()
  for(fun_name in function_names){
    if (grepl(paste0("\\b", fun_name, "\\b"), twig_text)){
      keep_fun <- c(keep_fun, fun_name)
    }
  }
  return(keep_fun)
}


# Function to convert a nested list to a single long string
nested_list_to_string <- function(lst) {
  result <- ""
  for (item in lst) {
    if (is.list(item)) {
      result <- paste0(result, nested_list_to_string(item))
    } else if (is.character(item)) {
      result <- paste0(result, item, " ")
    } else if (inherits(item, "language")) {
      result <- paste0(result, deparse(item), " ")
    } else if (inherits(item, "call")) {
      #print("call")
      result <- paste0(result, as.character(item), " ")
    }
  }
  return(result)
}

# building transition prob matrix logic =======
get_event_df <- function(twig_obj){
  event_layers <- retrieve_layer_by_type(twig_obj, type = "event") 
  event_df_list <- list()
  i <- 0
  for (event_layer in event_layers){
    i <- i + 1
    # temp_df <- event_layers[[i]] %>% 
    #   as.data.frame() %>% 
    #   dplyr::mutate(values = as.character(values)) 
    temp_dt <- as.data.table(event_layers[[i]])[, values := as.character(values)]
    
    event_df_list[[i]] <- temp_dt
  }
  event_dt <- rbindlist(event_df_list)[, id := .I]
  
  #event_dt <- as.data.table(event_df)
  return(event_dt)
}

get_event_chain_ids <- function(data, goto_id) {
  events <- data$event[data$goto == goto_id]
  all_lineages <- list()
  
  for (event in events) {
    all_lineages[[length(all_lineages) + 1]] <- get_event_chain_ids(data, event)
  }
  
  if (length(events) == 0) {
    return(0) #list(as.character(outcomes_id)))
  } else {
    individual_lineages <- list()
    if (length(all_lineages)>0){
      for (i in 1:length(all_lineages)) {
        X <- get_id_with_events(data, goto_id)[i]
        individual_lineages <- c(
          individual_lineages, 
          lapply(all_lineages[[i]], function(x) c(x, X))
        )
      }
    } else {
      stop("An error occured in the event_mappings.  Make sure the events are correctly mapped. For more information please refer to the vignettes")
    }
    
    return(individual_lineages)
  }
}

# Function to retrieve the value 'X' based on the 'event'
get_id_with_events <- function(data, goto_id) {
  #return(paste0("(",data$probs[data$goto == goto_id],")"))
  return(data$id[data$goto == goto_id])
}