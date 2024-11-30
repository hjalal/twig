# nolint start
# Helper functions
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("To get started with twig, please check the Articles section on https://hjalal.github.io/twig/")
}


# takes in all the segment probs of an event and computes the complement
return_complement <- function(compl_id, fun_outputs, x_cols, events_df) {
  event_i <- events_df[id == compl_id]$event
  p_names <- events_df[event == event_i & probs != "#"]$probs

  if (length(p_names) == 1) { # just compute complementary
    result_dt <- copy(fun_outputs[[p_names]])

    # Apply the transformation (1 - x) to each 'sim_' column
    result_dt[, (x_cols) := lapply(.SD, function(x) 1 - x), .SDcols = x_cols]
  } else { # join, sum and compute complementary
    dt_list <- fun_outputs[p_names]
    result_dt <- harmonize_and_join(
      sel_fun_outputs = dt_list,
      x_cols = x_cols,
      operation = "sum",
      complement = TRUE
    )
  }
  return(result_dt)
}


# # idea to expand the data.table so they all have the same dimenions. each array has a single active variable = x
# smart_sum <- function(sel_fun_outputs, x_cols, complement = FALSE){
#   # harmonize and join the objects
#   res <- harmonize_and_join(sel_fun_outputs, x_cols, remove_zeros = !complement, operation = "sum")
#   joined_datasets <- res$joined_datasets
#   # stack them
#
#
#   return(aggregated_data)
#   # sum
# }


# # harmonize the data.tables and then multiply them by only multiplying those that have elements in all data.tables
# smart_prod <- function(sel_fun_outputs, x_cols){
#   # harmonize and join the objects
#   res <- harmonize_and_join(sel_fun_outputs, x_cols, remove_zeros = TRUE)
#   joined_datasets <- res$joined_datasets
#   unique_cols = res$unique_cols
#   n_sel_fun_outputs <- length(sel_fun_outputs)
#   # stack them
#   # conditional product if # is == number of datasets
#   # Aggregate to compute product of 'x' only when .N == n_sel_fun_outputs
#   aggregated_data <- joined_datasets[, lapply(.SD, function(col) if (.N == n_sel_fun_outputs) prod(col) else NA_real_),
#                                      .SDcols = x_cols, by = unique_cols]
#   aggregated_data <- joined_datasets[, c(
#     # Calculate the product for each sim_ column
#     lapply(.SD, function(col) prod(col, na.rm = TRUE)),
#     # Calculate the count of rows in each group
#     count_col = .N
#   ), .SDcols = x_cols,
#   by = unique_cols
#   ]
#
#   # Remove rows where all x_cols are NA
#   aggregated_data <- aggregated_data[N == n_sel_fun_outputs]
#   aggregated_data[, N := NULL]
#   return(aggregated_data)
#   # sum
# }


# looks into a list of functions and returns  unique values across all functions.
# note this is different from all possible values
get_unique_values <- function(sel_fun_outputs, x_cols) {
  unique_values <- list()
  for (i in 1:length(sel_fun_outputs)) {
    dataset <- sel_fun_outputs[[i]]

    # Exclude the 'x' columns
    relevant_columns <- setdiff(names(dataset), x_cols)

    for (col in relevant_columns) {
      unique_vals <- unique(dataset[[col]])
      # If this is the first dataset being processed for the column, it simply adds the new unique values.
      if (!is.null(unique_values[[col]])) {
        unique_values[[col]] <- unique(c(unique_values[[col]], unique_vals))
      } else {
        unique_values[[col]] <- unique_vals
      }
    }
  }
  return(unique_values)
}

# harmonize and join twig functions. this is essential for a minimal joining.
# first it determines which columns should be expanded in which function_dataset
# only columns that don't exist are added with all possible values.
# if a column already exists its values are kept (not expanded to all possible)
harmonize_and_join <- function(sel_fun_outputs, x_cols, operation = NULL, complement = FALSE) {
  # really only want to keep zeros if we are computing the complements to get 1 for 0s.
  # for sum and multiplication it will be OK to remove 0s.
  remove_zeros <- !complement
  # Initialize lists to hold unique values
  unique_values <- get_unique_values(sel_fun_outputs, x_cols)

  # Initialize a list to store the joined datasets
  joined_dataset_list <- list()
  unique_cols <- names(unique_values)
  n_sel_fun_outputs <- length(sel_fun_outputs)
  # Iterate through each dataset to perform the cross join
  for (i in 1:n_sel_fun_outputs) {
    # only keep datasets with non-zeros
    # Filter rows where all values in x_cols are NOT equal to 0
    if (remove_zeros) { # don't remove 0s for complement
      dt <- sel_fun_outputs[[i]]
      dataset <- dt[rowSums(dt[, ..x_cols] != 0, na.rm = TRUE) > 0]
    } else {
      dataset <- sel_fun_outputs[[i]]
    }

    # Determine the columns for the join
    x <- names(dataset) # before
    y <- c(unique_cols, x_cols) # after what each dataset should have ...
    relevant_columns <- setdiff(y, x) # what is missing and need to be expanded
    shared_columns <- unique_cols[unique_cols %in% x] # y[y %in% x & !(y %in% x_cols)]
    if (length(relevant_columns) > 0) {
      # Create a list to hold data tables for the cross join
      join_data <- do.call(CJ, unique_values)

      if (length(shared_columns) > 0) {
        join_data <- merge(join_data, dataset, by = shared_columns) # ,allow.cartesian = TRUE)
      } else {
        # Add a temporary column to both A and B
        join_data[, tmp := 1]
        dataset[, tmp := 1]

        # Perform the Cartesian join using the temporary column and remove it afterward
        join_data <- merge(join_data, dataset, by = "tmp", allow.cartesian = TRUE)[, tmp := NULL]
        # join_data <- join_data[dataset, on = .(tmp), allow.cartesian = TRUE][, tmp := NULL]
      }
    } else {
      join_data <- dataset
    }

    current_key <- key(join_data)
    # Check if the desired key is already set
    if (!identical(current_key, unique_cols)) {
      setkeyv(join_data, cols = unique_cols)
    }
    # Store the result in the list
    joined_dataset_list[[i]] <- join_data
  }
  # make sure unique_cols are keys for all of the datasets
  # joined_datasets <- rbindlist(joined_dataset_list, use.names = TRUE)
  # joined_datasets <- Reduce(function(x,y) merge(x,y, by = unique_cols, all = full_join), joined_dataset_list)
  joined_datasets <- Reduce(
    function(...) {
      custom_merge(..., shared_columns = unique_cols, x_cols, operation = operation)
    },
    joined_dataset_list
  )

  # if complement compute 1 - x for each x_cols.

  if (complement) {
    joined_datasets <- joined_datasets[, (x_cols) := lapply(.SD, function(x) 1 - x), .SDcols = x_cols]
  }

  # add garbage collector
  gc()
  return(joined_datasets)
}


custom_merge <- function(dt1, dt2, shared_columns, x_cols, operation = NULL) {
  full_join <- (operation == "sum")

  # Perform the merge with the specified `all` argument
  merged <- merge(dt1, dt2, by = shared_columns, all = full_join, suffixes = c(".x", ".y"))
  # Identify the columns with `.x` and `.y` suffixes
  x_cols_x <- paste0(x_cols, ".x")
  x_cols_y <- paste0(x_cols, ".y")
  # Loop over each column in `x_cols` to compute row-wise sums and assign to the final columns
  for (col in x_cols) {
    col_x <- paste0(col, ".x")
    col_y <- paste0(col, ".y")
    if (operation == "sum") {
      # Assign the row-wise sum of `.x` and `.y` columns to the original column name
      merged[, (col) := rowSums(.SD, na.rm = TRUE), .SDcols = c(col_x, col_y)]
    } else if (operation == "prod") {
      # This method keeps everything within the `data.table` structure
      merged[, (col) := Reduce(`*`, .SD), .SDcols = c(col_x, col_y)]
    } else {
      stop("allowed operations are sum and product")
    }
  }
  # Remove the `.x` and `.y` columns
  merged[, (c(x_cols_x, x_cols_y)) := NULL]
  gc()
  # Sum the same columns from both datasets and store them as the final x_cols
  return(merged)
}



# get segment probabilities. segments mean each event scenario.
# also takes care of complement segments
get_seg_probs <- function(events_df, fun_outputs, x_cols) {
  dt_prob_list <- list() # as.list(rep(NA,n))
  # Event/Scenario level
  # iterate through each id in events_df and assign htem to the dt_prob_list
  for (i in 1:nrow(events_df)) {
    # if probs == hash
    prob <- events_df[i, probs]
    id <- events_df[i]$id
    if (prob == "#") { # compute and return the complement
      dt_prob_list_temp <- return_complement(compl_id = id, fun_outputs = fun_outputs, x_cols, events_df)
      dt_prob_list[[id]] <- dt_prob_list_temp
    } else { # just get the dataset
      dt_prob_list[[id]] <- fun_outputs[[prob]]
    }
  }
  return(dt_prob_list)
}



# takes care of choosing probability rows from a probability dataset with events that match
# the events in the paths.  note that probability data.tables can have multiple events, but
# each path really can have a single value per event.
filter_prob_list <- function(dt_prob_list, sel_row) {
  if (is.data.table(dt_prob_list)) { # if a single data.table wrap it with a list
    dt_prob_list <- list(dt_prob_list)
  }

  filtered_prob_list <- list()
  for (i in 1:length(dt_prob_list)) {
    dt_prob <- dt_prob_list[[i]]
    event_cols <- intersect(names(dt_prob), names(sel_row))
    if (length(event_cols) > 0) {
      # Filter rows based on matching elements in sel_row, ignoring missing columns
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
  if (length(filtered_prob_list) == 1) {
    return(filtered_prob_list[[1]])
  } else {
    return(filtered_prob_list)
  }
}


# combines all segment probabilities on a path to a single probability data.table
# including all dependencies on decision, state and cycles, but events are removed
# since they are not needed anymore.
get_path_probs <- function(path_dt, dt_prob_list, x_cols, dt_curr_states, twig_type) {
  dt_pathprob_list <- list()
  for (i in 1:nrow(path_dt)) {
    sel_row <- as.list(path_dt[i])
    chain_id2 <- unlist(sel_row$seg_ids)
    dest <- sel_row$dest
    path_i <- sel_row$path_id

    if (length(chain_id2) == 1) {
      # remove zeros from teh single segment
      dt <- filter_prob_list(dt_prob_list[[chain_id2]], sel_row)
      temp_dt <- dt[rowSums(dt[, ..x_cols] != 0, na.rm = TRUE) > 0]
    } else { # do a product removing zeros from all segments before multiplication
      if (dest == "curr_state") {
        z <- 1
      }
      temp_dt <- harmonize_and_join(
        sel_fun_outputs = filter_prob_list(dt_prob_list[chain_id2], sel_row),
        x_cols = x_cols,
        operation = "prod"
      )
    }

    # if markov, add markov details to the path data.
    if (twig_type == "markov") {
      if (dest == "curr_state") {
        # Split 'expanded_state' into 'state' and 'cycle_in_state' based on "_tnl"
        temp_dt <- merge(temp_dt, dt_curr_states, by = "state")

        # make sure all are values are added together because the current_state may generate
        # duplicated paths.

        cols <- names(temp_dt)
        key_cols <- cols[!cols %in% x_cols]
        temp_dt <- temp_dt[, lapply(.SD, sum), .SDcols = x_cols, by = key_cols]
      } else {
        if (dest %in% dt_curr_states$state) {
          dest_state <- dest
        } else {
          dest_state <- paste0(dest, "_tnl1")
        }
        temp_dt[, state2 := dest_state]
      }
    }

    dt_pathprob_list[[path_i]] <- temp_dt
  }

  return(dt_pathprob_list)
}

# creates a data.table for each path, with originating state, destination states,
# a vector of segment ids on the path
get_path_dt <- function(events_df, dt_curr_states, all_events, twig_dims, twig_type) {
  default_event_scenario <- "none"

  # compute product of prob by joining first then multiplying.
  origin_events <- unique(events_df$event)
  dest <- unique(events_df$goto)
  dest_states <- dest[!dest %in% origin_events]

  dt_path_list <- list()
  # for each path join & multiply ids
  # path_ids <- unique(path_dt$path_id)
  path_i <- 0
  # dest <- dest_states[2]

  for (dest in dest_states) {
    chain_ids <- get_event_chain_ids(events_df, goto_id = dest)

    for (chain_id in chain_ids) {
      path_i <- path_i + 1
      chain_id2 <- chain_id[chain_id > 0]
      filtered_events <- events_df[chain_id2]

      # Create a named list with `get_sick` and `die` keys, setting each to "yes"
      other_events <- setdiff(all_events, filtered_events$event)
      other_event_values <- rep(default_event_scenario, length(other_events))
      event_list <- setNames(
        as.list(c(filtered_events$values, other_event_values)),
        c(filtered_events$event, other_events)
      )

      # Convert each event_list item to a factor with levels from twig_dims
      event_factors <- lapply(names(event_list), function(name) {
        factor(event_list[[name]],
          levels = levels(twig_dims[[name]])
        )
      })
      # Set names for the resulting vector
      names(event_factors) <- names(event_list)

      # Convert to data.table and assign to a variable
      event_factors_dt <- as.data.table(event_factors)

      # Add columns using := syntax
      dt_path_list[[path_i]] <- event_factors_dt[, `:=`(path_id = path_i, dest = dest, seg_ids = list(chain_id2))]
    }
  }
  path_dt <- rbindlist(dt_path_list, use.names = TRUE)
  if (twig_type == "decision_tree") {
    setnames(path_dt, "dest", "outcome")
  }
  return(path_dt)
}


# adds simulation and cycles to the p0 vector
get_dt_p0 <- function(states_layers, n_sims, x_cols) {
  p0 <- copy(states_layers$dt_p0)
  # Repeat 'x' for the number of simulations
  # Create new columns 'x_sim1', 'x_sim2', ..., 'x_simN' by repeating 'x'
  p0[, (x_cols) := lapply(1:n_sims, function(i) rep(x, length.out = .N))]
  p0[, x := NULL]
  return(p0)
}


# computes the trace data.table, by iteratively multiplying p0 against the
# trans_prob data.table.
get_trace <- function(trans_probs, p0, n_cycles, x_cols) {
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
  for (j in 1:n_cycles) {
    if ("cycle" %in% names(trans_probs)) { # transition prob is cycle dependent
      P <- trans_probs[cycle == j, ]
    } else {
      P <- trans_probs
    }

    if (j == 1) p <- p0[, cycle := 1]
    p <- harmonize_and_join(
      sel_fun_outputs = list(P, p),
      x_cols = x_cols,
      operation = "prod"
    )
    # Remove the original 'state' column
    p[, state := NULL]
    # Rename 'state2' to 'state'
    setnames(p, "state2", "state")
    # Modify the code to sum each element of x_cols
    p <- p[, lapply(.SD, sum, na.rm = TRUE),
      .SDcols = x_cols,
      by = setdiff(names(p), x_cols)
    ]

    list_dt_trace[[j]] <- copy(p)
    p[, cycle := cycle + 1]

    pb$tick()
    gc()
  }
  Trace <- rbindlist(list_dt_trace, use.names = TRUE)
  return(Trace)
}


# get decision, state, event(s), cycle and sim dimensions.
# these are all possible values for the various twig dimensions
# including decsioins, states(expanded), event(s), cycles
get_twig_dims <- function(twig_env, events_df, n_sims, twig_type, n_cycles = NULL) {
  decisions <- retrieve_layer_by_type(twig_env, type = "decisions")$decisions
  if (twig_type == "markov") {
    states <- retrieve_layer_by_type(twig_env, type = "states")$expanded_states
    list_dims <- list(
      decision = factor(decisions, levels = decisions),
      state = factor(states, levels = states),
      cycle = 1:n_cycles,
      sim = 1:n_sims
    )
  } else {
    outcomes <- unique(events_df$goto[!events_df$goto %in% events_df$event])
    list_dims <- list(
      decision = factor(decisions, levels = decisions),
      outcome = factor(outcomes, levels = outcomes),
      sim = 1:n_sims
    )
  }

  events <- unique(events_df$event)
  for (event in events) {
    event_levels <- events_df$values[events_df$event == event]
    list_dims[[event]] <- factor(event_levels, levels = event_levels)
  }
  return(list_dims)
}

# this function is needed for transition payoffs/rewards.  This is because the transitoin rewards depend on events values
# these events happen at the path level.
weight_payoff_by_path_prob <- function(payoff_dt, payoff_event_cols, path_dt, dt_pathprob_list, x_cols, twig_type) {
  # iterate through each row of path_dt and multiply weights by payoffs
  weighted_payoff_list <- list()
  for (i in 1:nrow(path_dt)) {
    sel_row <- as.list(path_dt[i])
    path_i <- sel_row$path_id
    filtered_payoff_dt <- payoff_dt[
      , .SD[Reduce(`&`, lapply(payoff_event_cols, function(col) get(col) == sel_row[[col]]))]
    ]

    # Remove columns in event_cols
    filtered_payoff_dt[, (payoff_event_cols) := NULL]
    dt_prob <- copy(dt_pathprob_list[[i]])
    # remove state2
    if (twig_type == "markov") {
      dt_prob[, state2 := NULL]
    }
    weighted_payoff_list[[i]] <- harmonize_and_join(
      sel_fun_outputs = list(dt_prob, filtered_payoff_dt),
      x_cols = x_cols,
      operation = "prod"
    )
  }

  #
  weighted_payoff <- harmonize_and_join(
    sel_fun_outputs = weighted_payoff_list,
    x_cols = x_cols,
    operation = "sum",
    complement = FALSE
  )
  return(weighted_payoff)
}

# calculates the markov payoffs.  If there are any transition rewards, these are weighted first at the path level.
# Once these are weighted by the path probabilities then they are multiplied by the Markov trace.
compute_payoffs_markov <- function(twig_env, event_cols, fun_outputs, path_dt, dt_pathprob_list, Trace, x_cols) {
  payoff_layer <- retrieve_layer_by_type(twig_env, type = "payoffs")
  payoff_names <- payoff_layer$payoffs

  # weight payoffs by paths if they have events
  payoff_trace_list <- list()
  total_payoff_list <- list()
  for (payoff in payoff_names) {
    payoff_dt <- fun_outputs[[payoff]]
    payoff_event_cols <- intersect(names(payoff_dt), event_cols)

    # weight payoffs for each path if they are functions of events
    if (length(payoff_event_cols) > 0) {
      weighted_payoff <- weight_payoff_by_path_prob(payoff_dt, payoff_event_cols, path_dt, dt_pathprob_list, x_cols, twig_type = "markov")
    } else {
      weighted_payoff <- payoff_dt
    }

    # apply discount
    # multiply payoffs by the trace
    payoff_trace <- harmonize_and_join(
      sel_fun_outputs = list(weighted_payoff, Trace),
      x_cols = x_cols,
      operation = "prod"
    )

    # Apply the discount formula to each sim_* column
    discount_rate <- payoff_layer$discount_rates[payoff]
    payoff_trace[, (x_cols) := lapply(.SD, function(col) col / (1 + discount_rate)^cycle), .SDcols = x_cols]

    payoff_trace_list[[payoff]] <- payoff_trace

    # Sum for each column in x_cols without grouping
    # column_sums <- c(list(payoff = payoff), unlist(lapply(payoff_trace[, ..x_cols], sum)))
    # Sum for each column in x_cols, grouped by "decision"
    total_payoff_list[[payoff]] <- payoff_trace[, c(
      list(payoff = unique(payoff)),
      lapply(.SD, sum)
    ),
    by = decision, .SDcols = x_cols
    ]
    # Convert the result to a data.table for cleaner output
    # total_payoff_list[[payoff]] <- as.data.table(column_sums)
  }

  payoff_trace <- rbindlist(payoff_trace_list)

  total_payoff <- rbindlist(total_payoff_list, use.names = TRUE)
  total_payoff

  # Calculate row means across x_cols
  mean_payoff <- copy(total_payoff)
  mean_payoff[, x_mean := rowMeans(.SD), .SDcols = x_cols]
  mean_payoff[, (x_cols) := NULL]
  gc()
  return(list(
    mean_payoff = mean_payoff,
    total_payoff = total_payoff,
    payoff_trace = payoff_trace
  ))
}


# calculates the payoffs for Decision Trees.  These are a bit differnet from Markov models, in that
# we only need to determine the path probabiliteis and multiply those by the payoffs.
# Again payoffs must be filtered for only those that apply to the path first.
compute_payoffs_decision_tree <- function(twig_env, event_cols, fun_outputs, path_dt, dt_pathprob_list, x_cols) {
  payoff_layer <- retrieve_layer_by_type(twig_env, type = "payoffs")
  payoff_names <- payoff_layer$payoffs

  # weight payoffs by paths if they have events
  payoff_trace_list <- list()
  total_payoff_list <- list()
  for (payoff in payoff_names) {
    payoff_dt <- fun_outputs[[payoff]]
    payoff_event_cols <- intersect(names(payoff_dt), c(event_cols, "outcome"))

    # multiply path probs by payoff
    # iterate through each row of path_dt and multiply weights by payoffs
    weighted_payoff_list <- list()
    for (i in 1:nrow(path_dt)) {
      sel_row <- as.list(path_dt[i])
      path_i <- sel_row$path_id

      # only get payoffs with events that match the event scenarios in the path
      # if (length(payoff_event_cols)>0){
      filtered_payoff_dt <- payoff_dt[
        , .SD[Reduce(`&`, lapply(payoff_event_cols, function(col) get(col) == sel_row[[col]]))]
      ]
      filtered_payoff_dt[, (payoff_event_cols) := NULL]
      # }

      dt_prob <- copy(dt_pathprob_list[[i]])
      # remove state2
      weighted_payoff_list[[i]] <- harmonize_and_join(
        sel_fun_outputs = list(dt_prob, filtered_payoff_dt),
        x_cols = x_cols,
        operation = "prod"
      )
    }

    #
    weighted_payoff <- harmonize_and_join(
      sel_fun_outputs = weighted_payoff_list,
      x_cols = x_cols,
      operation = "sum",
      complement = FALSE
    )
    payoff_trace_list[[payoff]] <- weighted_payoff

    # Sum for each column in x_cols, grouped by "decision"
    column_sums <- weighted_payoff[, c(
      list(payoff = unique(payoff)),
      lapply(.SD, sum)
    ),
    by = decision, .SDcols = x_cols
    ]
    # Convert the result to a data.table for cleaner output
    total_payoff_list[[payoff]] <- as.data.table(column_sums)
  }

  payoff_trace <- rbindlist(payoff_trace_list)

  total_payoff <- rbindlist(total_payoff_list, use.names = TRUE)
  # total_payoff

  # Calculate row means across x_cols
  mean_payoff <- copy(total_payoff)
  mean_payoff[, x_mean := rowMeans(.SD), .SDcols = x_cols]
  mean_payoff[, (x_cols) := NULL]
  gc()

  return(list(
    mean_payoff = mean_payoff,
    total_payoff = total_payoff,
    payoff_trace = payoff_trace
  ))
}



# retrieves Twig layers by their type.
retrieve_layer_by_type <- function(twig_env, type) {
  # Use lapply to filter the list based on the condition
  outcome <- lapply(twig_env$layers, function(x) if (x$type == type) x else NULL)
  # Remove NULL elements from the list
  lyr <- Filter(Negate(is.null), outcome)
  if (length(lyr) == 1 & type != "event") {
    lyr <- lyr[[1]] # only select the first element if there is no more
  }
  lyr
}

# Expands the functions by generating data.tables for each function based on their dependencies and parameters
# the cool thing here is that it also takes in the parameters and creates columns of values one column per parameter sample set.
twig_expand_functions <- function(twig_env,
                                  twig_type,
                                  params = NULL,
                                  n_cycles = NULL,
                                  states_layers = NULL,
                                  # fixed_params = NULL,
                                  fun_names = NULL,
                                  excel_file_name = NULL) {
  # add fixed parameters to the function environment
  if (!is.null(params)) {
    sim <- params$sim
    param_names <- names(params)
  } else {
    sim <- 1
  }

  # for each function get the function arguments
  if (is.null(fun_names)) {
    fun_names <- fun_in_twig(twig_env)
  }
  # fun_name <- fun_names
  if (!is.null(excel_file_name)) {
    wb <- openxlsx::createWorkbook()
  }
  fun_outputs <- list()
  arg_values <- list()
  for (fun_name in fun_names) {
    arg_all <- get_function_arguments(fun_name)

    # get the values for these arguments
    arg_core <- arg_all[arg_all != "cycle_in_state"] # function arguments other than cycle_in_state and sim params
    if (!is.null(params)) {
      arg_core <- arg_core[!(arg_core %in% param_names)]
      arg_params <- c("sim", param_names[param_names %in% arg_all]) # sim params + sim
    }
    if (!"state" %in% arg_core) {
      arg_core <- c(arg_core, "state")
    }
    for (arg_name in arg_core) { # arg_name <- arg_all[1]
      # if (arg_name == "state"){arg_name <- "expanded_state"}
      # print(paste(fun_name, "-", arg_name))
      arg_values[[fun_name]][[arg_name]] <- get_fun_arg_values(twig_env, arg_name, n_cycles = n_cycles)
    }
    values_dt <- do.call(CJ, c(arg_values[[fun_name]], list(sim = sim)))

    if (twig_type == "markov") {
      # Rename the 'state' column to 'expanded_state'
      setnames(values_dt, "state", "expanded_state")

      # Split 'expanded_state' into 'state' and 'cycle_in_state' based on "_tnl"
      values_dt[, c("state", "cycle_in_state") := tstrsplit(expanded_state, "_tnl", fixed = TRUE)]

      # Convert 'cycle_in_state' to integer, if necessary
      # values_dt[, cycle_in_state := as.integer(cycle_in_state)]
      suppressWarnings(values_dt[, cycle_in_state := as.integer(cycle_in_state)])
    }


    # create the combinatorials of the arguments
    if (!is.null(params)) {
      values_dt <- merge(values_dt, params[, ..arg_params], by = "sim")
    }

    # value of the function evaluation
    # Use lapply with a condition to convert factors to character, and leave numeric columns unchanged
    values_dt[, x := do.call(fun_name, lapply(.SD, function(col) {
      if (is.factor(col)) {
        as.character(col) # Convert factor to character
      } else {
        col # Leave numeric column unchanged
      }
    })), .SDcols = arg_all]

    # add sheets to workbook
    if (!is.null(excel_file_name)) {
      openxlsx::addWorksheet(wb, fun_name)
      openxlsx::writeData(wb, sheet = fun_name, values_dt, startCol = 1, startRow = 1)
    }

    if (twig_type == "markov") {
      # Rename 'expanded_state' back to 'state'
      setnames(values_dt, "expanded_state", "state")
    }

    # Remove the 'state' and 'cycle_in_state' columns
    values_dt <- values_dt[, c("sim", arg_core, "x"), with = FALSE]
    # Dynamically identify the grouping columns (all columns except for 'sim' and 'x')
    group_cols <- setdiff(names(values_dt), c("sim", "x"))

    # # Pivot wider by `sim` using the dynamic group columns
    # values_dt_wide <- dcast(
    #   values_dt,
    #   as.formula(paste(paste(group_cols, collapse = " + "), "~ sim")),
    #   value.var = "x"
    # )
    # Check if group_cols is empty and create the formula accordingly
    if (length(group_cols) > 0) {
      formula_str <- paste(paste(group_cols, collapse = " + "), "~ sim")
      # Pivot wider by `sim` using the dynamic formula
      values_dt_wide <- dcast(
        values_dt,
        as.formula(formula_str),
        value.var = "x"
      )
    } else {
      formula_str <- "1 ~ sim"
      # Pivot wider by `sim` using the dynamic formula
      values_dt_wide <- dcast(
        values_dt,
        as.formula(formula_str),
        value.var = "x"
      )
      values_dt_wide <- values_dt_wide[, !".", with = FALSE]
    }



    # Rename columns to include prefix "sim_"
    setnames(values_dt_wide, old = as.character(unique(values_dt$sim)), new = paste0("sim_", unique(values_dt$sim)))


    fun_outputs[[fun_name]] <- values_dt_wide
  }
  if (!is.null(excel_file_name)) {
    openxlsx::saveWorkbook(wb, excel_file_name, overwrite = TRUE)
  }
  return(list(
    fun_names = fun_names,
    arg_values = arg_values,
    fun_outputs = fun_outputs
  ))
}


# gets function values from the twig. note that state is already combiend with cycle_in_state if it is a tunnel state.
get_fun_arg_values <- function(twig_env, arg_name, n_cycles = NULL) {
  if (arg_name %in% c("decision", "state", "cycle")) { # , "cycle_in_state")){
    arg_name <- paste0(arg_name, "s")

    if (arg_name %in% c("states")) {
      # Use lapply to filter the list based on the condition
      index <- which(sapply(twig_env$layers, function(x) "states" %in% x$type))
      # Remove NULL elements from the list
      lyr <- twig_env$layers[[index]]
      arg_values <- lyr$names
    } else if (arg_name %in% c("decisions")) {
      # Use lapply to filter the list based on the condition
      index <- which(sapply(twig_env$layers, function(x) arg_name %in% x$type))
      # Remove NULL elements from the list
      lyr <- twig_env$layers[[index]]
      arg_values <- lyr[[arg_name]]
    } else if (arg_name %in% c("cycles")) { # ,"cycle_in_states")){
      arg_values <- 1:n_cycles
    }
  } else if (arg_name %in% c("outcome")) {
    # only for decision trees
    events_df <- get_events_df(twig_env)
    arg_values <- unique(events_df$goto[!events_df$goto %in% events_df$event])
  } else if (arg_name == "expanded_state") {
    # Use lapply to filter the list based on the condition
    index <- which(sapply(twig_env$layers, function(x) "states" %in% x$type))
    # Remove NULL elements from the list
    lyr <- twig_env$layers[[index]]
    tunnel_lengths <- lyr$tunnel_lengths
    states <- lyr$names
    states_expanded <- rep(states, tunnel_lengths)
    # Create expanded states only for states with tunnel_lengths > 1
    expanded_states <- lapply(1:length(states), function(i) {
      if (tunnel_lengths[i] > 1) {
        paste0(states[i], "_tnl", 1:tunnel_lengths[i])
      } else {
        states[i]
      }
    })
    # Flatten the list to a vector
    arg_values <- unlist(expanded_states)
  } else { # event name
    # Use lapply to filter the list based on the condition
    index <- which(sapply(twig_env$layers, function(x) arg_name %in% x$event))
    lyr <- twig_env$layers[[index]]
    arg_values <- lyr$values
  }
  return(arg_values)
}


# takes out function arguments from the function name definition.
get_function_arguments <- function(func_name) {
  # Handle single or multiple function names
  if (length(func_name) == 1) {
    # Get the function object
    func <- get(func_name)

    # Get the formal arguments of the function
    arguments <- names(formals(func))
  } else {
    # For multiple functions, get unique arguments across all
    arguments <- unique(unlist(lapply(func_name, function(fname) {
      func <- get(fname)
      names(formals(func))
    })))
  }

  return(arguments)
}



fun_in_twig <- function(twig_env) {
  # get initial probabilities functions
  states <- retrieve_layer_by_type(twig_env, type = "states")
  init_probs <- states$init_probs

  # Get events from twig object
  events <- retrieve_layer_by_type(twig_env, type = "event")

  # Extract and flatten all probability values
  all_probs <- unlist(lapply(events, function(x) x$probs))

  # Remove '#' placeholder and get unique values
  unique_probs <- unique(all_probs[!all_probs %in% c('"#"', "complement")])

  # get payoffs
  payoffs <- retrieve_layer_by_type(twig_env, type = "payoffs")
  unique_payoffs <- payoffs$payoffs

  unique_funs <- c(init_probs, unique_probs, unique_payoffs)


  # # Check if each function exists
  # for (fun_name in unique_funs) {
  #   if (!exists(fun_name, mode = "function")) {
  #     stop(sprintf("Function '%s' does not exist", fun_name))
  #   }
  # }

  # Filter to keep only elements that are functions
  unique_funs <- unique_funs[sapply(unique_funs, function(fun_name) {
    exists(fun_name, mode = "function")
  })]

  return(unique_funs)
}

get_core_args <- function(mytwig) {
  core_args <- c("decision", "state", "cycle", "expanded_state", "final_outcome")
  events <- retrieve_layer_by_type(mytwig, type = "event")
  for (event in events) {
    core_args <- c(core_args, event$event)
  }
  return(core_args)
}


# Function to convert a nested list to a single long string
# may not need this anymore.
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
      result <- paste0(result, as.character(item), " ")
    }
  }
  return(result)
}


# creates a data.table by event scenario, their origin and destination.
# each one of these correspond to a segment. And some are complements.
# get_events_df <- function(twig_env) {
#   event_layers <- retrieve_layer_by_type(twig_env, type = "event")
#   events_df_list <- list()
#   i <- 0
#   for (event_layer in event_layers) {
#     i <- i + 1
#     # temp_df <- event_layers[[i]] %>%
#     #   as.data.frame() %>%
#     #   dplyr::mutate(values = as.character(values))
#     temp_dt <- as.data.frame(event_layers[[i]])[, values := as.character(values)]

#     events_df_list[[i]] <- temp_dt
#   }
#   events_df <- rbindlist(events_df_list, use.names = TRUE)[, id := .I]

#   # events_df <- as.data.frame(events_df)
#   return(events_df)
# }

# retrieves the segement ids in each chain from an original state to a destination.
get_event_chain_ids <- function(data, goto_id) {
  events <- data$event[data$goto == goto_id]
  all_lineages <- list()

  for (event in events) {
    all_lineages[[length(all_lineages) + 1]] <- get_event_chain_ids(data, event)
  }

  if (length(events) == 0) {
    return(0) # list(as.character(outcomes_id)))
  } else {
    individual_lineages <- list()
    if (length(all_lineages) > 0) {
      for (i in 1:length(all_lineages)) {
        X <- get_id_with_events(data, goto_id)[i]
        individual_lineages <- c(
          individual_lineages,
          lapply(all_lineages[[i]], function(x) c(x, X))
        )
      }
    } else {
      stop("An error occured in the event_mappings.  Make sure the events are correctly mapped. For more information please refer to the articles on Github")
    }

    return(individual_lineages)
  }
}

# Function to retrieve the value 'X' based on the 'event'
get_id_with_events <- function(data, goto_id) {
  # return(paste0("(",data$probs[data$goto == goto_id],")"))
  return(data$id[data$goto == goto_id])
}

# determines twig type based on hte presence of the states layer.
get_twig_type <- function(twig_env) {
  states <- retrieve_layer_by_type(twig_env, type = "states")
  if (length(states) > 0) {
    twig_type <- "markov"
  } else {
    twig_type <- "decision_tree"
  }
  return(twig_type)
}


# function to convert a vector to a string of function names
to_strings <- function(vec) {
  # as.character(substitute(vec)[-1])
  sapply(substitute(vec)[-1], deparse)
}


# incomplete function - needs to be completed.
expand_states <- function(twig_env) {
  states_layer <- retrieve_layer_by_type(twig_env, type = "states")
  names <- states_layer$names
  tunnel_lengths <- states_layer$tunnel_lengths
  init_probs <- states_layer$init_probs

  # Create expanded states only for states with tunnel_lengths > 1
  expanded_states <- lapply(1:length(names), function(i) {
    if (tunnel_lengths[i] > 1) {
      paste0(names[i], "_tnl", 1:tunnel_lengths[i])
    } else {
      names[i]
    }
  })

  # Flatten the list to a vector
  expanded_states <- unlist(expanded_states)

  # Create corresponding initial probabilities
  expanded_init_probs <- lapply(1:length(names), function(i) {
    if (tunnel_lengths[i] > 1) {
      c(init_probs[i], rep(0, tunnel_lengths[i] - 1))
    } else {
      init_probs[i]
    }
  })
  expanded_init_probs <- unlist(expanded_init_probs)

  # Create dt_curr_states with state2 column
  state2 <- sapply(1:length(expanded_states), function(i) {
    curr_state <- expanded_states[i]
    if (grepl("_tnl", curr_state)) {
      # Extract base state and tunnel number
      parts <- strsplit(curr_state, "_tnl")[[1]]
      base_state <- parts[1]
      tunnel_num <- as.numeric(parts[2])

      # Get the tunnel length for this state
      state_idx <- match(base_state, names)
      max_tunnel <- tunnel_lengths[state_idx]

      # If not at max tunnel length, increment tunnel number
      if (tunnel_num < max_tunnel) {
        paste0(base_state, "_tnl", tunnel_num + 1)
      } else {
        curr_state
      }
    } else {
      curr_state
    }
  })

  dt_curr_states <- data.frame(
    state = expanded_states,
    state2 = state2
  )

  dt_p0 <- data.frame(
    state = expanded_states,
    x = expanded_init_probs
  )

  return(list(
    type = "states",
    expanded_states = expanded_states,
    dt_p0 = dt_p0,
    dt_curr_states = dt_curr_states
  ))
}
