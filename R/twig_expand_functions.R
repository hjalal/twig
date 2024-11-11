# generate probs and payoffs excel file and datasets
#' Title
#'
#' @param twig_obj 
#' @param fun_names 
#' @param excel_file_name 
#'
#' @return 
#' @export
#'
#' @examples 
#' 
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
    # Rename columns to include prefix "x_sim"
    setnames(values_dt_wide, old = as.character(unique(values_dt$sim)), new = paste0("x_sim", unique(values_dt$sim)))
    

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