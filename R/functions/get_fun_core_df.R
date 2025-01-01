
# add a data.frame of core arguments to each function 

get_fun_core_df <- function(twig_funs, fun_args, core_args, arg_values){
     
    fun_core_df <- list()
    for (fun in twig_funs){
      sel_fun_args <- fun_args[[fun]]
      # this is to sort the arguments in the order of core_args
      sel_core_args <- core_args[core_args %in% sel_fun_args]

    # Extract values for sel_core_args from arg_values
      sel_arg_values <- arg_values[sel_core_args]
      
      # Create all possible permutations
      permutations <- expand.grid(sel_arg_values)
     if ("state" %in% sel_fun_args){
          # Split the expanded_state column into state and cycle_in_statef
          split_columns <- strsplit(as.character(permutations$state), "_tnl")
          permutations$state <- sapply(split_columns, `[`, 1)
          if ("cycle_in_state" %in% sel_fun_args){
            # browser()
            cycle_in_state_value <- as.numeric(sapply(split_columns, `[`, 2))
            cycle_in_state_value[is.na(cycle_in_state_value)] <- 1
          permutations$cycle_in_state <- cycle_in_state_value
          }
          #permutations$expanded_state <- NULL
      }
    fun_core_df[[fun]] <- permutations
    }
    return(fun_core_df)
}