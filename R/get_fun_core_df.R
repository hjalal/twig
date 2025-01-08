


get_fun_core_df <- function(twig_funs, fun_args, core_args, arg_values){
     
    fun_core_df <- list()
    for (fun in twig_funs){
      
      sel_fun_args <- fun_args[[fun]]
      
      sel_core_args <- core_args[core_args %in% sel_fun_args]

    
      sel_arg_values <- arg_values[sel_core_args]
      
      
      permutations <- expand.grid(sel_arg_values)
     if ("state" %in% sel_core_args){
          
          split_columns <- strsplit(as.character(permutations$state), "_tnl")
          permutations$state <- sapply(split_columns, `[`, 1)
          if ("cycle_in_state" %in% sel_fun_args){
            
            cycle_in_state_value <- as.numeric(sapply(split_columns, `[`, 2))
            cycle_in_state_value[is.na(cycle_in_state_value)] <- 1
          permutations$cycle_in_state <- cycle_in_state_value
          }
          
      }
    fun_core_df[[fun]] <- permutations
    }
    return(fun_core_df)
}