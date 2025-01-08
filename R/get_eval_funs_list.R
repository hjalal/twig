

get_eval_funs_list <- function(eval_funs, fun_core_df, twig_funs) {
  eval_funs_list <- list()
  for (fun in twig_funs) {
    if (nrow(fun_core_df[[fun]]) == 0) {
      eval_funs_list[[fun]] <- eval_funs[[fun]]
    } else {
      eval_funs_list[[fun]] <- cbind(fun_core_df[[fun]], eval_funs[fun])
    }
  }
  return(eval_funs_list)
}
