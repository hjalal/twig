
#remove.packages("twig")
#Sys.unsetenv("GITHUB_PAT")
#remotes::install_github("hjalal/twig", build_vignettes = FALSE, force = TRUE)


# ==============
#remove.packages("twig")

devtools::document(roclets = c('rd', 'collate', 'namespace'))
devtools::build(vignettes = FALSE)
# "/Users/hjalal/github/twig_0.0.1.0.tar.gz"

#library(data.table)
source("sandbox/test_dataset_join_example.R")
source("sandbox/test_dataset_join_funs.R")
library(data.table)
mytwig
# list2env(params, envir = .GlobalEnv)
list_fun_outputs <- twig_expand_functions(mytwig, 
                                          params = params)
str(list_fun_outputs)

fun_names <- list_fun_outputs$fun_names
arg_values <- list_fun_outputs$arg_values
fun_outputs <- list_fun_outputs$fun_outputs



#unique_values <- get_unique_values(sel_fun_outputs)


events_df <- twig:::get_event_df(mytwig)
events_dt <- as.data.table(events_df)
events_dt

states_layers <- retrieve_layer_by_type(mytwig, type = "states") 
dt_curr_states <- states_layers$dt_curr_states
mytwig


dt_prob_list <- get_seg_probs(events_dt, fun_outputs)

# path level ======
dt_pathprob_list <- get_path_probs(events_dt, dt_prob_list, dt_curr_states)

# sum over all paths + curr_state =========
trans_probs <- smart_sum(dt_pathprob_list)

sum_x_by_group <- trans_probs[, .(sum_x = sum(x, na.rm = TRUE)), by = .(sim, state, cycle, decision)]
sum_x_by_group

# Create trace =======
p0 <- states_layers$dt_p0
n_cycles
j <- 1
list_dt_trace <- list()
for (j in 1:n_cycles){
  P <- trans_probs[cycle==j,] 
  if (j == 1) p <- p0
  p <- smart_prod(list(P, p))
  # Remove the original 'state' column
  p[, state := NULL]
  # Rename 'Y' to 'state'
  setnames(p, "Y", "state")
  p <- p[, .(x = sum(x, na.rm = TRUE)), by = setdiff(names(p), "x")]
  list_dt_trace[[j]] <- copy(p)
  p[, cycle:=cycle+1]
}
Trace <- rbindlist(list_dt_trace)


# apply rewards ========


