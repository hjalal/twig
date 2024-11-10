
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
setDTthreads(9)
getDTthreads()
mytwig
# list2env(params, envir = .GlobalEnv)
list_fun_outputs <- twig_expand_functions(mytwig, 
                                          params = params)
str(list_fun_outputs)

fun_names <- list_fun_outputs$fun_names
arg_values <- list_fun_outputs$arg_values
fun_outputs <- list_fun_outputs$fun_outputs


#View(fun_outputs$pDie)
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

# Sum each of the x_sim columns by the other variables except 'state2'
sum_x_by_group <- trans_probs[, 
                 lapply(.SD, sum, na.rm = TRUE), 
                 .SDcols = c("x_sim1", "x_sim2", "x_sim3"), 
                 by = .(state, cycle, decision)  # Grouping by other variables except state2
]
View(sum_x_by_group)

# Create trace =======
p0 <- states_layers$dt_p0
x_cols <- paste0("x_sim", 1:n_sims)

# Repeat 'x' for the number of simulations
# Create new columns 'x_sim1', 'x_sim2', ..., 'x_simN' by repeating 'x'
p0[, (x_cols) := lapply(1:n_sims, function(i) rep(x, length.out = .N))]
p0[, x := NULL]
p0
n_cycles
j <- 1
list_dt_trace <- list()
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
  p <- p[, lapply(.SD, sum, na.rm = TRUE), .SDcols = x_cols, by = setdiff(names(p), x_cols)]
  
  list_dt_trace[[j]] <- copy(p)
  p[, cycle:=cycle+1]
}
Trace <- rbindlist(list_dt_trace)


# apply rewards ========


