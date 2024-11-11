
#remove.packages("twig")
#Sys.unsetenv("GITHUB_PAT")
#remotes::install_github("hjalal/twig", build_vignettes = FALSE, force = TRUE)

library(progress)
# ==============
#remove.packages("twig")

devtools::document(roclets = c('rd', 'collate', 'namespace'))
devtools::build(vignettes = FALSE)
# "/Users/hjalal/github/twig_0.0.1.0.tar.gz"

# global parameter 
rm(list = ls())

n_sims <- 1
n_cycles <- 10
default_event_scenario <- "none"
x_cols <- paste0("x_sim", 1:n_sims)

#library(data.table)
source("sandbox/test_dataset_join_example_events.R")
source("sandbox/test_dataset_join_funs.R")
library(data.table)
setDTthreads(5)
getDTthreads()
mytwig
# list2env(params, envir = .GlobalEnv)
list_fun_outputs <- twig_expand_functions(mytwig)
#str(list_fun_outputs)

fun_names <- list_fun_outputs$fun_names
arg_values <- list_fun_outputs$arg_values
fun_outputs <- list_fun_outputs$fun_outputs

#View(fun_outputs$pDie)
#unique_values <- get_unique_values(sel_fun_outputs)


events_df <- twig:::get_event_df(mytwig)
events_dt <- as.data.table(events_df)
#events_dt
all_events <- unique(events_dt$event)
twig_dims <- get_twig_dims(mytwig, events_dt, n_cycles, n_sims)
states_layers <- retrieve_layer_by_type(mytwig, type = "states") 
dt_curr_states <- states_layers$dt_curr_states
#mytwig


dt_prob_list <- get_seg_probs(events_dt, fun_outputs)

# get path data frame that lists the events and their outcomes
path_dt <- get_path_dt(events_dt, dt_curr_states)

# path level ======
dt_pathprob_list <- get_path_probs(path_dt, dt_prob_list, dt_curr_states)




# sum over all paths + curr_state =========
trans_probs <- smart_sum(dt_pathprob_list)

# Sum each of the x_sim columns by the other variables except 'state2'
sum_x_by_group <- trans_probs[, 
                 lapply(.SD, sum, na.rm = TRUE), 
                 .SDcols = x_cols, 
                 by = .(state, cycle, decision)  # Grouping by other variables except state2
]
#View(sum_x_by_group)

# Create trace =======


p0 <- get_dt_p0(states_layers, n_sims, x_cols)


Trace <- get_trace(trans_probs, p0, n_cycles, x_cols)

head(Trace)

# apply rewards ========
pyaoffs_layer <- retrieve_layer_by_type(mytwig, type = "payoffs") 
dt_curr_states <- states_layers$dt_curr_states
fun_outputs$cost
fun_outputs$utility

