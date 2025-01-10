pkgname <- "twig"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "twig-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('twig')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("calculate_icers")
### * calculate_icers

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calculate_icers
### Title: Calculate Incremental Cost-Effectiveness Ratios (ICERs)
### Aliases: calculate_icers

### ** Examples

payoffs_summary <- matrix(c(100, 200, 0.5, 0.7), ncol = 2, 
                          dimnames = list(c("Strategy A", "Strategy B"), 
                                          c("cost", "utility")))
calculate_icers(payoffs_summary)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calculate_icers", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("decisions")
### * decisions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: decisions
### Title: Add decisions to a twig
### Aliases: decisions

### ** Examples

decisions(names = c(A, B, C))
decisions(names = c("A", "B", "C"))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("decisions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("event")
### * event

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: event
### Title: Add an event layer to a twig object
### Aliases: event

### ** Examples

#' # Adding the event layer to a twig object
twig_obj <- twig() + event(name = event_progress, 
                           options = c(yes, none), 
                           probs = c(pProgress, leftover), 
                           transitions = c(Severe, stay))

event_layer <- event(name = "event_progress", 
                     options = c("yes", "none"), 
                     probs = c(pProgress, leftover), 
                     transitions = c("Severe", "stay"))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("event", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("grapes-out-grapes")
### * grapes-out-grapes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: %out%
### Title: Negation of %in% operator
### Aliases: %out%

### ** Examples

x <- c("A", "B", "C")
table <- c("B", "C", "D")
x %out% table



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("grapes-out-grapes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("payoffs")
### * payoffs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: payoffs
### Title: Add payoffs to a twig object
### Aliases: payoffs

### ** Examples

payoffs_layer <- payoffs(names = c(cost, effectiveness), discount_rates = c(0.03, 0.03))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("payoffs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_ceac")
### * plot_ceac

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_ceac
### Title: Plot Cost-Effectiveness Acceptability Curve (CEAC)
### Aliases: plot_ceac

### ** Examples

# Example payoffs simulation array
payoffs_sim <- array(
  data = c(1000, 2000, 1500, 0.8, 0.85, 0.82, 1000, 2000, 1500, 0.8, 0.85, 0.82),
  dim = c(3, 2, 2),
  dimnames = list(c("StrategyA", "StrategyB", "StrategyC"), c("cost", "utility"), NULL)
)

# Define WTP range
wtp_range <- seq(0, 100000, by = 1000)

# Plot CEAC
ceac_plot <- plot_ceac(payoffs_sim, wtp_range)
print(ceac_plot)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_ceac", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plus-.twig_class")
### * plus-.twig_class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: +.twig_class
### Title: Define a method for the '+' operator for 'twig' objects
### Aliases: +.twig_class

### ** Examples

twig_obj <- twig() + 
  decisions(names = c(StandardOfCare, StrategyA, StrategyB, StrategyAB))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plus-.twig_class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("prob2rate")
### * prob2rate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: prob2rate
### Title: Convert Probability to Rate
### Aliases: prob2rate

### ** Examples

prob <- 0.1
rate <- prob2rate(prob)
print(rate)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("prob2rate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rate2prob")
### * rate2prob

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rate2prob
### Title: Convert Rate to Probability
### Aliases: rate2prob

### ** Examples

rate <- 0.1
prob <- rate2prob(rate)
print(prob)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rate2prob", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("run_twig")
### * run_twig

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: run_twig
### Title: Run a twig model
### Aliases: run_twig

### ** Examples

## Not run: 
##D run_twig(mytwig, params, n_cycles = 10)
##D # see the vignettes for more examples
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("run_twig", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("states")
### * states

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: states
### Title: Add Markov states to a twig
### Aliases: states

### ** Examples

states(names = c(H,S,D), 
                 init_probs = c(0.5, prob_fun, leftover),
                 max_cycles = c(1, 2, 1)) 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("states", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("twig")
### * twig

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: twig
### Title: Create a new twig object
### Aliases: twig

### ** Examples

twig_obj <- twig()
# see vignettes for more



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("twig", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
