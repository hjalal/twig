![](man/figures/hex_sticker.png)

# `twig`

## Documentation and Tutorials

**twig** is an R package for building Markov models and decision trees for Medical Decision Making and cost-effectiveness analyses. An online graphical user interface is available at [DecisionTwig](https://www.dashlab.ca/projects/decision_twig/) to interactively build the twig syntax. 

## Installation

A CRAN version will soon be available. 

To install **twig** from GitHub, use the following command in R:

``` r
library(devtools)
install_github("hjalal/twig")
```

## Overview

`twig` streamlines the process of building models by defining a Grammar of Modeling similar to the Grammar of Graphics and the and the `ggplot2` package. A `twig` syntax typically consists of

``` r
mymodel <- twig() + 
  decisions() + 
  states() + 
  event() + 
  event() + 
  ... 
  payoffs()
```

This will generate a `twig` object with 1 `decisions` layer, 1 `states` layer, one `payoff` layer, and 1 or more `event` layers.


## Minimal example:

Consider this `twig`:

``` r
mytwig <- twig() + 
  decisions(A,B) + # decision alternatives
  states(names=c(Alive,Dead), # Markov state names
         init_probs=c(1,0)) + # The cohort starts healthy
  event(name=death_event, # A death event can occur with 
      options=c(yes,none),  # 2 options "yes" and "none",
      probs=c(pDie,leftover), # that can occur with probabilities pDie and 1-pDie, 
      transitions=c(Dead,stay)) + # can lead to death state otherwise stay in their current state
  payoffs(names = c(cost, utility))  # we measure the cost and utility of the cohort
```
We can build the twig interactively using [DecisionTwig](https://www.dashlab.ca/projects/decision_twig/)

![](man/figures/decision_twig_demo2.png)

Next, we define the three functions that we use in the twig: `pDie`, `cost` and `utility`:

``` r
# probability of death is a function of the cohort's age (cycle) and can only occur if someone is alive
pDie <- function(state, rrMortA){
  rDie <- cycle * rrMortA * (state=="Alive") # rate of probability increases at 0.01 per cycle (year)
  rate2prob(rDie) # convert the rate into probability
}

# cost is a function of the decision
cost <- function(decision, cA, cB){
  cA * (decision=="A") + 
  cB * (decision=="B")
}

# utility is uAlive if alive, otherwise 0
utility <- function(state, uAlive){
  uAlive * (state=="Alive")
}
``` 

Then, we can define our parameters as a probabilistic dataset of the parameters:

```r
r_sims <- 1000

psa_params <- data.frame(
  rrMortA = rnorm(n_sims, 0.01, 0.001),
  cA = rlnorm(n_sims, 10, 1),
  cB = rlnorm(n_sims, 12, 1),
  uAlive = rbeta(0.8, 0.2))

head(psa_params)

      rrMortA        cA        cB    uAlive
1 0.009240403 36434.444 150835.86 0.1819717
2 0.012729583 24576.010  89905.15 0.6901971
3 0.010743772  7363.222 127403.07 0.1229560
4 0.009765270 14057.464 119454.46 0.9619981
5 0.010409442 34011.326  37668.27 0.9956655
6 0.009722109 18366.484 124804.39 0.9141599
``` 

Finally, we run the model for 50 cycles (years):
``` r 
results <- run_twig(twig_obj = mytwig, params = psa_params, n_cycles = 50


results$Rewards_summary
```



a simple Markov model with 2 decisions: StandardofCare, and NewTreatment, and two health states: Healthy and Dead. The cohort starts at the healthy state each year there is a 0.1 probability of death.

First, we define the **generic cycle tree**, which is at the core of the Grammar of Modeling.

![[DecisionTwig](https://www.dashlab.ca/projects/decision_twig/)](man/figures/decision_twig_demo.png){width="400"}

This same generic cycle tree is applied to both health states ("Healthy" and "Dead"). For example, in each cycle a proportion of the cohort that is healthy, will die. This will be determined by the function `pDie(state="Healthy")`. The rest will stay healthy computed by the infinity `Inf` placeholder which `twig` will translate to `1-pDie(state="Healthy")`. The proportion that remains healthy handled by the special health state `stay`. Likewise, this cycle tree will also be applied to the proportion of the cohort that is already dead. But, in this case, none of the cohort will die because `pDie(state="Dead")` returns 0.

The twig syntax for this generic cycle tree will consist of a single `event` layer:

``` r
event(name="die", 
      options=c("Yes","No"), 
      probs=c(pDie(decision, state),Inf), 
      transitions=c("Dead","stay"))
```

and we can define `pDie` like a standard `R` function. Here we assume that `NewTreatment` reduces probability of death from 0.2 to 0.1:

``` r
pDie <- function(decision, state){
  if(state == "Healthy"){
    if (decision == "NewTreatment") 0.1 else 0.2
  } else {
    0
  }
}
```

Similarly, we can define a function `compute_cost` which we will set to return \$1000 for `NewTreatment`, and 0 for `StandardOfCare`:

``` r
compute_cost <- function(decision){
  if (decision=="NewTreatment") 1000 else 0
}
```

The code below shows the full `twig` syntax with the two functions:

``` r


# probabiltiy of death
pDie <- function(decision, state){
  if(state == "Healthy"){
    if (decision == "NewTreatment") 0.1 else 0.2
  } else {
    0
  }
}
# cost payoff
compute_cost <- function(decision){
  if (decision=="NewTreatment") 1000 else 0
}
```

To confirm that the functions are behaving as expected, it is always good to check them. the function `twig_expand_functions(twig_obj)` iterates through the functions and dependencies and produces a dataset for each function with the values.

``` r
twig_expand_functions(twig_obj)
# Note: The dataset  df_compute_cost  created for function  compute_cost .
# Note: The dataset  df_pDie  created for function  pDie .

# df_pDie
#         decision   state pDie
# 1 StandardOfCare Healthy  0.2
# 2   NewTreatment Healthy  0.1
# 3 StandardOfCare    Dead  0.0
# 4   NewTreatment    Dead  0.0

# df_compute_cost
#         decision compute_cost
# 1 StandardOfCare            0
# 2   NewTreatment         1000
```

`pDie` and `compute_cost` are behaving as expected.

## Running the model

First, we convert the `twig` syntax to a standard R function, using `twig_gen_model_function()`, then we define the n_cycles variable, and run the generated model function, which by default it will be named `my_markov_model`

``` r
twig_obj <- twig_gen_model_function(twig_obj) 
n_cycles <- 5
my_markov_model(twig_obj)
# $summary_payoffs
#                cost
# StandardOfCare    0
# NewTreatment   5000
```

As expected for the 5 cycles, cost of `NewTreatment` is \$5000. We can also produce the transition probability matrix `P` and the Markov trace `Trace`:

``` r
my_markov_model(twig_obj, return_transition_prob = T, return_trace = T)
# $P
# , , StandardOfCare
# 
#         Healthy Dead
# Healthy     0.8  0.2
# Dead        0.0  1.0
# 
# , , NewTreatment
# 
#         Healthy Dead
# Healthy     0.9  0.1
# Dead        0.0  1.0
# 
# 
# $Trace
# , , StandardOfCare
# 
#   Healthy   Dead
# 1  1.0000 0.0000
# 2  0.8000 0.2000
# 3  0.6400 0.3600
# 4  0.5120 0.4880
# 5  0.4096 0.5904
# 
# , , NewTreatment
# 
#   Healthy   Dead
# 1  1.0000 0.0000
# 2  0.9000 0.1000
# 3  0.8100 0.1900
# 4  0.7290 0.2710
# 5  0.6561 0.3439
# 
# 
# $summary_payoffs
#                cost
# StandardOfCare    0
# NewTreatment   5000
```

## Additional features of `twig`

currently, `twig` can support these features:

-   simulation time dependency (e.g., age dependency)

-   state residency dependency (e.g., tunnel states)

-   multiple payoffs (e.g., cost, effectiveness, life expectancy, ... etc)

-   multiple events in each cycle

-   transition rewards (e.g., transitional cost or disutility)

-   discounting

Explore **twig** capabilities by reviewing the vignettes in the [**Articles**](https://hjalal.github.io/twig/) section. Currently, there are 8 vignettes, 4 for decision trees D0-D3, and 4 for Markov models M0-M3. The vignettes are labelled from 0 to 3 going from beginner to advanced.

## Disclaimer

Please note that both **Decision Twigs** and **twig** are still under active development and are provided as-is without any warranty.

## License

This project is licensed under the GPL v3 International.

## Suggested citations:

Jalal, H. (2024). Grammar of Modelling, twig R package. Retrieved from <https://github.com/hjalal/twig>

Jalal, H. (2024). DecisionTwig. Retrieved from <https://www.dashlab.ca/projects/decision_twig/>
