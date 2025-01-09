## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list = ls())
options(width = 200) 
library(twig)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mytwig <- twig() + 
  decisions(names=c(DoNotTreat, Treat, Biopsy)) +  # treatment options
  event(name = DIE,  # first event
        options = c(yes, none), # either occurs or doesn't occur
        probs = c(pDie, leftover),  # occurs with prob pDie and doesn't occur with 1-pDie (leftover)
        transitions = c(Death, HVE_event)) + # if it occurs, transitions to Death, otherwise the HVE_event
  event(name = HVE_event,  # similarly, HVE_event occurs with f_HVE but if not it will be OVE
        options = c(yes, none), 
        probs = c(f_HVE, leftover), 
        transitions = c(get_HVE_comp, get_OVE_comp)) +
  event(name = get_HVE_comp, # evaluate whether HVE complications occured 
        options = c(yes, none), 
        probs = c(p_comp, leftover),
        transitions = c(HVE_comp, no_HVE_comp))  +
  event(name = get_OVE_comp, # evaluate whether other viral encephalitis (OVE) complications occured
        options = c(yes, none), 
        probs = c(p_comp, leftover),
        transitions = c(OVE_comp, no_OVE_comp)) + 
  payoffs(names = c(cost, utility)) # finally measure the cost and utilities 


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
params <- list(
  # Probabilities,
  p_HVE          = 0.52   ,# prevalence of HVE
  p_HVE_comp     = 0.71   ,# complications with untreated HVE
  p_OVE_comp     = 0.01   ,# complications with untreated OVE
  p_HVE_comp_tx  = 0.36   ,# complications with treated HVE
  p_OVE_comp_tx  = 0.20   ,# complications with treated OVE
  p_biopsy_death = 0.005  ,# probability of death due to biopsy
  
  # Costs,
  c_VE           = 1200   ,# cost of viral encephalitis care without complications
  c_VE_comp      = 9000   ,# cost of viral encephalitis care with complications
  c_tx           = 9500   ,# cost of treatment
  c_biopsy       = 25000  ,# cost of brain biopsy
  
  # QALYs,
  q_VE           = 20     ,# remaining QALYs for those without VE-related complications
  q_VE_comp      = 19     ,# remaining QALYs for those with VE-related complications
  q_loss_biopsy  = 0.01   ,# one-time QALY loss due to brain biopsy
  q_death_biopsy = 0      # remaining QALYs for those who died during biopsy
)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pDie <- function(decision, p_biopsy_death){
  p_biopsy_death * (decision == "Biopsy")
}

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p_comp <- function(decision, HVE_event, p_HVE_comp, p_OVE_comp, 
                   p_HVE_comp_tx, p_OVE_comp_tx) {

    # complication of untreated HVE
    p_HVE_comp * (decision == "DoNotTreat" & HVE_event=="yes") + 

    # complication of untreated OVE
    p_OVE_comp * (decision %in% c("DoNotTreat", "Biopsy") & HVE_event=="none") + 

    # complications of treated HVE
    p_HVE_comp_tx * (decision %in% c("Treat", "Biopsy") & HVE_event=="yes") + 

    # complications of treated OVE
    p_OVE_comp_tx * (decision == "Treat" & HVE_event=="none")
}

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
f_HVE <- function(p_HVE){
  p_HVE
}

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cost <- function(decision, outcome, c_biopsy, c_tx, c_VE_comp, c_VE){ 

  # cost of biopsy
  c_biopsy*(decision=="Biopsy") + 

    # cost of treatment if treated or biopsy was +ve for HVE
    c_tx*(decision=="Treat" | (decision=="Biopsy" & outcome %in% c("HVE_comp", "no_HVE_comp"))) + 

    # cost of complication if outcomes are in either HVE or OVE complications
    c_VE_comp*(outcome %in% c("HVE_comp", "OVE_comp")) + 

    # cost of viral encephalitis if complications didn't occur
    c_VE*(outcome %in% c("no_HVE_comp", "no_OVE_comp")) 
}

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
utility <- function(decision, outcome, q_loss_biopsy, q_VE_comp, q_VE){

  # apply utility discount for biopsy
  -q_loss_biopsy*(decision=="Biopsy") + 

  # apply utility values for complications
    q_VE_comp*(outcome %in% c("HVE_comp", "OVE_comp")) + 

    # apply utility values if complications didn't occur
    q_VE*(outcome %in% c("no_HVE_comp", "no_OVE_comp"))
}

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
results <- run_twig(twig_obj = mytwig, params = params, parallel = FALSE, progress_bar = FALSE)

results$sim_ev

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
calculate_icers(results$mean_ev)

