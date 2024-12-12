
## ----setup--------------------------------------------------------------------
rm(list = ls())
library(twig)

## -----------------------------------------------------------------------------
twig_env <- twig() + 
  decisions("DoNotTreat", "Treat", "Biopsy") + 
  #final_outcomes("Death", "HVE_comp", "no_HVE_comp", "OVE_comp", "no_OVE_comp") +
  #events("DIE", "HVE","get_comp") + 
  event(name = "DIE",  
        scenarios = c("yes", "none"), 
        probs = c("pDie", "#"), 
        goto = c("Death", "HVE_event")) + 
  event(name = "HVE_event",  
        scenarios = c("yes", "none"), 
        probs = c("f_HVE", "#"), 
        goto = c("get_HVE_comp", "get_OVE_comp")) +
  event(name = "get_HVE_comp", 
        scenarios = c("yes", "none"), 
        probs = c("p_comp", "#"),
        goto = c("HVE_comp", "no_HVE_comp"))  +
  event(name = "get_OVE_comp", 
        scenarios = c("yes", "none"), 
        probs = c("p_comp", "#"),
        goto = c("OVE_comp", "no_OVE_comp")) + 
  #payoffs(cost = cost(decision, final_outcome, prop_with_event("HVE"=TRUE, decision)), 
  payoffs(names = c("cost", "effectiveness"))


## -----------------------------------------------------------------------------
v_names_str    <- c("No Tx", "Tx All", "Biopsy")  # names of strategies
n_str          <- length(v_names_str)             # number of strategies
wtp            <- 100000                          # willingness to pay threshold

params <- list(
  wtp            = 100000 ,                         # willingness to pay threshold
  
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


## -----------------------------------------------------------------------------
f_HVE <- function(p_HVE){
  p_HVE
}

pDie <- function(decision, p_biopsy_death){
  ifelse(decision == "Biopsy", p_biopsy_death, 0)
}

p_comp <- function(decision, HVE_event, p_HVE_comp, p_OVE_comp, 
                   p_HVE_comp_tx, p_OVE_comp_tx) {
  ifelse(decision == "DoNotTreat" & HVE_event=="yes", p_HVE_comp,
         ifelse(decision == "DoNotTreat" & HVE_event=="none", p_OVE_comp,
                ifelse(decision == "Treat" & HVE_event=="yes", p_HVE_comp_tx,
                       ifelse(decision == "Treat" & HVE_event=="none", p_OVE_comp_tx,
                              ifelse(decision == "Biopsy" & HVE_event=="yes", p_HVE_comp_tx, 
                                     p_OVE_comp)))))
}

c_HVE <- function(decision,c_tx){
  ifelse(decision == "biopsy", c_tx, 0)
}

cost <- function(decision, outcome, c_biopsy, c_tx, c_VE_comp, c_VE){ 
  c_biopsy*(decision=="Biopsy") + 
    c_tx*(decision=="Treat" | (decision=="Biopsy" & outcome %in% c("HVE_comp", "no_HVE_comp"))) + 
    c_VE_comp*(outcome %in% c("HVE_comp", "OVE_comp")) + 
    c_VE*(outcome %in% c("no_HVE_comp", "no_OVE_comp")) 
}

effectiveness <- function(decision, outcome, q_loss_biopsy, q_VE_comp, q_VE){
  -q_loss_biopsy*(decision=="Biopsy") + 
    q_VE_comp*(outcome %in% c("HVE_comp", "OVE_comp")) + 
    q_VE*(outcome %in% c("no_HVE_comp", "no_OVE_comp"))
}

