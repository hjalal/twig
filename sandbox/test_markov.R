# test advanced markov model
#library(twig)
#library(magrittr)

# Define the number of samples

mytwig <- twig() + # for illustration it is 75 in the tutorial 
  decisions("StandardOfCare", "StrategyA", "StrategyB", "StrategyAB") + 
  states(names=c("H", "S1", "S2", "D"), 
         init_probs=c(1,0,0,0),
         tunnel_lengths=c(1,n_cycles,1,1)) + 
  event(name = "die",  
        scenarios = c("yes","none"), 
        probs = c("pDie", "#"), 
        goto = c("D", "get_event")) +  
  event(name = "get_event",  
        scenarios = c("recover", "getsick", "progress", "none"), 
        probs = c("pRecover", "pGetSick", "pProgress", "#"), 
        goto = c("H", "S1", "S2", "curr_state")) +  
  payoffs(names = c("cost", "utility"), discount_rates = c(0.05,0.05))

n_age_init <- 25 - 1 # age at baseline twig starts at cycle 1 instead of 0 in the tutorial
n_age_max  <- 100 # maximum age of follow up



## Age-dependent mortality rates ----
lt_usa_2015 <- as.data.table(read.csv("~/github/twig/inst/extdata/LifeTable_USA_Mx_2015.csv"))
#* Extract age-specific all-cause mortality for ages in model time horizon
# v_r_mort_by_age <- lt_usa_2015 %>% 
#   dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
#   dplyr::select(Total) %>%
#   as.matrix() # anyone above 100 have the same mortality

v_r_mort_by_age <- as.matrix(lt_usa_2015[Age >= n_age_init & Age < n_age_max, .(Total)])


params <- list(
  ### Transition rates (annual), and hazard ratios (HRs) ----
  r_HS1  = 0.15,  # constant annual rate of becoming Sick when Healthy
  r_S1H  = 0.5 ,  # constant annual rate of becoming Healthy when Sick
  hr_S1  = 3   ,  # hazard ratio of death in Sick vs Healthy
  hr_S2  = 10  ,  # hazard ratio of death in Sicker vs Healthy

  ### Effectiveness of treatment B ----
  hr_S1S2_trtB = 0.6,  # hazard ratio of becoming Sicker when Sick under treatment B

  #* Weibull parameters for state-residence-dependent transition probability of
  #* becoming Sicker when Sick conditional on surviving
  r_S1S2_scale = 0.08, # scale
  r_S1S2_shape = 1.1 , # shape

  ### State rewards ----
  #### Costs ----
  c_H    = 2000 , # annual cost of being Healthy
  c_S1   = 4000 , # annual cost of being Sick
  c_S2   = 15000, # annual cost of being Sicker
  c_D    = 0    , # annual cost of being dead
  c_trtA = 12000, # annual cost of receiving treatment A
  c_trtB = 13000, # annual cost of receiving treatment B
  #### Utilities ----
  u_H    = 1   ,  # annual utility of being Healthy
  u_S1   = 0.75,  # annual utility of being Sick
  u_S2   = 0.5 ,  # annual utility of being Sicker
  u_D    = 0   ,  # annual utility of being dead
  u_trtA = 0.95,  # annual utility when receiving treatment A

  ### Transition rewards ----
  du_HS1 = 0.01,  # disutility when transitioning from Healthy to Sick
  ic_HS1 = 1000,  # increase in cost when transitioning from Healthy to Sick
  ic_D   = 2000  # increase in cost when dying
)



# Create the data.table with random samples
# params <- data.table(
#   r_HS1         = rbeta(n_sims, 2, 10),             # Transition rate with beta distribution
#   r_S1H         = rbeta(n_sims, 5, 5),              # Another transition rate with a different shape
#   hr_S1         = rlnorm(n_sims, log(3), 0.2),      # Hazard ratio, log-normal to allow skewness
#   hr_S2         = rlnorm(n_sims, log(10), 0.2),     # Higher hazard ratio, same distribution
# 
#   hr_S1S2_trtB  = rbeta(n_sims, 6, 4),              # Hazard ratio under treatment with beta distribution
# 
#   r_S1S2_scale  = rgamma(n_sims, shape = 2, rate = 25), # Scale parameter, gamma distribution
#   r_S1S2_shape  = rgamma(n_sims, shape = 3, rate = 3),  # Shape parameter, gamma distribution
# 
#   c_H           = rnorm(n_sims, mean = 2000, sd = 50),   # Annual cost, slight variation for simulation
#   c_S1          = rnorm(n_sims, mean = 4000, sd = 100),  # Higher annual cost, slightly varied
#   c_S2          = rnorm(n_sims, mean = 15000, sd = 500), # Large cost with moderate variation
#   c_D           = 0,                                        # Constant, no variation
#   c_trtA        = rnorm(n_sims, mean = 12000, sd = 200), # Cost of treatment A with small variation
#   c_trtB        = rnorm(n_sims, mean = 13000, sd = 200), # Cost of treatment B
# 
#   u_H           = rbeta(n_sims, 10, 1),                  # Utility close to 1 for Healthy
#   u_S1          = rbeta(n_sims, 7.5, 2.5),               # Utility less than Healthy, beta distribution
#   u_S2          = rbeta(n_sims, 5, 5),                   # Utility for Sicker
#   u_D           = 0,                                        # Utility for Dead is constant
#   u_trtA        = rbeta(n_sims, 9.5, 1),                 # Utility with treatment A, close to Healthy
# 
#   du_HS1        = rnorm(n_sims, mean = 0.01, sd = 0.005), # Disutility with slight variation
#   ic_HS1        = rnorm(n_sims, mean = 1000, sd = 100),   # Cost increase with transition
#   ic_D          = rnorm(n_sims, mean = 2000, sd = 100)    # Cost increase when dying
# )

# Display the resulting data.table
print(params)

pRecover <- function(state, r_S1H){
  rRecover <- ifelse(state=="S1", r_S1H, 0)
  rate2prob(rRecover)
}

# pRecover <- function(state){
#   rRecover <- ifelse(state=="S1", 0.5, 0)
#   rate2prob(rRecover)
# }

pGetSick <- function(state, r_HS1){
  rGetSick <- ifelse(state=="H", r_HS1, 0)
  rate2prob(rGetSick)
}

pProgress <- function(state, decision, cycle_in_state,
                      hr_S1S2_trtB, r_S1S2_scale, r_S1S2_shape){
  
  rProgress<- ifelse(state=="S1",
    ifelse(decision %in% c("StrategyB", "StrategyAB"), hr_S1S2_trtB, 1) *
    
    ((cycle_in_state*r_S1S2_scale)^r_S1S2_shape - 
      ((cycle_in_state - 1)*r_S1S2_scale)^r_S1S2_shape)

    , 0) # else 0
  rate2prob(rProgress)
}

pDie <- function(state, cycle,
                 hr_S1, hr_S2){
  r_HD <- v_r_mort_by_age[cycle] # fixed_params
  rDie <- ifelse(state=="H", r_HD, 
          ifelse(state=="S1", r_HD*hr_S1, 
          ifelse(state=="S2", r_HD*hr_S2,
          ifelse(state=="D", 0, 0))))
  rate2prob(rDie)
}

cost <- function(state, decision, get_event, die, 
                 ic_HS1, ic_D, c_trtA, c_trtB, 
                 c_H, c_S1, c_S2, c_D){
  # cost of decision is only applied if the state is either S1 or S2
  trans_cost_getting_sick <- (get_event=="getsick")*ic_HS1 # increase in cost when transitioning from Healthy to Sick
  trans_cost_dying <- (die=="yes")*ic_D # increase in cost when dying
  
  c_decision <- ifelse(state %in% c("S1","S2"),
                       ifelse(decision=="StandardOfCare", 0, 
                       ifelse(decision=="StrategyA", c_trtA,
                       ifelse(decision=="StrategyB", c_trtB,
                       ifelse(decision=="StrategyAB", c_trtA+c_trtB, 0))))
  , 0)
  
  # cost of the state is a function of the state
  c_state <- ifelse(state=="H",c_H,
             ifelse(state=="S1", c_S1,
             ifelse(state=="S2", c_S2,
             ifelse(state=="D", c_D, 0))))
  # combine both
  return(c_decision + c_state + trans_cost_getting_sick + trans_cost_dying)
}

utility <- function(state, decision, get_event,
                    du_HS1, u_H, u_trtA, u_S1, u_S2, u_D){
  trans_util_getting_sick <- -du_HS1*(get_event=="getsick")
  u_state <- ifelse(state=="H", u_H,
             ifelse(state=="S1", ifelse(decision %in% c("StrategyA", "StrategyAB"), u_trtA, u_S1),
             ifelse(state=="S2", u_S2,
             ifelse(state=="D", u_D, 0))))
  return(u_state + trans_util_getting_sick)
}

#model_struc <- twig_gen_model_function(mytwig)
#model_struc

#source("../R/myfun.R")

#model_results <- my_markov_model(model_struc, params, return_trace = T)
#dim(model_results$Trace)
#model_results


