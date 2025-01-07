v_names_str    <- c("No Tx", "Tx All", "Biopsy")  # names of strategies
n_str          <- length(v_names_str)             # number of strategies
wtp            <- 100000                          # willingness to pay threshold

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
list2env(params, envir = .GlobalEnv)
# Create vector of weights for each strategy 
  
v_w_no_tx  <- c(    p_HVE  *      p_HVE_comp     ,  # HVE, complications
                    p_HVE  * (1 - p_HVE_comp)    ,  # HVE, no complications
               (1 - p_HVE) *      p_OVE_comp     ,  # OVE, complications
               (1 - p_HVE) * (1 - p_OVE_comp))      # OVE, no complications
  
v_w_tx     <- c(    p_HVE  *      p_HVE_comp_tx  ,  # HVE w/tx, complications
                    p_HVE  * (1 - p_HVE_comp_tx) ,  # HVE w/tx, no complications
               (1 - p_HVE) *      p_OVE_comp_tx  ,  # OVE w/tx, complications
               (1 - p_HVE) * (1 - p_OVE_comp_tx))   # OVE w/tx, no complications
  
v_w_biopsy <- c(p_biopsy_death                   ,  # biopsy death
               # no biopsy death.,   HVE w/tx,        complications
               (1-p_biopsy_death)   *    p_HVE  *    p_HVE_comp_tx  ,  
               # no biopsy death.,   HVE w/tx,     no complications
               (1-p_biopsy_death)   *    p_HVE  * (1-p_HVE_comp_tx) ,  
               # no biopsy death.,        OVE,        complications
               (1-p_biopsy_death)   * (1-p_HVE) *      p_OVE_comp   ,  
               # no biopsy death.,        OVE,     no complications
               (1-p_biopsy_death)   * (1-p_HVE) * (1 - p_OVE_comp))      
  
# Create vector of final_outcomes (QALYs) for each strategy 
  
v_qaly_no_tx  <- c(q_VE_comp ,          # HVE, complications
                   q_VE      ,          # HVE, no complications
                   q_VE_comp ,          # OVE, complications
                   q_VE)                # OVE, no complications
  
v_qaly_tx     <- c(q_VE_comp ,          # HVE, complications
                   q_VE      ,          # HVE, no complications
                   q_VE_comp ,          # OVE, complications
                   q_VE)                # OVE, no complications
  
  
v_qaly_biopsy <- -q_loss_biopsy     +   # loss due to biopsy
                  c(q_death_biopsy  ,   # biopsy complications
                    q_VE_comp       ,   # no biopsy comp., HVE w/tx, complications 
                    q_VE            ,   # no biopsy comp., HVE w/tx, no complications
                    q_VE_comp       ,   # no biopsy comp., OVE, complications
                    q_VE)               # no biopsy comp., OVE, no complications
  
# Create vector of costs for each strategy 
  
v_cost_no_tx  <- c(c_VE_comp ,          # HVE, complications
                   c_VE      ,          # HVE, no complications
                   c_VE_comp ,          # OVE, complications
                   c_VE)                # OVE, no complications
  
v_cost_tx     <- c_tx +                 # cost of treatment
                 c(c_VE_comp ,          # HVE, complications
                   c_VE      ,          # HVE, no complications
                   c_VE_comp ,          # OVE, complications
                   c_VE)                # OVE, no complications
  
v_cost_biopsy <- c_biopsy           +   # cost of biopsy procedure
                 c(0                ,   # cost of death (zero)
                   c_VE_comp + c_tx ,   # no biopsy comp., HVE w/tx, complications 
                   c_VE + c_tx      ,   # no biopsy comp., HVE w/tx, no complications
                   c_VE_comp        ,   # no biopsy comp., OVE, complications
                   c_VE)                # no biopsy comp., OVE, no complications
 
# Calculate total utilities for each strategy 
total_qaly_no_tx  <- v_w_no_tx  %*%  v_qaly_no_tx      
total_qaly_tx     <- v_w_tx     %*%  v_qaly_tx
total_qaly_biopsy <- v_w_biopsy %*%  v_qaly_biopsy
  
# Calculate total costs for each strategy 
total_cost_no_tx  <- v_w_no_tx  %*%  v_cost_no_tx    
total_cost_tx     <- v_w_tx     %*%  v_cost_tx
total_cost_biopsy <- v_w_biopsy %*%  v_cost_biopsy
  
# vector of total QALYs
v_total_qaly <- c(total_qaly_no_tx, total_qaly_tx, total_qaly_biopsy) 
# vector of total costs
v_total_cost <- c(total_cost_no_tx, total_cost_tx, total_cost_biopsy) 

  
# Name final_outcomes
names(v_total_qaly) <- v_names_str  # names for the elements of the total QALYs vector
names(v_total_cost) <- v_names_str  # names for the elements of the total cost vector
  
df_output <- data.frame(Strategy =  v_names_str,
                        Cost     =  v_total_cost,
                        Effect   =  v_total_qaly)

# model output
df_output
