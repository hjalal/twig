

# test advanced markov model
#library(twig)
#library(magrittr)

# Define the number of samples
twig_obj <- twig() + 
  decisions("BrainBiopsy", "TreatAll", "TreatNone") + 

  event(name = "BiopSeq",  
                options = c("die", "sev", "mod", "none"), 
                probs = c(fDieBiop, fSevBiop, fModBiop, leftover), 
                transitions = c("DEAD", "HSE", "HSE", "HSE"))  + 

  event(name = "HSE",  
                options = c("yes", "none"), 
                probs = c(fHSE, leftover), 
                transitions = c("BiopRes", "BiopRes"))  +

  event(name = "BiopRes",  
                options = c("pos", "none"), 
                probs = c(fBiopRes, leftover), 
                transitions = c("dieHSE", "dieHSE"))  + 

  event(name = "dieHSE",  
                options = c("yes", "none"), 
                probs = c(fDieHSE, leftover), 
                transitions = c("DEAD", "sevHSE")) +

  event(name = "sevHSE",  
                options = c("yes", "none"), 
                probs = c(fSevSeqHSE, leftover), 
                transitions = c("SEVHSESEQ", "modHSE")) +

  event(name = "modHSE",  
                options = c("yes", "none"), 
                probs = c(fModSeqHSE, leftover), 
                transitions = c("MODHSESEQ", "MLDHSESEQ")) +

  payoffs(names = "util")

fDieBiop <- function(decision, pDieBiopsy){
  pDieBiopsy * (decision=="BrainBiopsy")
}
fSevBiop <- function(decision, pDieBiopsy, pSevBiopsy){
  pSevBiopsy * (1 - pDieBiopsy) * (decision=="BrainBiopsy")
}
fModBiop <- function(decision, pDieBiopsy, pSevBiopsy, pModBiopsy){
  pModBiopsy * (1 - pSevBiopsy) * (1 - pDieBiopsy) * (decision=="BrainBiopsy")
}

# allow twig to take in global variables and numbers just like initial probs in decsion trees
fHSE <- function(pHSE){
    pHSE
}

fDieHSE <- function(HSE, decision, BiopRes, pDieHSE, pDieNoHSE, fDie, addProbDie){
  pEvent(HSE, decision, BiopRes, pDieHSE, pDieNoHSE, fDie, addProbDie)
}
fSevSeqHSE <- function(HSE, decision, BiopRes, pSevHSE, pSevNoHSE, fSev, addProbSev){
  pEvent(HSE, decision, BiopRes, pSevHSE, pSevNoHSE, fSev, addProbSev)
}
fModSeqHSE <- function(HSE, decision, BiopRes, pModHSE, pModNoHSE, fMod, addProbMod){
  pEvent(HSE, decision, BiopRes, pModHSE, pModNoHSE, fMod, addProbMod)
}

pEvent <- function(HSE, decision, BiopRes, pEventHSE, pEventNoHSE, fEvent, addProbEvent){

  pEventRx <- (1-fEvent)*pEventHSE*(HSE=="yes") + 
              (pEventNoHSE+addProbEvent-pEventNoHSE*addProbEvent)*pEventNoHSE*(HSE=="none") 

  pEventNoRx <- pEventHSE * (HSE=="yes") + 
                pEventNoHSE * (HSE=="none")

    pEventNoRx * (decision=="TreatNone" | (decision=="BrainBiopsy" & BiopRes=="none")) +
    pEventRx * (decision=="TreatAll" | (decision=="BrainBiopsy" & BiopRes=="yes")) 
}

fBiopRes <- function(decision, HSE, sensBiopsy, specBiopsy){
  ((HSE=="yes")*sensBiopsy + 
  (HSE=="none")*(1-specBiopsy)) * 
  (decision=="BrainBiopsy") 
}


util <- function(decision, outcome, BiopSeq, HSE, BiopRes, uDie, uSev, uMod, uMld){
    uMult <- (uSev * (BiopSeq=="sev") + 
             uMod * (BiopSeq=="mod") + 
             uMld * (BiopSeq=="mld")) ^ 
             (decision=="BrainBiopsy") 

    (uDie * (outcome=="DEAD") + 
    uSev * (outcome=="SEVHSESEQ") +
    uMod * (outcome=="MODHSESEQ") +
    uMld * (outcome=="MLDHSESEQ")) * uMult

}



params <- list(
    pDieBiopsy = 0.004,
    pSevBiopsy = 0.01,
    pModBiopsy = 0.03,
    sensBiopsy = 0.95,
    specBiopsy = 0.99,
    pHSE = 0.4, #overall

    pDieHSE = .7,
    pSevHSE = .333,
    pModHSE = .5,

    fDie = .37,
    fSev = .2,
    fMod = .2,

    pDieNoHSE = .18,
    pSevNoHSE = .122,
    pModNoHSE = .139,

    addProbDie = .004,
    addProbSev = .01,
    addProbMod = .02,

    uDie = 0,
    uSev = 0.02,
    uMod = .8,
    uMld = 1
)


# # Create the data.table with random samples
# params <- data.frame(
#   r_HS1         = rbeta(n_sims, 2, 10),             # Transition rate with beta distribution
#   r_S1H         = rbeta(n_sims, 5, 5),              # Another transition rate with a different shape
#   hr_S1         = rlnorm(n_sims, log(3), 0.2),      # Hazard ratio, log-normal to allow skewness
#   hr_S2         = rlnorm(n_sims, log(10), 0.2),     # Higher hazard ratio, same distribution

#   hr_S1S2_trtB  = rbeta(n_sims, 6, 4),              # Hazard ratio under treatment with beta distribution

#   r_S1S2_scale  = rgamma(n_sims, shape = 2, rate = 25), # Scale parameter, gamma distribution
#   r_S1S2_shape  = rgamma(n_sims, shape = 3, rate = 3),  # Shape parameter, gamma distribution

#   c_H           = rnorm(n_sims, mean = 2000, sd = 50),   # Annual cost, slight variation for simulation
#   c_S1          = rnorm(n_sims, mean = 4000, sd = 100),  # Higher annual cost, slightly varied
#   c_S2          = rnorm(n_sims, mean = 15000, sd = 500), # Large cost with moderate variation
#   c_D           = 0,                                        # Constant, no variation
#   c_trtA        = rnorm(n_sims, mean = 12000, sd = 200), # Cost of treatment A with small variation
#   c_trtB        = rnorm(n_sims, mean = 13000, sd = 200), # Cost of treatment B

#   u_H           = rbeta(n_sims, 10, 1),                  # Utility close to 1 for Healthy
#   u_S1          = rbeta(n_sims, 7.5, 2.5),               # Utility less than Healthy, beta distribution
#   u_S2          = rbeta(n_sims, 5, 5),                   # Utility for Sicker
#   u_D           = 0,                                        # Utility for Dead is constant
#   u_trtA        = rbeta(n_sims, 9.5, 1),                 # Utility with treatment A, close to Healthy

#   du_HS1        = rnorm(n_sims, mean = 0.01, sd = 0.005), # Disutility with slight variation
#   ic_HS1        = rnorm(n_sims, mean = 1000, sd = 100),   # Cost increase with transition
#   ic_D          = rnorm(n_sims, mean = 2000, sd = 100),    # Cost increase when dying
#   p0_H          = rbeta(n_sims, 1, 9)                   # Initial probability of being Healthy
# )

