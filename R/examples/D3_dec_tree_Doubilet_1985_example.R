

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
twig_obj <- twig() + 
  decisions(names=c("BrainBiopsy", "TreatAll", "TreatNone")) + 

  event(name = "Biopsy",  
        options = c("yes", "none"), 
        probabilities = c(pBiopsy, leftover), 
        transitions = c("dieBiop", "HSE"))  + 

  event(name = "dieBiop",  
        options = c("yes", "none"), 
        probabilities = c(fDieBiopsy, leftover), 
        transitions = c("DEAD", "sevBiopSeq"))  + 

  event(name = "sevBiopSeq",  
        options = c("yes", "none"), 
        probabilities = c(fSevBiopsy, leftover), 
        transitions = c("HSE", "modBiopSeq"))  + 

  event(name = "modBiopSeq",  
        options = c("yes", "none"), 
        probabilities = c(fModBiopsy, leftover), 
        transitions = c("HSE", "HSE"))  + 

  event(name = "HSE",  
        options = c("yes", "none"), 
        probabilities = c(fHSE, leftover), 
        transitions = c("BiopAvail", "BiopAvail"))  +

  event(name = "BiopAvail",  
        options = c("yes", "none"), 
        probabilities = c(pBiopsy, leftover), 
        transitions = c("BiopRes", "die"))  + 

  event(name = "BiopRes",  
        options = c("yes", "none"), 
        probabilities = c(pBiopRes, leftover), 
        transitions = c("die", "die"))  + 

  event(name = "die",  
        options = c("yes", "none"), 
        probabilities = c(pEvent_die, leftover), 
        transitions = c("DEAD", "sevSeqHSE")) +

  event(name = "sevSeqHSE",  
        options = c("yes", "none"), 
        probabilities = c(pEvent_sev, leftover), 
        transitions = c("SEVSEQHSE", "modSeqHSE")) +

  event(name = "modSeqHSE",  
        options = c("yes", "none"), 
        probabilities = c(pEvent_mod, leftover), 
        transitions = c("MODSEQHSE", "MLDSEQHSE")) + 

  payoffs(names = c("utility", "cost"))


## -----------------------------------------------------------------------------
pEvent_die <- function(HSE, decision, BiopRes, pDieHSE, pDieNoHSE, fDie, addProbDie){
  pEvent(HSE, decision, BiopRes, pEventHSE=pDieHSE, pEventNoHSE=pDieNoHSE, fEvent=fDie, addProbEvent=addProbDie, 
         pEventRx, pEventNoRx)
}

pEvent_sev <- function(HSE, decision, BiopRes, pSevHSE, pSevNoHSE, fSev, addProbSev){
  pEvent(HSE, decision, BiopRes, pEventHSE=pSevHSE, pEventNoHSE=pSevNoHSE, fEvent=fSev, addProbEvent=addProbSev, 
         pEventRx, pEventNoRx)
}

pEvent_mod <- function(HSE, decision, BiopRes, pModHSE, pModNoHSE, fMod, addProbMod){
  pEvent(HSE, decision, BiopRes, pEventHSE=pModHSE, pEventNoHSE=pModNoHSE, fEvent=fMod, addProbEvent=addProbMod, 
         pEventRx, pEventNoRx)
}

pEvent <- function(HSE, decision, BiopRes, pEventHSE, pEventNoHSE, fEvent, addProbEvent, 
                             pEventRx, pEventNoRx) {
  # Vectorized calculation of pEventRx and pEventNoRx
  pEventRx <- ifelse(HSE=="yes", (1 - fEvent) * pEventHSE, pEventNoHSE + addProbEvent - pEventNoHSE * addProbEvent)
  pEventNoRx <- ifelse(HSE=="yes", pEventHSE, pEventNoHSE)
  
  # Decision logic based on the provided decision variable
  result <- ifelse(decision == "TreatNone", pEventNoRx,
            ifelse(decision == "TreatAll", pEventRx,
            ifelse(BiopRes=="yes", pEventRx, pEventNoRx))) # if No biopsy
  
  return(result)
}

## -----------------------------------------------------------------------------
fDieBiopsy <- function(pDieBiopsy){
  pDieBiopsy
}
fSevBiopsy <- function(pSevBiopsy){
  pSevBiopsy
}
fModBiopsy <- function(pModBiopsy){
  pModBiopsy
}
fHSE <- function(pHSE){
  pHSE
}

pBiopsy <- function(decision){
  (decision=="BrainBiopsy")
}

## -----------------------------------------------------------------------------
pBiopRes <- function(HSE, sensBiopsy, specBiopsy){
  (HSE=="yes")*sensBiopsy + (HSE=="none")*(1-specBiopsy)
}


## -----------------------------------------------------------------------------
utility <- function(decision, outcome, sevBiopSeq, modBiopSeq, uSev, uMod, uMld, uDie) {
  # Vectorized calculation of uMult
  uMult <- ifelse(decision == "BrainBiopsy", 
                  ifelse(sevBiopSeq=="yes", uSev, 
                  ifelse(modBiopSeq=="yes", uMod, uMld)), 
                  1)
  
  # Vectorized handling of outcomes
  result <- ifelse(outcome == "DEAD", uDie,
                   ifelse(outcome == "SEVSEQHSE", uSev * uMult,
                          ifelse(outcome == "MODSEQHSE", uMod * uMult,
                                 uMld * uMult)))
  
  return(result)
}


## -----------------------------------------------------------------------------


