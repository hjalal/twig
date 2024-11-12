

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
mytwig <- twig() + 
  decisions("BrainBiopsy", "NoBiopsy_Treat", "NoBiopsy_NoTreat") + 
  #final_outcomes("DEAD","SEVSEQHSE","MODSEQHSE","MLDSEQHSE") + 
  event(name = "Biopsy",  
        scenarios = c("yes", "none"), 
        probs = c("pBiopsy", "#"), 
        goto = c("dieBiop", "HSE"))  + 
  event(name = "dieBiop",  
        scenarios = c("yes", "none"), 
        probs = c("fDieBiopsy", "#"), 
        goto = c("DEAD", "sevBiopSeq"))  + 
  event(name = "sevBiopSeq",  
        scenarios = c("yes", "none"), 
        probs = c("fSevBiopsy", "#"), 
        goto = c("HSE", "modBiopSeq"))  + 
  event(name = "modBiopSeq",  
        scenarios = c("yes", "none"), 
        probs = c("fModBiopsy", "#"), 
        goto = c("HSE", "HSE"))  + 
  event(name = "HSE",  
        scenarios = c("yes", "none"), 
        probs = c("fHSE", "#"), 
        goto = c("BiopAvail", "BiopAvail"))  +
  event(name = "BiopAvail",  
        scenarios = c("yes", "none"), 
        probs = c("pBiopsy", "#"), 
        goto = c("BiopRes", "die"))  + 
  event(name = "BiopRes",  
        scenarios = c("yes", "none"), 
        probs = c("pBiopRes", "#"), 
        goto = c("die", "die"))  + 
  event(name = "die",  
        scenarios = c("yes", "none"), 
        probs = c("pEvent_die", "#"), 
        goto = c("DEAD", "sevSeqHSE")) +
  event(name = "sevSeqHSE",  
        scenarios = c("yes", "none"), 
        probs = c("pEvent_sev", "#"), 
        goto = c("SEVSEQHSE", "modSeqHSE")) +
  event(name = "modSeqHSE",  
        scenarios = c("yes", "none"), 
        probs = c("pEvent_mod", "#"), 
        goto = c("MODSEQHSE", "MLDSEQHSE")) + 
  payoffs(names = "utility")


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
  result <- ifelse(decision == "NoBiopsy_NoTreat", pEventNoRx,
            ifelse(decision == "NoBiopsy_Treat", pEventRx,
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


