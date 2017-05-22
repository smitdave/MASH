#################################################################
#
#   MASH
#   R6-ified
#   Testing routines for PfSI and SimBitePfSI
#   Sean Wu
#   May 21, 2017
#
#################################################################

rm(list=ls())
library(MASH.R6)

# setup class methods for PfSI and SimBite modules
PfSI.Setup()
SimBitePfSI.Setup()

# simulate a single human population
pop1 = HumanPop$new(nHum = 10)
pop1$PfSI.Init(PfPR = 0)

tMax = 365*5

pop1$queueBites_simBitePfSI(tMax = tMax,bitingRate = 1/15)
pop1$queueVaccination_simBitePfSI(tVaccine = (365*1),tTreat = (365*1)+1,fracPop = 0.75)
pop1$simHumans(tPause = tMax+10)

# simulate many human populations
library(parallel)
simPars = replicate(n = 10,expr = PfSI.Parameters(),simplify = FALSE)
simParOut = parallel::mclapply(X = simPars,FUN = function(x){
  pop1 = HumanPop$new(nHum = 10)
  pop1$set_PfSI_PAR(x)
  pop1$PfSI.Init(PfPR = 0)
  tMax = 365*5
  pop1$queueBites_simBitePfSI(tMax = tMax,bitingRate = 1/15)
  pop1$queueVaccination_simBitePfSI(tVaccine = (365*1),tTreat = (365*1)+1,fracPop = 0.75)
  pop1$simHumans(tPause = tMax+10)
  pop1$get_History()
})
