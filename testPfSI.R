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
pop = HumanPop$new(nHum = 10)
pop$PfSI.Init(PfPR = 0)

tMax = 365*5

pop$queueBites_simBitePfSI(tMax = tMax,bitingRate = 1/15)
pop$queueVaccination_simBitePfSI(tVaccine = (365*1),tTreat = (365*1)+1,fracPop = 0.75)
pop$simHumans(tPause = tMax+10)

pop$get_History()

# simulate a single human population with negbinom biting
# see ?queueBitesNegBinom_SimBitePfSI for details
rm(pop)
pop = HumanPop$new(nHum = 10)
pop$PfSI.Init(PfPR = 0)

tMax = 365*5

pop$queueBitesNegBinom_SimBitePfSI(tMax = tMax, meanNumberBites = 100, plot = TRUE)
pop$simHumans(tPause = tMax+10)
pop$get_History()

# simulate many human populations
library(parallel)
simPars = replicate(n = 10,expr = PfSI.Parameters(),simplify = FALSE)
simParOut = parallel::mclapply(X = simPars,FUN = function(x){
  pop = HumanPop$new(nHum = 10)
  pop$set_PfSI_PAR(x)
  pop$PfSI.Init(PfPR = 0)
  tMax = 365*5
  pop$queueBites_simBitePfSI(tMax = tMax,bitingRate = 1/15)
  pop$queueVaccination_simBitePfSI(tVaccine = (365*1),tTreat = (365*1)+1,fracPop = 0.75)
  pop$simHumans(tPause = tMax+10)
  pop$get_History()
})
