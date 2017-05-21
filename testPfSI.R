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

PfSI.Setup()
SimBitePfSI.Setup()

humanPopulation = HumanPop$new(nHum = 1e2)
humanPopulation$PfSI.Init(PfPR = 0)

tMax = 365*5

humanPopulation$queueBites_simBitePfSI(tMax = tMax,bitingRate = 1/15)

humanPopulation$simHumans(tPause = tMax+10)
