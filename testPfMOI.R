#################################################################
#
#   MASH
#   R6-ified
#   Testing routines for PfMOI and SimBitePfMOI
#   Sean Wu
#   June 18, 2017
#
#################################################################

rm(list=ls())
library(MASH)

# setup class methods for PfMOI and SimBite modules
PfMOI.Setup()
SimBitePfMOI.Setup()

#################################################################
# simulate a single human population
#################################################################

# make a population of 1000 people
nHumans = 1e3
HumanPop_PAR = HumanPop.Parameters(nSite = 1,bWeight = NULL,siteSize = nHumans,siteMin = nHumans)
HumanPopulation = HumanPop$new(HumanPop_PAR)

# set up PfMOI module with all MOI initially 0
PfMOI_PAR = PfMOI.Parameters()
HumanPopulation$set_PfMOI_PAR(PfMOI_PAR = PfMOI_PAR)
HumanPopulation$init_MICRO_PfMOI(PfMOI = NULL)

# queue bites and PE vaccination
tMax = 365*5
bites = SimBite_MeanBites(nH = nHumans,meanNumberBites = 100,days = tMax,plot = TRUE)
HumanPopulation$queueBites_SimBitePfMOI(bites = bites)
HumanPopulation$queueVaccination_SimBitePfMOI(tVaccine = 365*3,tTreat = (365*3)+1,fracPop = 0.25)

# run SimBitePfMOI module
debug(HumanPopulation$get_Human(1)$probeHost_PfMOI)
HumanPopulation$simHumans(tPause = tMax+10)
