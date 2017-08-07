#################################################################
#
#   MASH
#   R6-ified
#   Testing routines for MICRO
#   Sean Wu
#   May 30, 2017
#
#################################################################

rm(list=ls())
library(MASH)

#################################################################
# Microsimulation Tile Tests
#################################################################

#################################################################
# Init the MicroTile
#################################################################

MBITES_module = "BRO"
AQUA_module = "emerge"

# XX.Setup() functions to initialize classes for MICRO
MICRO.Humans.Setup(overwrite = TRUE)
SEARCH.MicroKernel.Setup(MBITES = "BRO",overwrite = TRUE)
MICRO.Emerge.Setup(overwrite = TRUE)
PfSI.Setup(overwrite = TRUE,
           Pf_b = 1,
           Pf_c = 1)

# DEBUGGING FLAGS (SET PRIOR TO MAKING OBJECTS)
# MicroMosquitoFemale$debug("humanEncounter")
# MicroMosquitoFemale$debug("oneBout")
# MicroMosquitoFemale$debug("surviveResting")
# MicroMosquitoFemale$debug("surviveFlight")

# XX.Parameters() functions to generate parameters for objects in a MicroTile
Landscape_PAR = Landscape.Parameters(nFeed = 8,nAqua = 3,module = AQUA_module,modulePars = list(N=3,lambda=5))
# AquaEmergeLambdaPlot_utility(Landscape_PAR$AquaticSite_PAR$lambda)
HumanPop_PAR = HumanPop.Parameters(nSite = 8,siteSize = 3,siteMin = 1,bWeight = 1)
MosquitoPop_PAR = MicroMosquitoPop.Setup(module = MBITES_module,
                                         aquaModule = AQUA_module,
                                         N_female = 20,
                                         time = 0,
                                         ix_female = rep(1,20),
                                         genotype_female = rep(1,20),
                                         batchSize = "bms",
                                         eggMatT = "off",
                                         PfEIP = 0.1,
                                         B_succeed = 1)

# Generate a MicroTile
tile = MicroTile$new(Landscape_PAR,
                     HumanPop_PAR,
                     MosquitoPop_PAR,
                     directory = "/Users/slwu89/Desktop/mash.out/")

# # plot the movement kernels
# MicroLandscapePlot_utility(tile$get_Landscape())
# MicroKernelPlot_utility(S = tile$get_Landscape()$get_AquaSites(),D = tile$get_Landscape()$get_FeedingSites())
#
# #################################################################
# # Activity Space
# #################################################################
#
# tile$get_HumanPop()$init_ActivitySpace(nDaily = 1.4)
# tile$get_HumanPop()$sim_ActivitySpace()
# for(i in 1:tile$get_Landscape()$FeedingSitesN){
#   print(
#     tile$get_Landscape()$get_FeedingSites(i)$get_RiskQ()$get_HumanHost()
#   )
# }
#
# # initialize PfSI infections
# tile$get_HumanPop()$init_MICRO_PfSI(PfPR = 0.15, tStart = 0)
# tile$get_HumanPop()$get_PfSI_history()
#
# #################################################################
# # Aquatic Ecology
# #################################################################
#
# for(i in 1:tile$get_Landscape()$AquaSitesN){
#   print(
#     tile$get_Landscape()$get_AquaSites(ixS = i)$get_ImagoQ()$get_ImagoQ()[1]
#   )
# }
#
# tile$get_Landscape()$oneStep_AquaticEcology() # run the oneStep dynamics
#
# for(i in 1:tile$get_Landscape()$AquaSitesN){
#   print(
#     tile$get_Landscape()$get_AquaSites(ixS = i)$get_ImagoQ()$get_ImagoQ()[1]
#   )
# }
#
# tile$get_Landscape()$addCohort()
# tile$get_FemalePop()$get_MosquitoIxM(21)
# tile$get_FemalePop()
#
# #################################################################
# # M-BITES
# #################################################################
#
# tile$get_FemalePop()$MBITES()

#################################################################
# Run MICRO
#################################################################

# plots
AquaEmergeLambdaPlot_utility(Landscape_PAR$AquaticSite_PAR$lambda)
MicroLandscapePlot_utility(tile$get_Landscape())
MicroKernelPlot_utility(S = tile$get_Landscape()$get_AquaSites(),D = tile$get_Landscape()$get_FeedingSites())

# initialize human activity space and pfsi infections
tile$get_HumanPop()$init_ActivitySpace(nDaily = 1.4)
tile$get_HumanPop()$init_MICRO_PfSI(PfPR = 0.5, tStart = 0)

# debug(tile$get_Landscape()$addCohort)
# debug(tile$get_FemalePop()$clear_pop)
# debug(tile$get_FemalePop()$get_MosquitoIxM(1)$MBITES)

# run sim
tMax = 200
while(tile$get_tNow() < tMax){
  tile$simMICRO_oneStep(timeStep = 1,print = TRUE,logInterval = 10)
}

PfSI_history = tile$get_HumanPop()$get_PfSI_history()
plot_PfSI(PfSI_history)



#################################################################
# Component Tests
#################################################################

# MICRO.Emerge.Setup(overwrite = TRUE)
#
# xx = FeedingSite$new(ix = 1, siteXY = c(0.5,0.5), searchWt = 0.5, enterP = 0.9)
# xx$get_RiskQ()$add_HumanHost(who_new = 1,pTm_new = 0.5,w_new = 9)
# xx$get_RiskQ()$get_HumanHost()
# xx$get_RiskQ()$get_OtherHost()
#
# yy = AquaticSite$new(ix = 1, siteXY = c(0.5,0.5), searchWt = 99, lambda = 500, haz = 0, module = "emerge")
# yy$get_ImagoQ()
# yy$get_ImagoQ()$add_ImagoQ(N_new=10,tEmerge_new=4,genotype_new=1,damID_new="1",sireID_new="1")
# yy$get_ImagoQ()$add_ImagoQ(N_new=5,tEmerge_new=5,genotype_new=2,damID_new="2",sireID_new="2")
# yy$get_ImagoQ()$get_ImagoQTime(tNow=4.1,clear=FALSE)
#
#
#
# yy$set_lambda(lambda = 1:365)
# yy$get_lambda()
#
# yy$oneDay_EmergeSite(tNow = 50)
# yy$get_ImagoQ()$get_ImagoQTime(tNow=50.1,clear=FALSE)
#
# yy$get_ImagoQ()$get_ImagoQTime(tNow=50.1,clear=TRUE)
# yy$get_ImagoQ()$get_ImagoQ()
#
#
# # make a landscape
# Landscape_PAR = Landscape.Parameters(nFeed = 10,nAqua = 12,module = "emerge",modulePars = list(N=12,lambda=5))
# zz = Landscape$new(Landscape_PAR)
#
# MvAll = MicroKernel_exactAll(zz)
#
# # make a Tile
# MICRO.Humans.Setup(overwrite = TRUE)
#
# MicroTile_PAR = MICRO.Tile.Parameters(nFeed = 5,nAqua = 3,module = "emerge",modulePars = list(N=3,lambda=7))
# tile = MicroTile$new(MicroTile_PAR)
#
# # set up SEARCH-MicroKernels
# SEARCH.MicroKernel.Setup(MBITES = "BRO",overwrite = TRUE)


