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
# Microsimulation Tile Tests 'Emerge'
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
           Pf_c = 1,
           FeverPf = 0.75)

# XX.Parameters() functions to generate parameters for objects in a MicroTile
Landscape_PAR = Landscape.Parameters(nFeed = 9,nAqua = 9,pointGen = "lattice",module = AQUA_module,modulePars = list(N=9,lambda=8))
# AquaEmergeLambdaPlot_utility(Landscape_PAR$AquaticSite_PAR$lambda)
HumanPop_PAR = HumanPop.Parameters(nSite = 9,siteSize = 3,siteMin = 1,bWeight = 1)
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

#################################################################
# Run MICRO
#################################################################

# plots
AquaEmergeLambdaPlot_utility(Landscape_PAR$AquaticSite_PAR$lambda)
MicroLandscapePlot_utility(tile$get_Landscape())
MicroKernelPlot_utility(S = tile$get_Landscape()$get_AquaSites(),D = tile$get_Landscape()$get_FeedingSites())

# initialize human activity space and pfsi infections
tile$get_HumanPop()$init_ActivitySpace(nDaily = 0)
tile$get_HumanPop()$init_MICRO_PfSI(PfPR = 0.5, tStart = 0)

# debug(tile$get_Landscape()$addCohort)
# debug(tile$get_FemalePop()$clear_pop)
# debug(tile$get_FemalePop()$get_MosquitoIxM(1)$MBITES)

# run sim
tMax = 365
while(tile$get_tNow() < tMax){
  tile$simMICRO_oneStep(timeStep = 1,print = TRUE,logInterval = 10)
}

PfSI_history = tile$get_HumanPop()$get_PfSI_history()
plot_PfSI(PfSI_history)



#################################################################
# Microsimulation Tile Tests 'EL4P'
# basically, set up the tile, then run Cohort, then fit EL4P, then update the mosy pop.
#################################################################

rm(list=ls())
library(MASH)

# set up classes
SEARCH.MicroKernel.Setup(MBITES = "BRO",overwrite = TRUE)
MICRO.EL4P.Setup(overwrite = TRUE)
PfSI.Setup(overwrite = TRUE)
MICRO.Humans.Setup(overwrite = TRUE)

# first generate the landscape parameters
MBITES_module = "BRO"
AQUA_module = "EL4P"

nFeed = 5
nAqua = 4
Landscape_PAR = Landscape.Parameters(nFeed = nFeed,nAqua = nAqua,pointGen = "poisson",module = AQUA_module,modulePars = NULL)
HumanPop_PAR = HumanPop.Parameters(nSite = nFeed,siteSize = 3,siteMin = 1,bWeight = 1)
# set N_female equal to small number because we need to update it later.
MosquitoPop_PAR = MicroMosquitoPop.Setup(module = MBITES_module,
                                         aquaModule = AQUA_module,
                                         N_female = 1,
                                         time = 0,
                                         ix_female = rep(1,1),
                                         genotype_female = rep(0,1),
                                         batchSize = "bms",
                                         eggMatT = "off",
                                         PfEIP = 0.1,
                                         B_succeed = 1)

# Generate a MicroTile
tile = MicroTile$new(Landscape_PAR,
                     HumanPop_PAR,
                     MosquitoPop_PAR,
                     directory = "/Users/slwu89/Desktop/mash.out/")

# plot the landscape on the tile
MicroLandscapePlot_utility(tile$get_Landscape())


#################################################################
# figure out how to do eqAqua
#################################################################

# get the movement object
movement = tile$get_FemalePop()$get_movementObject()


# fit EL4P
EL4P_PAR = EL4P.Parameters(nAqua = 50,nHumans = 300,R0 = 3,eqAqua = rep(x = 0.2,times=5),EIP = 12,lifespan = 11,
                           G = 65,nu = 65/2,S = 3)

EL4P_fit = EL4P.Mesh.Fit(mesh_N = 50,EL4P_PAR = EL4P_PAR,var_tol = 5,plot = TRUE)

# update the Landscape and AquaticSite

# remember to run updatePop to update the female pop after we find our M, eqAqua, etc.
MicroMosquitoPopFemale$update_pop()
