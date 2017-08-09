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

# for mbites-bro, make the relevant matrix
aquaMatrix = matrix(data = 0,nrow = nFeed+nAqua,ncol = nFeed+nAqua)
aquaNames = c(paste0("F",1:nFeed),paste0("L",1:nAqua))
colnames(aquaMatrix) = aquaNames
rownames(aquaMatrix) = aquaNames


feedIx = grep(pattern = "F",x = rownames(aquaMatrix))
aquaIx = grep(pattern = "L",x = rownames(aquaMatrix))

# fill in parts of the matrix corresponding to moving from feeding site to aquatic habitat (F2L)
for(ixF in feedIx){

  # starting site
  feedSite = as.numeric(substr(x = aquaNames[ixF],start = 2,stop = 2))

  # movement object from starting site to destination sites
  mvOb = movement$F2L[[feedSite]]$near

  for(i in 1:length(mvOb$id)){
    aquaMatrix[ feedIx[feedSite], aquaIx[mvOb$id[i]] ] = mvOb$pr[i]
  }

}


# fill in L2L
for(ixA in aquaIx){

  # starting site
  aquaSite = as.numeric(substr(x = aquaNames[ixA],start = 2,stop = 2))

  # movement object from starting site to destination sites
  mvOb = movement$L2L[[aquaSite]]$near

  for(i in 1:length(mvOb$id)){
    aquaMatrix[ aquaIx[aquaSite], aquaIx[mvOb$id[i]] ] = mvOb$pr[i]
  }

}

aquaMarkov = new(Class = "markovchain",states = aquaNames, transitionMatrix = aquaMatrix,name = "aquaMarkov")
markovchain::steadyStates(aquaMarkov)


# fit EL4P
EL4P_PAR = EL4P.Parameters(nAqua = 50,nHumans = 300,R0 = 3,eqAqua = rep(x = 0.2,times=5),EIP = 12,lifespan = 11,
                           G = 65,nu = 65/2,S = 3)

EL4P_fit = EL4P.Mesh.Fit(mesh_N = 50,EL4P_PAR = EL4P_PAR,var_tol = 5,plot = TRUE)

# update the Landscape and AquaticSite
