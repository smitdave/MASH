library(MASH.MBPT)

# global parameters
P = MBITES.PAR(maxBatch = 35)

# set up LANDSCAPE
LANDSCAPE = makeLandscape(nF = 20,nA = 15,nS = 5,nM = 5,aquaMod = "el4p",pointGen = pointsPoisson,hhSize = 10,hhMin = 2,aquaSD = 0.025)
staticLandscapePlot()
MvOb = exactAll(LANDSCAPE)

# test MBITES-Basic
basicN = 5e3
basicOut = MBITES.basic(N = basicN,P = P,parallel = TRUE,move = TRUE)

# basicTraj = getStateTraj(ix = 1:basicN,mosyPop = basicOut)
# cohortStateTrajectory(stateT = basicTraj,logX = F,normY = T,lwd = 1.5)

cohortBionomics = cohortBionomics(mosyPop = basicOut,ix = 1:basicN)
aquaEq = aquaIx_equilibrium(mosyPop = basicOut)
