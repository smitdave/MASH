#################################################################
#
#   MASH
#   R6-ified
#   Testing routines for MACRO
#   Sean Wu
#   May 25, 2017
#
#################################################################

rm(list=ls())
library(MASH)

set.seed(42)

# initialize classes for MACRO
MACRO.Humans.Setup()

# setup class methods for PfSI and SimBite modules
# MACRO relies on SimBitePfSI module for adding bites to humans
PfSI.Setup()
SimBitePfSI.Setup()

# MACRO Patch initialization
MACRO.Patch.Emerge.Setup() # 'Emerge' model

# patch parameters
nPatch = 10
tileParameters = MACRO.Tile.Parameters(N = nPatch,aquaModel = "emerge",aquaPars = list(N=nPatch,lambda=rep(50,nPatch)))
tileParameters$MacroMosquitoPop_PAR$M_density = rep(200,nPatch)
tile = MacroTile$new(MacroTile_PAR = tileParameters)

PfPR = c(rep(0,nPatch/2),rep(0.75,nPatch/2))
# PfPR = rep(0,nPatch)
tile$init_PfSI(PfPR = PfPR)

tile$simMacro(1e3)

pfsiHist = tile$get_HumanPop()$get_History()
plot_PfSI(pfsiHist)

travelHist = tile$get_HumanPop()$get_travelHistory()
tile$get_HumanPop()$json_travelHistory(con = file(description = "/Users/slwu89/Desktop/OUTPUT/humanTravel.json",open = "wt"))
