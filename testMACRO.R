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
library(MASH.R6)

set.seed(42)

# initialize classes for MACRO
MACRO.Humans.Setup()

# setup class methods for PfSI and SimBite modules
# MACRO relies on SimBitePfSI module for adding bites to humans
PfSI.Setup()
SimBitePfSI.Setup()

# MACRO Patch initialization
MACRO.Patch.Emerge.Setup() # 'Emerge' model

# MACRO specific Human methods
MACRO.Humans.Setup()

# set debug flags in R6 generator classes before initializing objects
# HumanPop$debug("queueInfectiousBites")
# Human$debug("expectedBites")
MacroTile$debug("simMacro")

tileParameters = MACRO.Tile.Parameters(N = 10)
tile = MacroTile$new(MacroTile_PAR = tileParameters)

tile$init_humanInf(PfPR = 0.15)

tile$simMacro(1e3)

tile$get_HumanPop()$get_History()
