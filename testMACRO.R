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

# initialize classes for MACRO
MACRO.Humans.Setup()

# setup class methods for PfSI and SimBite modules
# MACRO relies on SimBitePfSI module for adding bites to humans
PfSI.Setup()
SimBitePfSI.Setup()

# MACRO Patch initialization
MACRO.Patch.Emerge.Setup() # 'Emerge' model

tileParameters = MACRO.Tile.Parameters(N = 10)
tile = MacroTile$new(MacroTile_PAR = tileParameters)

# # MACRO - Emergence
# MacroPatch$oneDay_MacroEmerge()
# MacroPatch$addCohort_MacroEmerge()
