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

MICRO.Aqua.Setup(module = "emerge",overwrite = TRUE)
MICRO.Emerge.Setup(overwrite = TRUE)

xx = FeedingSite$new(ix = 1, siteXY = c(0.5,0.5), searchWt = 0.5, enterP = 0.9)
xx$get_RiskQ()$add_HumanHost(who_new = 1,pTm_new = 0.5,w_new = 9)
xx$get_RiskQ()$get_HumanHost()
xx$get_RiskQ()$get_OtherHost()

# xx$add_riskList(who = 2,pTm = 0.23,w = 2)
# xx$get_riskList()
#
# xx$add_riskList(who = 1,pTm = 0.3423,w = 32)
# xx$get_riskList()
#
# xx$add_riskList(who = 3,pTm = 232,w = 4.53)
# xx$get_riskList()


yy = AquaticSite$new(ix = 1, siteXY = c(0.5,0.5), searchWt = 99, lambda = 500, haz = 0, maxQ = 2L)
yy$get_ImagoQ()
yy$add_ImagoQ(newImago = newImago(N = 1,tEmerge = 4))

yy$set_lambda(lambda = 1:365,ix = NULL)
yy$get_lambda()
yy$oneDay_EmergeSite(tNow = 50)
yy$get_ImagoQ()

yy$clear_ImagoQ()


# make a landscape
Landscape_PAR = Landscape.Parameters(nFeed = 10,nAqua = 12,module = "emerge",modulePars = list(N=12,lambda=5))
zz = Landscape$new(Landscape_PAR)

MvAll = MicroKernel_exactAll(zz)

# make a Tile
MICRO.Humans.Setup(overwrite = TRUE)

MicroTile_PAR = MICRO.Tile.Parameters(nFeed = 5,nAqua = 3,module = "emerge",modulePars = list(N=3,lambda=7))
tile = MicroTile$new(MicroTile_PAR)

# set up SEARCH-MicroKernels
SEARCH.MicroKernel.Setup(MBITES = "BRO",overwrite = TRUE)


