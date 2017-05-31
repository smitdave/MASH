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
library(MASH.R6)

xx = FeedingSite$new(ix = 1, siteXY = c(0.5,0.5), searchWt = 0.5, enterP = 0.9, maxH=2)
xx$add_riskList(who = 1,pTm = 0.5,w = 9)
xx$get_riskList()

xx$add_riskList(who = 2,pTm = 0.23,w = 2)
xx$get_riskList()

xx$add_riskList(who = 1,pTm = 0.3423,w = 32)
xx$get_riskList()

xx$add_riskList(who = 3,pTm = 232,w = 4.53)
xx$get_riskList()

MICRO.Aqua.Setup(module = "emerge",overwrite = TRUE)
MICRO.Emerge.Setup(overwrite = TRUE)
yy = AquaticSite$new(ix = 1, siteXY = c(0.5,0.5), searchWt = 99, lambda = 500, haz = 0, maxQ = 2L)
yy$get_ImagoQ()
yy$add_ImagoQ(newImago = newImago(N = 1,tEmerge = 4))

yy$set_lambda(lambda = 1:365,ix = NULL)
yy$get_lambda()
yy$oneDay_EmergeSite(tNow = 50)
yy$get_ImagoQ()

yy$clear_ImagoQ()
