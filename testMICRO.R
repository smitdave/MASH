#################################################################
#
#   MASH
#   R6-ified
#   Testing routines for MICRO
#   Sean Wu
#   May 30, 2017
#
#################################################################


xx = FeedingSite$new(ix = 1, siteXY = c(0.5,0.5), searchWt = 0.5, enterP = 0.9, maxH=2)
xx$add_riskList(who = 1,pTm = 0.5,w = 9)
xx$get_riskList()

xx$add_riskList(who = 2,pTm = 0.23,w = 2)
xx$get_riskList()

xx$add_riskList(who = 1,pTm = 0.3423,w = 32)
xx$get_riskList()

xx$add_riskList(who = 3,pTm = 232,w = 4.53)
xx$get_riskList()
