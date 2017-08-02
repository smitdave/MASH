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

MBITES_module = "BRO"
AQUA_module = "emerge"

# XX.Setup() functions to initialize classes for MICRO
MICRO.Humans.Setup(overwrite = TRUE)
SEARCH.MicroKernel.Setup(MBITES = "BRO",overwrite = TRUE)
MICRO.Emerge.Setup(overwrite = TRUE)
PfSI.Setup(overwrite = TRUE)

# XX.Parameters() functions to generate parameters for objects in a MicroTile
Landscape_PAR = Landscape.Parameters(nFeed = 3,nAqua = 3,module = AQUA_module,modulePars = list(N=3,lambda=10))
AquaEmergeLambdaPlot_utility(Landscape_PAR$AquaticSite_PAR$lambda)
HumanPop_PAR = HumanPop.Parameters(nSite = 3,siteSize = 3,siteMin = 1)
MosquitoPop_PAR = MicroMosquitoPop.Setup(module = MBITES_module,
                                         aquaModule = AQUA_module,
                                         N_female = 20,
                                         time = 0,
                                         ix_female = rep(1,20),
                                         genotype_female = rep(1,20),
                                         batchSize = "bms",
                                         eggMatT = "off")

# Generate a MicroTile
tile = MicroTile$new(Landscape_PAR,
                     HumanPop_PAR,
                     MosquitoPop_PAR,
                     directory = "/Users/slwu89/Desktop/mash.out/")

MicroLandscapePlot_utility(tile$get_Landscape())
MicroKernelPlot_utility(S = tile$get_Landscape()$get_AquaSites(),D = tile$get_Landscape()$get_FeedingSites())


tile$get_HumanPop()$init_ActivitySpace(nDaily = 1.4)
tile$get_HumanPop()$sim_ActivitySpace()
for(i in 1:3){
  print(tile$get_Landscape()$get_FeedingSites(i)$get_RiskQ()$get_HumanHost()  )
}



MicroActivitySpacePlot_utility <- function(Landscape){

  nSites = Landscape$FeedingSitesN



}


#riskList$atRisk stores the risk distribution object
human_pTm <- function(atRisk){
  h_id <- unique(as.vector(atRisk$Who)) #grab unique human IDs
  h_id <- h_id[h_id!=0]
  h_pTm <- matrix(0.0,nrow=length(h_id),ncol=atRisk$N.f)
  h_w <- matrix(0.0,nrow=length(h_id),ncol=atRisk$N.f)
  for(i in h_id){
    h_ix <- which(atRisk$Who == i,arr.ind=TRUE) #grab indicies for human i
    for(j in 1:nrow(h_ix)){ #fill h_pTm and h_w for human i
      h_pTm[i,unname(h_ix[j,1])] <- atRisk$pTm[h_ix[j,1],h_ix[j,2]]
      h_w[i,unname(h_ix[j,1])] <- atRisk$w[h_ix[j,1],h_ix[j,2]]
    } #end iter over human i sites
  } #end iter over all humans
  return(list(id=h_id,pTm=h_pTm,w=h_w))
}



###base plotting animation###

#generate pTm data
atRisk_hist_pTm <- vector(mode="list",length=t_max)
for(i in 1:t_max){
  atRisk_hist_pTm[[i]] <- human_pTm(atRisk_hist[[i]])
}

generate_risk_animation <- function(pTm_hist){

  colors <- colorRampPalette(colors=c("#132B43","#56B1F7"))(20)
  max_iter <- length(pTm_hist)

  for(i in 1:max_iter){

    par(mar=c(0,0,0,0),mgp=c(0,0,0))
    image(t(pTm_hist[[i]]$pTm[nrow(pTm_hist[[i]]$pTm):1,]),col=colors)
    text(x=0.9,y=0.9,labels=paste0("day: ",i),cex=1.25,col="white")

    print(paste0("on iteration: ",i,", of: ",max_iter))

  }

}

saveGIF(generate_risk_animation(atRisk_hist_pTm),movie.name="/Users/slwu89/Desktop/MASH-Development/images/risk_animation.gif",
        interval=0.25,ani.width=480,ani.height=480)





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


