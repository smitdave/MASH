#################################################################
#
#   MASH/MBITES
#   Landscape generation routines
#   R version
#   Sean Wu
#   January 24, 2017
#
#################################################################


##########################################
# Point Clustering Patterns
##########################################
pointsPoisson <- function(n, xLim=c(0,1), yLim=c(0,1)){
  #. pointsPoisson: Poisson point pattern for landscape sites
  ps = spatstat::rpoispp(lambda = n,win = spatstat::owin(xLim,yLim))
  while(ps$n != n){
    ps = spatstat::rpoispp(lambda = n,win = spatstat::owin(xLim,yLim))
  }

  return(list(x=ps$x,y=ps$y))
}

pointsClustered <- function(n, meanParents = 10, clusteredness = .25, xLim=c(0,1), yLim=c(0,1)){
  #. pointsClustered: Matern cluster point pattern for landscape sites
  meanDist = clusteredness / sqrt(meanParents)
  meanChildren = n / meanParents

  ps = spatstat::rMatClust(meanParents, meanDist, meanChildren, win = spatstat::owin(xLim,yLim))
  while(ps$n != n){
    ps = spatstat::rMatClust(meanParents, meanDist, meanChildren, win = spatstat::owin(xLim,yLim))
  }

  return(list(x=ps$x,y=ps$y))
}

pointsOverdispersed <- function(n, inhibitionFactor = 1, xLim=c(0,1), yLim=c(0,1)){
  #. pointsOverdispersed: Overdispersed points pattern
  ps = spatstat::rSSI(inhibitionFactor / sqrt(n), n, win = spatstat::owin(xLim,yLim))
  while(ps$n != n){
    inhibitionFactor = inhibitionFactor - .01
    ps = spatstat::rSSI(inhibitionFactor / sqrt(n), n, win = spatstat::owin(xLim,yLim))
  }

  return(list(x=ps$x,y=ps$y))
}

pointsLattice <- function(n, xLim=c(0,1), yLim=c(0,1)){
  #. pointsLattice: Regular lattice points distribution
  # return a grid of points
  Pdim = ceiling(sqrt(n))
  lb.x = (xLim[1] + xLim[2])/Pdim; lb.x = (xLim[1] + lb.x)/2
  lb.y = (yLim[1] + yLim[2])/Pdim; lb.y = (yLim[1] + lb.y)/2
  ub.x = xLim[2] - lb.x
  ub.y = yLim[2] - lb.y

  g1 = seq(lb.x, ub.x, length.out = Pdim)
  g2 = seq(lb.y, ub.y, length.out = Pdim)
  x = as.vector(matrix(g1,Pdim,Pdim))
  y = as.vector(matrix(g1,Pdim,Pdim,byrow = T))
  list(x=x,y=y)
}


# ##########################################
# # Define Site Types
# ##########################################
# makeFeedingSite <- function(siteIx , siteXY, humanN, humanIx, searchWt, landHaz, enterHouseP){
#   #. makeFeedingSite: creates a blood-feeding site on the landscape
#   site = list(
#     ix = siteIx,
#     siteXY = siteXY,
#     humanN = humanN,
#     humanIx = humanIx,
#     w = searchWt,
#     haz = landHaz,
#     enterHouseP = enterHouseP,
#     riskList = NULL
#     #################################################
#     # Vector Control
#     #################################################
#     # AREA.REPEL=UniformProbabilityIntervention(INTERVENTIONS_PARAMETERS$AREA.REPEL),
#     # BAITED.TRAP=UniformProbabilityIntervention(INTERVENTIONS_PARAMETERS$BAITED.TRAP),
#     # EAVE.TUBE=UniformProbabilityIntervention(INTERVENTIONS_PARAMETERS$EAVE.TUBE),
#     # IMPROVE.HOME=UniformProbabilityIntervention(INTERVENTIONS_PARAMETERS$IMPROVE.HOME),
#     # IRS=UniformProbabilityIntervention(INTERVENTIONS_PARAMETERS$IRS),
#     # ZOOSPRAY=UniformProbabilityIntervention(INTERVENTIONS_PARAMETERS$ZOOSPRAY)
#   )
#
#   return(site)
# }
#
# makeAquaSite <- function(siteIx, siteXY, searchWt, landHaz, aquaMod, nBatches = 5, nAdults = 5, nEL4P = 5){
#   #. makeAquaSite: creates an aquatic habitat (oviposition spot) on the landscape
#   site = list(
#     ix = siteIx, #index
#     siteXY = siteXY, #XY coordinates
#     w = searchWt, #searchWt
#     haz = landHaz, #landHaz
#     EggQ = allocEggQ(N = nBatches), #EggQ for this site
#     ImagoQ = allocImagoQ(N = nAdults) #ImagoQ for this site
#     #################################################
#     # Vector Control
#     #################################################
#     # BIOLOGICAL.CONTROL=UniformProbabilityIntervention(INTERVENTIONS_PARAMETERS$BIOLOGICAL.CONTROL),
#     # FOUL.HABITAT=UniformProbabilityIntervention(INTERVENTIONS_PARAMETERS$FOUL.HABITAT),
#     # LARVICIDING=UniformProbabilityIntervention(INTERVENTIONS_PARAMETERS$LARVICIDING),
#     # OVITRAP=UniformProbabilityIntervention(INTERVENTIONS_PARAMETERS$OVITRAP),
#     # SOURCE.REDUCTION=UniformProbabilityIntervention(INTERVENTIONS_PARAMETERS$SOURCE.REDUCTION)
#   )
#   if(aquaMod == "el4p"){
#     site$EL4P = EL4P()
#     site$alpha = 0
#     site$psi = 0
#     site$p = 0
#   }
#   if(aquaMod == "emerge"){
#     site$season = vector(mode="numeric",length=365) #seasonal emergence for this site
#   }
#
#   return(site)
# }
#
# makeSugarSite <- function(siteIx, siteXY, searchWt, landHaz){
#   #. makeSugarSite: creates a sugar-feeding site on the landscape
#   site = list(
#     ix = siteIx,
#     siteXY = siteXY,
#     w = searchWt,
#     haz = landHaz
#     #################################################
#     # Vector Control
#     #################################################
#     # ATSB=UniformProbabilityIntervention(INTERVENTIONS_PARAMETERS$ATSB)
#   )
#
#   return(site)
# }
#
# makeSwarmingSite <- function(siteIx, siteXY, searchWt, landHaz, nMales = 5){
#   #. makeSwarmingSite: creates a swarming (mating) site on the landscape
#   site = list(
#     ix = siteIx,
#     siteXY = siteXY,
#     w = searchWt,
#     haz = landHaz,
#     matingQ = allocMatingQ(nMales = nMales)
#     #################################################
#     # Vector Control
#     #################################################
#     # FOGGING=UniformProbabilityIntervention(INTERVENTIONS_PARAMETERS$FOGGING),
#     # SWARM.SPRAYING=UniformProbabilityIntervention(INTERVENTIONS_PARAMETERS$SWARM.SPRAYING)
#   )
#
#   return(site)
# }
#
# ##########################################
# # Generate Landscape Object
# ##########################################
# makeLandscape <- function(nF, nA, nS, nM, aquaMod, pointGen, hhSize, hhMin = 2, xLim=c(0,1), yLim=c(0,1), aquaSD = 0.01, swarmSD = 0.05, ...){
#   #. makeLandscape: generate the entire landscape object
#   #nF: number of feeding sites
#   #nA: number of aquatic habitats
#   #nM: number of swarming sites (usually clustered near aquatic habitats)
#   #aquaMod: char; "emerge" or "el4p"
#   #pointGen: point generation function
#   #hhSize: average number of hosts at feeding sites
#   #hhMin: minimum number of hosts at feeding sites
#   #...: additional named arguments for pointGen()
#
#   # sanity checks
#   if(!aquaMod %in% c("emerge","el4p")){
#     stop("aquaMod must be one of 'emerge' or 'el4p'")
#   }
#
#   #########################################
#   # Generate Feeding Sites
#   #########################################
#   feedXY = pointGen(nF, ...)
#   feedWt = rgamma(n = nF,1,1); feedWt = feedWt / sum(feedWt)
#   feedHaz = rbeta(n = nF,99,1)
#   enterHouseP = rbeta(n = nF,9,1)
#   #distribute humans
#   hh = hhMin + rpois(n = nF,lambda = hhSize - hhMin)
#   nH = sum(hh) #number of humans
#   hhIx = vector(mode="list",length=nF) #id of humans by household
#   hhIxI = 0
#   for(i in 1:nF){
#     hhIx[[i]] = (hhIxI+1):(hhIxI+hh[i])
#     hhIxI = hhIxI+hh[i]
#   }
#   #make feeding sites
#   feedSites = vector(mode="list",length=nF)
#   for(i in 1:nF){
#     feedSites[[i]] = makeFeedingSite(siteIx = i,siteXY = c(feedXY$x[i],feedXY$y[i]),humanN = hh[i],humanIx = hhIx[[i]],
#                                      searchWt = feedWt[i],landHaz = feedHaz[i], enterHouseP = enterHouseP[i])
#   }
#
#   #########################################
#   # Generate Aquatic Habitats
#   #########################################
#   #generate aquatic habitat parameters
#   aquaIx = sample(x = nF,size = nA,replace = TRUE)
#   aquaXY = list(x = rnorm(n = nA,mean = feedXY$x[aquaIx],sd = aquaSD), y = rnorm(n = nA,mean = feedXY$y[aquaIx],sd = aquaSD))
#   aquaWt = rgamma(n = nA,1,1); aquaWt = aquaWt / sum(aquaWt)
#   aquaHaz = rbeta(n = nA,99,1)
#   #make aquatic habitats
#   aquaSites = vector(mode="list",length=nA)
#   for(i in 1:nA){
#     aquaSites[[i]] = makeAquaSite(siteIx = i,siteXY = c(aquaXY$x[i],aquaXY$y[i]),searchWt = aquaWt[i],landHaz = aquaHaz[i], aquaMod = aquaMod)
#   }
#   landscape = list(
#     feedSites = feedSites, aquaSites = aquaSites,
#     xLim = xLim, yLim = yLim, #bounding box limits
#     nH = nH, hhIx = hhIx, hhSizes = hh, #information on distribution and numbers of humans
#     nF = nF, nA = nA #number of sites
#   )
#
#   #########################################
#   # Generate Sugar Sites
#   #########################################
#   if(nS > 0){
#     #generate swarming site parameters
#     sugarXY = pointGen(nS, ...)
#     sugarWt = rgamma(n = nS,1,1); sugarWt = sugarWt / sum(sugarWt)
#     sugarHaz = rbeta(n = nS,99,1)
#     #make swarming sites
#     sugarSites = vector(mode="list",length=nS)
#     for(i in 1:nS){
#       sugarSites[[i]] = makeSugarSite(siteIx = i,siteXY = c(sugarXY$x[i],sugarXY$y[i]),searchWt = sugarWt[i],landHaz = sugarHaz[i])
#     }
#
#     landscape$nS = nS
#     landscape$sugarSites = sugarSites
#   }
#
#   #########################################
#   # Generate Swarming Sites
#   #########################################
#   if(nM > 0){
#     #generate swarming site parameters
#     swarmIx = sample(x = nA,size = nM,replace = TRUE)
#     swarmXY = list(x = rnorm(n = nM,mean = aquaXY$x[swarmIx],sd = swarmSD), y = rnorm(n = nM,mean = aquaXY$y[swarmIx],sd = swarmSD))
#     swarmWt = rgamma(n = nM,1,1); swarmWt = swarmWt / sum(swarmWt)
#     swarmHaz = rbeta(n = nM,99,1)
#     #make swarming sites
#     swarmSites = vector(mode="list",length=nM)
#     for(i in 1:nM){
#       swarmSites[[i]] = makeSwarmingSite(siteIx = i,siteXY = c(swarmXY$x[i],swarmXY$y[i]),searchWt = swarmWt[i],landHaz = swarmHaz[i])
#     }
#
#     landscape$nM = nM
#     landscape$swarmSites = swarmSites
#   }
#
#   return(landscape)
# }
