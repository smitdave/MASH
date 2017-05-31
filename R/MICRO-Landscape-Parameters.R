####################################################################################
#
#   MASH
#   R6-ified
#   Minimal Landscape Parameters for Well-mixed Patch
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   May 9, 2017
#
####################################################################################


####################################################################################
# Point Clustering Patterns
####################################################################################

#' Generate Poisson Point Pattern for Landscape Sites
#'
#' This function is a low-level utility to generate spatial point patterns for the MICRO-LANDSCAPE point sets.
#'
#' @param n number of points to generate
#' @param xLim a length 2 numeric vector of bounds for the sampling grid
#' @param yLim a length 2 numeric vector of bounds for the sampling grid
#' @return a list with two elements x and y corresponding to the sampled points
#' @examples
#' pointsPoisson(n=10, xLim=c(0,1), yLim=c(0,1))
#' @export
pointsPoisson <- function(n, xLim=c(0,1), yLim=c(0,1)){
  ps = spatstat::rpoispp(lambda = n,win = spatstat::owin(xLim,yLim))
  while(ps$n != n){
    ps = spatstat::rpoispp(lambda = n,win = spatstat::owin(xLim,yLim))
  }

  return(list(x=ps$x,y=ps$y))
}

#' Generate Matern Clustering Point Pattern for Landscape Sites
#'
#' This function is a low-level utility to generate spatial point patterns for the MICRO-LANDSCAPE point sets.
#'
#' @param n number of points to generate
#' @param meanParents intensity of Poisson process for cluster centers
#' @param clusteredness control mean scatter of child points around cluster centers
#' @param xLim a length 2 numeric vector of bounds for the sampling grid
#' @param yLim a length 2 numeric vector of bounds for the sampling grid
#' @return a list with two elements x and y corresponding to the sampled points
#' @examples
#' pointsClustered(n=10, meanParents = 10, clusteredness = .25, xLim=c(0,1), yLim=c(0,1))
#' @export
pointsClustered <- function(n, meanParents = 10, clusteredness = .25, xLim=c(0,1), yLim=c(0,1)){
  meanDist = clusteredness / sqrt(meanParents)
  meanChildren = n / meanParents

  ps = spatstat::rMatClust(meanParents, meanDist, meanChildren, win = spatstat::owin(xLim,yLim))
  while(ps$n != n){
    ps = spatstat::rMatClust(meanParents, meanDist, meanChildren, win = spatstat::owin(xLim,yLim))
  }

  return(list(x=ps$x,y=ps$y))
}

#' Generate Overdispersed (SSI) Point Pattern for Landscape Sites
#'
#' This function is a low-level utility to generate spatial point patterns for the MICRO-LANDSCAPE point sets.
#'
#' @param n number of points to generate
#' @param inhibitionFactor controls level of overdispersion (higher values correspond to a more overdispersed spatial point process)
#' @param xLim a length 2 numeric vector of bounds for the sampling grid
#' @param yLim a length 2 numeric vector of bounds for the sampling grid
#' @return a list with two elements x and y corresponding to the sampled points
#' @examples
#' pointsOverdispersed(n=10, inhibitionFactor = 1, xLim=c(0,1), yLim=c(0,1))
#' @export
pointsOverdispersed <- function(n, inhibitionFactor = 1, xLim=c(0,1), yLim=c(0,1)){
  ps = spatstat::rSSI(inhibitionFactor / sqrt(n), n, win = spatstat::owin(xLim,yLim))
  while(ps$n != n){
    inhibitionFactor = inhibitionFactor - .01
    ps = spatstat::rSSI(inhibitionFactor / sqrt(n), n, win = spatstat::owin(xLim,yLim))
  }

  return(list(x=ps$x,y=ps$y))
}

#' Generate Regular Grid (Lattice) Point Pattern for Landscape Sites
#'
#' This function is a low-level utility to generate spatial point patterns for the MICRO-LANDSCAPE point sets.
#'
#' @param n number of points to generate
#' @param xLim a length 2 numeric vector of bounds for the sampling grid
#' @param yLim a length 2 numeric vector of bounds for the sampling grid
#' @return a list with two elements x and y corresponding to the sampled points
#' @examples
#' pointsLattice(n=10, xLim=c(0,1), yLim=c(0,1))
#' @export
pointsLattice <- function(n, xLim=c(0,1), yLim=c(0,1)){
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


####################################################################################
# Hazards & Search Weights
####################################################################################

#' Find shape2 (beta) Parameter of Beta Distribution for Given Mean
#'
#' Given a user-specified mean value, \code{betaRootB} uses \code{\link{uniroot}} to find the shape2 (beta)
#' parameter of the distribution that will give that mean. Parameter shape1 may also be given as user input, but has
#' a default value of 1.
#'
#' @param mean the mean of the beta distribution
#' @param alpha = 1 alpha parameter of beta distribution
#' @return numeric value
#' @examples
#' betaRootB(mean = 0.05, alpha = 1)
#' @export
betaRootB <- function(mean, alpha = 1){
  rootOut = uniroot(f = function(x,alpha,mean){
      (alpha/(alpha+x)) - mean
  },interval = c(0,1e12),mean=mean,alpha=alpha)
  return(rootOut$root)
}

#' Find shape1 (alpha) Parameter of Beta Distribution for Given Mean
#'
#' Given a user-specified mean value, \code{betaRootA} uses \code{\link{uniroot}} to find the shape1 (alpha)
#' parameter of the distribution that will give that mean. Parameter shape2 may also be given as user input, but has
#' a default value of 1.
#'
#' @param mean the mean of the beta distribution
#' @param beta beta parameter of the beta distribution
#' @return numeric value
#' @examples
#' betaRootA(mean = 0.95, beta = 20)
#' @export
betaRootA <- function(mean, beta = 1){
  rootOut = uniroot(f = function(x,beta,mean){
      (x/(x+beta)) - mean
  },interval = c(0,1e12),mean=mean,beta=beta)
  return(rootOut$root)
}


####################################################################################
# Parameter Generation Functions
####################################################################################

#' MICRO: Generate Parameters for Landscape Object
#'
#' This function is a specific instantiation of a generic system to generate parameters for a
#' chosen landscape. Any user-specified function can be written to generate parameters, as long as the
#' return list is in the same format.
#'
#' @param nFeed number of feeding sites (generated via pointGen(...))
#' @param nAqua number of aquatic habitats
#' @param pointGen character to select spatial point pattern generation function
#'  * "poisson": \code{\link{pointsPoisson}}
#'  * "clustered": \code{\link{pointsClustered}}
#'  * "overdispersed": \code{\link{pointsOverdispersed}}
#'  * "lattice": \code{\link{pointsLattice}}
#' @param hhSize average number of humans at feeding sites
#' @param hhMin minimum number of humans at feeding sites
#' @param lambda mean emerging females per human per day for entire landscape summing out seasonality
#' @param offset seasonal offset
#' @param hazV mean value for feeding site vegetation landing hazard (if 0 it is set to 0 for all sites)
#' @param hazW mean value for feeding site outside wall landing hazard (if 0 it is set to 0 for all sites)
#' @param hazI mean value for feeding site indoor wall landing hazard (if 0 it is set to 0 for all sites)
#' @param haz mean value for aquatic habitat landing hazard (if 0 it is set to 0 for all sites)
#' @param xLim x-axis bounds for simulated points
#' @param yLim y-axis bounds for simulated points
#' @param aquaSD standard deviation of aquatic habitat scatter around feeding sites
#' @param ... additional named arguments for pointGen()
#' @return a named list of parameters
#' * nFeed: number of feeding sites
#' * nAqua: number of aquatic habitats
#' * feedXY: 2 element list (x and y coords, respectively)
#' * feedWt: search weight for feeding sites
#' * sugar: opportunistic sugar weight at feeding sites
#' * enterP: probability to enter house
#' * hazV: feeding site vegetation landing hazards
#' * hazW: feeding site outside wall landing hazards
#' * hazI: feeding site indoor wall landing hazards
#' * humanPop: size of human population at each site
#' * humanPopIx: id of humans at each site
#' * humanPopN: total population size
#' * aquaIx: 'parent' feeding sites of aquatic habitats
#' * aquaXY: 2 element list (x and y coords, respectively)
#' * aquaWt: search weight for aquatic habitats
#' * haz: landing hazard for aquatic habitats
#' @md
#' @examples
#'
#' @export
Landscape.PAR <- function(
    nFeed,
    nAqua,
    pointGen = "poisson",
    hhSize,
    hhMin,
    lambda,
    offset = 0,
    hazV = 0,
    hazW = 0,
    hazI = 0,
    haz = 0,
    enterP = 1,
    xLim = c(0,1),
    yLim = c(0,1),
    aquaSD = 0.025,
    ...
  ){

    PAR = list()

    PAR$nFeed = nFeed
    PAR$nAqua = nAqua

    ##############################################
    # feeding site parameters
    ##############################################

    # spatial
    PAR$feedXY = pointGen(nFeed, ...)
    PAR$feedWt = rgamma(n = nFeed,1,1); PAR$feedWt = PAR$feedWt / sum(PAR$feedWt)

    # sugar
    PAR$sugar = rgamma(n = nFeed,1,1)

     # house entry
    if(enterP!=1){
      alpha = betaRootA(mean = hazV, beta = 20)
      PAR$enterP = rbeta(n = nFeed, shape1 = alpha, shape2 = 20)
    } else {
      PAR$enterP = rep(1,nFeed)
    }

    # landing hazards
    if(hazV!=0){ # vegetation landing hazards
      beta = betaRootB(mean = hazV, alpha = 1)
      PAR$hazV = rbeta(n = nFeed, shape1 = 1, shape2 = beta)
    } else {
      PAR$hazV = rep(0,nFeed)
    }
    if(hazW!=0){ # outside wall landing hazards
      beta = betaRootB(mean = hazW, alpha = 1)
      PAR$hazW = rbeta(n = nFeed, shape1 = 1, shape2 = beta)
    } else {
      PAR$hazW = rep(0,nFeed)
    }
    if(hazI!=0){ # indoor wall landing hazards
      beta = betaRootB(mean = hazI, alpha = 1)
      PAR$hazI = rbeta(n = nFeed, shape1 = 1, shape2 = beta)
    } else {
      PAR$hazI = rep(0,nFeed)
    }

    # distribute humans
    hh = hhMin + rpois(n = nFeed,lambda = hhSize - hhMin) # size of human population at each site
    nH = sum(hh) #number of humans
    hhIx = vector(mode="list",length=nFeed) #id of humans at each site
    hhIxI = 0
    for(i in 1:nFeed){
      hhIx[[i]] = (hhIxI+1):(hhIxI+hh[i])
      hhIxI = hhIxI+hh[i]
    }
    PAR$humanPop = hh # size of human population at each site
    PAR$humanPopIx = hhIx # id of humans at each site
    PAR$humanPopN = nH # total population size

    ##############################################
    # aquatic habitat parameters
    ##############################################

    # randomly distribute aquatic habitats nearby feeding site 'parents'
    PAR$aquaIx = (1:nFeed)[sample(x = 1:nFeed,size = nAqua, replace = TRUE)]
    PAR$aquaXY = list(
        x = rnorm(n = nAqua,mean = PAR$feedXY$x[PAR$aquaIx],sd = aquaSD),
        y = rnorm(n = nAqua,mean = PAR$feedXY$y[PAR$aquaIx],sd = aquaSD)
      )
    PAR$aquaWt = rgamma(n = nAqua,1,1); PAR$aquaWt = PAR$aquaWt / sum(PAR$aquaWt)

    # emergence
    w = rgamma(n = nAqua,1,1)
    K = lambda*w / sum(w)
    offset = rep(offset,length=nAqua)
    PAR$lambda = vector(mode="list",length=nAqua)
    for(ix in 1:nAqua){
      PAR$lambda[[ix]] = K[ix]*(1+sin(2*pi*(c(1:365)-offset[ix])/365))
    }

    # landing hazards
    if(haz!=0){ # aquatic habitat landing hazards
      alpha = betaRootA(mean = haz, beta = 1)
      PAR$haz = rbeta(n = nAqua, shape1 = alpha, shape2 = 1)
    } else {
      PAR$haz = rep(0,nAqua)
    }

    return(PAR)
}


#' MICRO: Generate Parameters for \code{Landscape} \code{AquaticSite}
#'
#' This function generates a named list of parameters to initialize all \code{\link{AquaticSite}} objects on a MICRO \code{\link{Landscape}}.
#'
#' @param nAqua number of aquatic habitats
#' @param siteXY two element list of \code{x} and \code{y} coordinates of aquatic habitats
#' @param module character
#'  * "emerge": initialize parameters for Emerge module of Aquatic Ecology
#'  * "EL4P": initialize parameters for EL4P module of Aquatic Ecology
#' @param modulePars additional list of named parameters to be passed to Aquatic Ecology module specific parameter generating functions
#'  * Emerge: see for deatils \code{\link{aquaEmerge_makeLambda}}
#'  * EL4P:
#' @param searchWt vector of searchWt (if \code{NULL} initialize to Gamma(1,1) distribution)
#' @param haz mean value of landing hazards (if \code{!= 0} use \code{\link{betaRootA}} to find alpha parameter of beta distribution to give that mean value and produce Beta distributed hazards)
#' @return return a list
#' @md
#' @export
Landscape.Aqua.PAR <- function(nAqua, siteXY, module , modulePars, searchWt = NULL, haz = 0){

  Landscape_Aqua_PAR = list()
  Landscape_Aqua_PAR$nAqua = nAqua
  Landscape_Aqua_PAR$siteXY = siteXY

  # Search Weights and Landing Hazards
  if(is.null(searchWt)){
    Landscape_Aqua_PAR$searchWt = rgamma(n=nAqua,1,1)
  } else {
    Landscape_Aqua_PAR$searchWt = searchWt
  }

  if(haz!=0){
    alpha = betaRootA(mean = haz, beta = 1)
    Landscape_Aqua_PAR$haz = rbeta(n = nAqua, shape1 = alpha, shape2 = 1)
  } else {
    Landscape_Aqua_PAR$haz = rep(0,nAqua)
  }

  # Aquatic Ecology modules
  if(module == "emerge"){

    Landscape_Aqua_PAR$lambda = aquaEmerge_makeLambda(modulePars)

  } else {
    stop("sean hasnt written EL4P or any other AQUA modules yet!")
  }


  return(Landscape_Aqua_PAR)

}


#' MICRO: Generate Parameters for \code{Landscape} \code{AquaticSite}
#'
#' This function generates a named list of parameters to initialize all \code{\link{AquaticSite}} objects on a MICRO \code{\link{Landscape}}.
#'
#' @param nFeed number of feeding sites
#' @param pointGen character to select spatial point pattern generation function
#'  * "poisson": \code{\link{pointsPoisson}}
#'  * "clustered": \code{\link{pointsClustered}}
#'  * "overdispersed": \code{\link{pointsOverdispersed}}
#'  * "lattice": \code{\link{pointsLattice}}
#' @param searchWt vector of searchWt (if \code{NULL} initialize to Gamma(1,1) distribution)
#' @param enterP vector of searchWt (if \code{NULL} initialize to Gamma(1,1) distribution)
#' @param hazV mean value for feeding site vegetation landing hazard (if 0 it is set to 0 for all sites)
#' @param hazW mean value for feeding site outside wall landing hazard (if 0 it is set to 0 for all sites)
#' @param hazI mean value for feeding site indoor wall landing hazard (if 0 it is set to 0 for all sites)
#' @param ... additional named arguments to be passed to the pointGen(nFeed, ...) function
#' @return return a list
#' @md
#' @export
Landscape.Feeding.PAR <- function(nFeed, pointGen = "poisson", searchWt = NULL, enterP = 1, ...){

  Landscape_Feeding_PAR = list()
  Landscape_Feeding_PAR$nFeed = nFeed

  switch(pointGen,
    "poisson" = Landscape_Feeding_PAR$siteXY <- pointsPoisson(nFeed, ...),
    "clustered" = Landscape_Feeding_PAR$siteXY <- pointsClustered(nFeed, ...),
    "overdispersed" = Landscape_Feeding_PAR$siteXY <- pointsOverdispersed(nFeed, ...),
    "lattice" = Landscape_Feeding_PAR$siteXY <- pointsLattice(nFeed, ...)
  )

  # Search Weights and Landing Hazards
  if(is.null(searchWt)){
    Landscape_Feeding_PAR$searchWt = rgamma(n=nAqua,1,1)
  } else {
    Landscape_Feeding_PAR$searchWt = searchWt
  }

  if(hazV!=0){ # vegetation landing hazards
    beta = betaRootB(mean = hazV, alpha = 1)
    Landscape_Feeding_PAR$hazV = rbeta(n = nFeed, shape1 = 1, shape2 = beta)
  } else {
    Landscape_Feeding_PAR$hazV = rep(0,nFeed)
  }
  if(hazW!=0){ # outside wall landing hazards
    beta = betaRootB(mean = hazW, alpha = 1)
    Landscape_Feeding_PAR$hazW = rbeta(n = nFeed, shape1 = 1, shape2 = beta)
  } else {
    Landscape_Feeding_PAR$hazW = rep(0,nFeed)
  }
  if(hazI!=0){ # indoor wall landing hazards
    beta = betaRootB(mean = hazI, alpha = 1)
    Landscape_Feeding_PAR$hazI = rbeta(n = nFeed, shape1 = 1, shape2 = beta)
  } else {
    Landscape_Feeding_PAR$hazI = rep(0,nFeed)
  }

  # sugar
  Landscape_Feeding_PAR$sugar = rgamma(n = nFeed,1,1)

  sitePops()
}





# ##############################################
# # feeding site parameters
# ##############################################
#
# # spatial
# PAR$feedXY = pointGen(nFeed, ...)
# PAR$feedWt = rgamma(n = nFeed,1,1); PAR$feedWt = PAR$feedWt / sum(PAR$feedWt)
#
# # sugar
# PAR$sugar = rgamma(n = nFeed,1,1)
#
#  # house entry
# if(enterP!=1){
#   alpha = betaRootA(mean = hazV, beta = 20)
#   PAR$enterP = rbeta(n = nFeed, shape1 = alpha, shape2 = 20)
# } else {
#   PAR$enterP = rep(1,nFeed)
# }
#
# # landing hazards
# if(hazV!=0){ # vegetation landing hazards
#   beta = betaRootB(mean = hazV, alpha = 1)
#   PAR$hazV = rbeta(n = nFeed, shape1 = 1, shape2 = beta)
# } else {
#   PAR$hazV = rep(0,nFeed)
# }
# if(hazW!=0){ # outside wall landing hazards
#   beta = betaRootB(mean = hazW, alpha = 1)
#   PAR$hazW = rbeta(n = nFeed, shape1 = 1, shape2 = beta)
# } else {
#   PAR$hazW = rep(0,nFeed)
# }
# if(hazI!=0){ # indoor wall landing hazards
#   beta = betaRootB(mean = hazI, alpha = 1)
#   PAR$hazI = rbeta(n = nFeed, shape1 = 1, shape2 = beta)
# } else {
#   PAR$hazI = rep(0,nFeed)
# }
#
# # distribute humans
# hh = hhMin + rpois(n = nFeed,lambda = hhSize - hhMin) # size of human population at each site
# nH = sum(hh) #number of humans
# hhIx = vector(mode="list",length=nFeed) #id of humans at each site
# hhIxI = 0
# for(i in 1:nFeed){
#   hhIx[[i]] = (hhIxI+1):(hhIxI+hh[i])
#   hhIxI = hhIxI+hh[i]
# }
# PAR$humanPop = hh # size of human population at each site
# PAR$humanPopIx = hhIx # id of humans at each site
# PAR$humanPopN = nH # total population size
