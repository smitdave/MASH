#################################################################
#
#   MASH
#   R6-ified
#   Auxiliary Functions for 'Emerge' model of aquatic ecology
#   David Smith, Hector Sanchez, Sean Wu
#   May 19, 2016
#
#################################################################

# THESE ARE GONNA BE METHODS TO ADD TO THE APPROPRIATE CLASS

# #oneDay_emerge: add emerging adults to ImagoQ
# #tNow: current daily time tick of model
# oneDay_emerge <- function(tNow){
#   lambdaEmerge = oneDay_emerge_MACRO
#
#   #add emerging adults to ImagoQ
#   for(ix in 1:LANDSCAPE$nA){
#     addAdults2Q(lambda = lambdaEmerge[ix],t = tNow,ix = ix,dam = 0,sire = 0)
#   }
#   browser()
# }
#
# PROBABLY ADD THIS ONE TO MacroPatch MACRO-Patch-Class.R
# oneDay_emerge_MACRO <- function(tNow){
#   lambdaExact = sapply(LANDSCAPE$aquaSites,function(x){x$season[floor(tNow)%%365+1]}) #exact emergence rates
#
#   nSites = length(lambdaExact)
#   rpois(n = nSites,lambda = lambdaExact)
# }


#' MICRO: Generate Seasonal Emergence (Lambda) for \code{Emerge} model of Aquatic Ecology
#'
#' Generate lambda for all sites.
#'
#' @param aquaPars a list of the following structure
#'  * N: number of aquatic habitats (required)
#'  * lambda: number of emerging adult females per human per day averaged over one year for the entire \code{\link{Landscape}} or \code{\link{MacroPatch}} (required)
#'  * lambdaWeight: vector of weights applied to each site (if not specified or set to \code{NULL} initialize to Gamma(1,1) distribution)
#'  * offset: vector of seasonal offsets in peak emergence applied to each site (if not specified or set to \code{NULL} initialize to 0 for all sites)
#' @md
#' @return list \code{lambda} where each element is the daily emergence for that \code{\link{FeedingSite}}
#' @examples
#' makeLambda_Micro(aquaPars= list(lambda = c(2,3,4)))
#' @export
makeLambda_Micro <- function(aquaPars){

  with(aquaPars,{

    if(!exists("lambdaWeight",inherits = FALSE) || is.null(lambdaWeight)){lambdaWeight = rgamma(n = N,shape = 1,rate = 1)}

    K = lambda*lambdaWeight / sum(lambdaWeight)
    if(!exists("offset",inherits = FALSE) || is.null(lambdaWeight)){offset = rep(0,length=N)}

    lambdaOut = vector(mode="list",length=N)
    for(ix in 1:N){
      lambdaOut[[ix]] = K[ix]*(1+sin(2*pi*(c(1:365)-offset[ix])/365))
    }

    return(lambdaOut)
  })

}


#' MACRO: Generate Seasonal Emergence (Lambda) for \code{Emerge} model of Aquatic Ecology
#'
#' Generate lambda for all patches.
#'
#' @param aquaPars a list of the following structure
#'  * lambda: vector of length equal to number of patches \code{\link{MacroPatch}} where each element is the number of emerging adult females per human per day averaged over one year
#'  * lambdaWeight: vector of weights applied to each site (if not specified or set to \code{NULL} initialize to Gamma(1,1) distribution)
#'  * offset: vector of seasonal offsets in peak emergence applied to each site (if not specified or set to \code{NULL} initialize to 0 for all sites)
#' @md
#' @return list \code{lambda} where each element is the daily emergence for that \code{\link{MacroPatch}}
#' @examples
#' makeLambda_Macro(aquaPars = list(lambda=c(5,10,15)))
#' @export
makeLambda_Macro <- function(aquaPars){

  with(aquaPars,{

    N = length(lambda)
    if(!exists("lambdaWeight",inherits = FALSE) || is.null(lambdaWeight)){lambdaWeight = rgamma(n = N,shape = 1,rate = 1)}

    K = lambda*lambdaWeight / sum(lambdaWeight)
    if(!exists("offset",inherits = FALSE) || is.null(lambdaWeight)){offset = rep(0,length=N)}

    lambdaOut = vector(mode="list",length=N)
    for(ix in 1:N){
      lambdaOut[[ix]] = K[ix]*(1+sin(2*pi*(c(1:365)-offset[ix])/365))
    }

    return(lambdaOut)

  })

}
