#################################################################
#
#   MASH
#   R6-ified
#   Auxiliary Functions for 'Emerge' model of aquatic ecology
#   Sean Wu
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


#' Generate Seasonal Emergence (Lambda) for \code{Emerge} model of Aquatic Ecology
#'
#' Write me!
#'
#' @param lambda vector of length equal to number of sites/patches where each element is the number of emerging adult females per human per day averaged over one year
#' @param lambdaWeight vector of weights applied to each site
#' @param offset seasonal offset in peak emergence
#' @return does stuff
#' @examples
#' aquaEmerge_makeLambda(lambda = c(2,3,4))
#' @export
aquaEmerge_makeLambda <- function(lambda, lambdaWeight = NULLs, offset = 0){

  N = length(lambda)
  if(is.null(lambdaWeight)){lambdaWeight = rgamma(n = N,shape = 1,rate = 1)}

  K = lambda*lambdaWeight / sum(lambdaWeight)
  offset = rep(offset,length=N)

  lambda = vector(mode="list",length=N)
  for(ix in 1:N){
    lambda[[ix]] = K[ix]*(1+sin(2*pi*(c(1:365)-offset[ix])/365))
  }

  return(lambda)
}
