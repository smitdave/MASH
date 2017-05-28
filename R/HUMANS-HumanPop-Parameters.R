#################################################################
#
#   MASH
#   R6-ified
#   HumanPop Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   May 26, 2017
#
#################################################################

#' Initialize HumanPop Parameters for \code{HumanPop}
#'
#' make a list of pars for \code{\link{HumanPop}}
#'
#' @param nSite number of sites (either \code{\link{FeedingSite}} or \code{link{MacroPatch}})
#' @param demographics either a list or \code{NULL}; if \code{NULL} then \code{\link{sitePops}} is used to generate these parameters, see that function for details on the list structure that is expected.
#' @param ... additional named arguments for \code{\link{sitePops}}
#' @return return a list \code{HumanPop_PAR}
#' @examples
#' HumanPop.Parameters(nSite = 5)
#' @export
HumanPop.Parameters <- function(nSite, demographics = NULL,...){

  if(is.null(demographics)){
    demographics = sitePops(N=nSite,...)
  }

  bitingWeights = rgamma(demographics$nHumans,1,1)

  list(
    nHumans = demographics$nHumans,
    tStart = 0,
    humanIDs = Reduce(f = c,x = demographics$siteHumanID),
    homeIDs = demographics$homeHumanID,
    bDay = -Reduce(f = c,x = demographics$siteAges),
    bWeight = bitingWeights
  )
}
