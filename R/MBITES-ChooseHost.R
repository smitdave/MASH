#################################################################
#
#   MASH
#   M-BITES
#   Host Choosing Methods
#   David Smith, Hector Sanchez, Sean Wu
#   July 28, 2017
#
#################################################################

#' MBITES-Generic: Host Choosing for \code{\link{MicroMosquitoFemale}}
#'
#' Choose a human or animal host at a site.
#'  * This method is bound to \code{MicroMosquitoFemale$chooseHost()}.
#' @md
mbitesGeneric_chooseHost <- function(){

  if(private$inPointSet != "f"){ #check M is in a feeding site
    stop(paste0("chooseHost error; mosy ",M$id," inPointSet: ",M$inPointSet," , not in a feeding site"))
  }

  # this can probably eventually be put into C++; see the wiki
  whoIx = private$LandscapePointer$get_FeedingSites(private$ix)$get_RiskQ()$get_who()
  pTmIx = private$LandscapePointer$get_FeedingSites(private$ix)$get_RiskQ()$get_pTm()
  wIx = private$LandscapePointer$get_FeedingSites(private$ix)$get_RiskQ()$get_w()

  # non-human hosts
  nO = private$LandscapePointer$get_FeedingSites(private$ix)$get_RiskQ()$get_nOther()
  if(nO > 0){
    otherHosts = private$LandscapePointer$get_FeedingSites(private$ix)$get_RiskQ()$get_OtherHost()
    for(i in 1:nO){
      pTmIx = c(pTmIx,1)
      whoIx = c(whoIx,otherHosts$typeID[i])
      wIx = c(wIx,otherHosts$otherW[i])
    }
  }

  private$hostID = sample(x = whoIx,size = 1,prob = wIx*pTmIx) #select a host
}
