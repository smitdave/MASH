#################################################################
#
#   MASH
#   R6-ified
#   MBITES General Method Definitions
#   David Smith, Hector Sanchez, Sean Wu
#   May 2, 2017
#
#################################################################


##############################################################
# Checks of Life Status
##############################################################

#' MBITES: Generic Alive Check for \code{MicroMosquito}
#'
#' Check if this mosquito is alive and return a logical value.
#'  * This method is bound to \code{MicroMosquito$isAlive()}.
#' @md
mbitesGeneric_isAlive <- function(){
  if(private$stateNew == "D" || private$state == "D"){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' MBITES: Generic Active Check for \code{MicroMosquito}
#'
#' Check if this mosquito is active and return a logical value.
#'  * This method is bound to \code{MicroMosquito$isActive()}.
#' @md
mbitesGeneric_isActive <- function(){
  if(private$state == "E"){
    return(FALSE)
  } else {
    return(self$isAlive())
  }
}


##############################################################
#
#  HOUSE ENTERING & RESTING BEHAVIOR:
#  At the end of the search bout, attempt bout, or after Egg
#  laying a mosquito has entered the area around a feeding
#  station and either rested or attempted to rest:
#    l) Leave the area
#    r) Reattempt Without Resting;
#    v) Rest on vegetation
#    w) Rest on the Outside wall of a structure
#    i) Rest on the Inside wall of a structure
#
##############################################################

#' M-BITES (All Modules): Generate New Landing Spot for \code{MicroMosquitoFemale}
#'
#' Method for return a new landing spot based on behavioral state of mosquito and weights from \code{\link{mbitesBRO_getWTS}}.
#' New landing spots generated at the end of the search bout, attempt bout, or after oviposition a mosquito has entered
#' the area around a feeding site and either rested or attempted to rest.
#'  * l: 1 leave the area
#'  * r: 2 reattempt without resting
#'  * v: 3 rest on vegetation
#'  * w: 4 rest on the outside wall of a structure
#'  * i: 5 rest on the inside wall of a structure
#'
#'  * This method is bound to \code{MicroMosquitoFemale$newSpot()}.
#'
#' @md
#' @return integer value corresponding to new landing spot
mbitesGeneric_newSpot <- function(){
  probs = private$FemalePopPointer$get_MBITES_PAR("InAndOut")[private$lspot,] * self$getWTS()
  return(
    sample(x = 5L,size = 1,prob = probs)
  )
}

#' M-BITES (All Modules): Attempt to Enter a House for \code{MicroMosquitoFemale}
#'
#' Method to simulate attempted house entry for mosquito, and call appropriate events if the mosquito enters.
#'  * This method is bound to \code{MicroMosquitoFemale$enterHouse()}.
#'
#' @md
mbitesGeneric_enterHouse <- function(){
  if(runif(1) < private$LandscapePointer$get_FeedingSites(private$ix)$get_enterP()){
    # mosquito is inside of house
    # private$LandscapePointer$get_FeedingSites(private$ix)$indoorVectorControl()
  } else {
    # mosquito is not inside of house
    private$lspot = self$newSpot()
    self$surviveFlight()
    if(private$lspot == 1L){
      print("using Recall on enterHouse!") # DEBUG
      Recall()
    }
  }
}
