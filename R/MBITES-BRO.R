#################################################################
#
#   MASH
#   R6-ified
#   MBITES-BRO (Blood Feeding, Resting, Oviposition)
#   David Smith, Hector Sanchez, Sean Wu
#   June 5, 2017
#
#################################################################


#################################################################
# MBITES-BRO: timingExponential
#################################################################

#' MBITES-BRO: Exponential Timing for \code{MicroMosquitoFemale}
#'
#' Method for exponentially-distributed bout lengths (model mosquito walk through state space as a strictly Markovian process).
#'  * This method is bound to \code{MicroMosquitoFemale$timingExponential()}.
#'
#' @md
mbitesBRO_timingExponential <- function(){
  if(self$isActive()){
    if(private$state == "B" && private$lspot == 1L){duration = private$FemalePopPointer$get_MBITES_PAR("F_time")}
    if(private$state == "B" && private$lspot != 1L){duration = private$FemalePopPointer$get_MBITES_PAR("B_time")}
    if(private$state == "O" && private$lspot == 1L){duration = private$FemalePopPointer$get_MBITES_PAR("L_time")}
    if(private$state == "O" && private$lspot != 1L){duration = private$FemalePopPointer$get_MBITES_PAR("O_time")}
    if(private$state == "R"){duration = private$FemalePopPointer$get_MBITES_PAR("R_time")}
    private$tNext = private$tNow + rexp(n=1,rate=1/duration)
  }
}


#################################################################
# MBITES-BRO: Landing
#################################################################

#' MBITES-BRO: Return Landing Spot Weights for \code{MicroMosquitoFemale}
#'
#' Method for return landing spot weights based on behavioral state of mosquito.
#'  * This method is bound to \code{MicroMosquitoFemale$getWTS()}.
#'
#' @md
#' @return vector of landing spot weights
mbitesBRO_getWTS <- function(){
  switch(private$state,
    B = private$FemalePopPointer$get_MBITES_PAR("B_wts"),
    R = private$FemalePopPointer$get_MBITES_PAR("R_wts"),
    O = private$FemalePopPointer$get_MBITES_PAR("O_wts")
  )
}

#' MBITES-BRO: Land After Flight \code{MicroMosquitoFemale}
#'
#' Mosquito lands after a flight, which may cause various events.
#' This function always calls \code{\link{mbitesGeneric_newSpot}} and may call \code{\link{mbitesGeneric_enterHouse}}
#'  * This method is bound to \code{MicroMosquitoFemale$landingSpot()}.
#'
#' @md
mbitesBRO_landingSpot <- function(){
  if(self$isActive()){
    oldSpot = private$lspot
    self$newSpot() # choose new lspot
    if(oldSpot != 5L & private$lspot == 5L){
      self$enterHouse() # enterHouse
    }
  }
}


#################################################################
# Initialize Methods and Fields (Setup)
#################################################################

#' MBITES-BRO: Initialize Additional Methods & Fields in \code{MicroMosquitoPop} and \code{MicroMosquito}
#'
#' Initialize M-BITES BRO (Blood Feeding, Resting, Oviposition) lifecycle model.
#'
#' @param overwrite overwrite methods
#' @examples
#' mbitesBRO.Setup()
#' @export
mbitesBRO.Setup <- function(overwrite = TRUE){

    message("initializing M-BITES generic shared methods")

    MicroMosquitoFemale$set(which = "public",name = "timingExponential",
              value = mbitesBRO_timingExponential,
              overwrite = overwrite
    )

}
