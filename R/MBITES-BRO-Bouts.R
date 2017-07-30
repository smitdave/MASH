#################################################################
#
#   MASH
#   R6-ified
#   MBITES-BRO (Blood Feeding, Resting, Oviposition)
#   Main Bout Methods & Auxiliary Definitions
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
    duration = switch(private$state,
      B = {private$FemalePopPointer$get_MBITES_PAR("B_time")},
      R = {private$FemalePopPointer$get_MBITES_PAR("R_time")},
      O = {private$FemalePopPointer$get_MBITES_PAR("O_time")}
    )
    private$tNext = private$tNow + rexp(n=1,rate=1/duration)
  }
}


#################################################################
#  MBITES-BRO: Post-bout Landing, House Entering, and Resting
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
#################################################################

#' MBITES-BRO: Return Site Type \code{MicroMosquitoFemale}
#'
#' Method to return integer corresponding to site type of \code{\link{MicroSite}} this mosquito is currently at.
#'  Site Types:
#'  * 1: peri-domestic site
#'  * 0: not peri-domestic site
#'
#'  * This method is bound to \code{MicroMosquitoFemale$getMySiteType()}.
#'
#' @md
#' @return vector of landing spot weights
mbitesBRO_getMySiteType <- function(){
  switch(private$inPointSet,
    f = {return(private$LandscapePointer$get_FeedingSites(private$ix)$get_siteType())},
    l = {return(private$LandscapePointer$get_AquaSites(private$ix)$get_siteType())},
    {stop("illegal point set for MBITES-BRO")}
  )
}

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

#' MBITES-BRO: Generate New Landing Spot for \code{MicroMosquitoFemale}
#'
#' Method for return a new landing spot based on behavioral state of mosquito and weights from \code{\link{mbitesBRO_getWTS}}.
#' New landing spots generated at the end of the search bout, attempt bout, or after oviposition a mosquito has entered
#' the area around a feeding site and either rested or attempted to rest.
#'  * i: 1 rest on the inside wall of a structure
#'  * w: 2 rest on the outside wall of a structure
#'  * v: 3 rest on vegetation
#'  * r: 4 reattempt without resting
#'  * l: 5 leave the area
#'
#'  * This method is bound to \code{MicroMosquitoFemale$newSpot()}.
#'
#' @md
#' @return integer value corresponding to new landing spot
mbitesBro_newSpot <- function(){
  if(self$getMySiteType() == 1){
    probs = private$FemalePopPointer$get_MBITES_PAR("InAndOut")[private$lspot,] * self$getWTS()
    sample(x = 5L,size = 1,prob = probs)
  } else {
    return(3L)
  }
}

#' MBITES-BRO: Attempt to Enter a House for \code{MicroMosquitoFemale}
#'
#' Method to simulate attempted house entry for mosquito, and call appropriate events if the mosquito enters.
#'  * This method is bound to \code{MicroMosquitoFemale$enterHouse()}.
#'
#' @md
mbitesBro_enterHouse <- function(){
  if(runif(1) < private$LandscapePointer$get_FeedingSites(private$ix)$get_enterP()){
    # mosquito is inside of house
    print(paste0("mosquito ",private$id," is entering house ",private$ix," at time: ",private$tNow)) # DEBUG
  } else {
    # mosquito is not inside of house
    private$lspot = self$newSpot()
    self$surviveFlight()
    if(private$lspot == 1L){
      print(paste0("mosquito ",private$id," is using Recall to enter house ",private$ix," at time: ",private$tNow)) # DEBUG
      Recall()
    }
  }
}

#' MBITES-BRO: Land After Flight \code{MicroMosquitoFemale}
#'
#' Mosquito lands after a flight, which may cause various events.
#' This function always calls \code{\link{mbitesBro_newSpot}} and may call \code{\link{mbitesBro_enterHouse}}
#'  * This method is bound to \code{MicroMosquitoFemale$landingSpot()}.
#'
#' @md
mbitesBRO_landingSpot <- function(){
  if(self$isActive()){
    oldSpot = private$lspot
    private$lspot = self$newSpot() # choose new lspot
    if(oldSpot != 1L & private$lspot == 1L){
      self$enterHouse() # enterHouse
    }
  }
}


#################################################################
# MBITES-BRO: Blood Feeding Bout
#################################################################


mbitesBRO_boutB <- function(){

}


#################################################################
# MBITES-BRO: Generic Bout
#################################################################

#' MBITES-BRO: One Bout \code{MicroMosquitoFemale}
#'
#' Mosquito behavior has a finite set of states (state space of model), within which there are certain biological functions that are always evaluated.
#' A bout is the actions taken by a mosquito between a launch and landing; \code{mbitesBro_oneBout} handles all the biological imperatives that occur during a bout,
#' while specialized bout action methods handle the events that occur due to the purpose of the bout.
#'  * \code{\link{mbitesBRO_boutB}}: blood feeding bout
#'  * \code{\link{mbitesBRO_boutR}}: blood feeding bout
#'  * \code{\link{mbitesBRO_boutO}}: blood feeding bout
#'
#' The generic bout runs necessary updates of timing, state, survival, energetics, and queue checks prior to calling the nested
#' specific bout action, and checks that the mosquito is alive/active before calling the bout. It updates \code{tNext} and \code{stateNew}.
#'
#' This corresponds to the following Gillespie-style algorithm:
#'
#' 1. tNow is set to tNext from previous bout
#' 2. moveMe: movement between point classes (if needed)
#' 3. boutFun: run bout function
#' 4. run energetics and check if alive
#' 5. run landingSpot and check if alive
#' 6. run surviveResting/surviveFlight and check if alive
#' 7. update tNext
#' 8. update state to stateNew which is determined in the bout
#'
#'  * This method is bound to \code{MicroMosquitoFemale$oneBout()}.
#'
#' @md
mbitesBro_oneBout <- function(){

  # update time and state
  private$tNow = private$tNext # update time
  private$state = private$stateNew # update current state
  self$timingExponential() # update tNext

  # movement
  self$moveMe()

  # landing spot
  self$landingSpot()

  # bout
  switch(private$state,
    B = {self$boutB()},
    R = {self$boutR()},
    O = {self$boutO()},
    {stop("illegal behavioral state for MBITES-BRO")}
  )

  # energetics
  # SUGAR ENERGETICS NOT IMPLEMENTED IN MBITES-BRO
  # self$energetics()

  # survival
  self$surviveResting()
  self$surviveFlight()

  # log history
  private$history$historyTrack(privateEnv = private, alive = self$isAlive())

}


#################################################################
# MBITES-BRO: Simulation
#################################################################

#' MBITES-BRO: Run Simulation for \code{MicroMosquitoFemale}
#'
#' Run the M-BITES life cycle simulation algorithm while alive and has not overrun time in enclosing \code{\link{MicroTile}}.
#' This method calls \code{\link{mbitesBro_oneBout}} to simulate each life stage.
#'  * This method is bound to \code{MicroMosquitoFemale$MBITES()}.
#'
#' @md
mbitesBro_MBITES <- function(){

  # run algorithm while alive and has not overrun tile time
  while(private$tNext < private$TilePointer$get_tNow() && private$stateNew != "D"){
    self$oneBout()
  }

}
