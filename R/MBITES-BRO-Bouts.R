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
# MBITES-BRO: Blood Feeding Bout
#################################################################




#################################################################
# MBITES-BRO: Generic Bout
#################################################################

#' MBITES-BRO: One Bout \code{MicroMosquitoFemale}
#'
#' Mosquito behavior has a finite set of states (state space of model), within which there are certain biological functions that are always evaluated.
#' The generic bout runs necessary updates of timing, state, survival, energetics, and queue checks prior to calling the nested
#' specific bout action, and checks that the mosquito is alive/active before calling the bout. It updates \code{tNext} and \code{stateNew}.
#'
#' This corresponds to the following Gillespie-style algorithm:
#'
#' 1. tNow is set to tNext from previous bout
#' 2. rMove: movement between point classes (if needed)
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
    O = {self$boutO()}
  )

  # energetics
  self$energetics()

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
