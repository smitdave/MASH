#################################################################
#
#   MASH
#   R6-ified
#   Human Movement-related Methods for MACRO
#   David Smith, Hector Sanchez, Sean Wu
#   May 21, 2017
#
#################################################################


###################################################################
# travelHabit: initialize inter-patch travel
###################################################################

#' Get \code{Human} travel
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
get_travel <- function(){
  return(private$travel)
}

#' Set \code{Human} travel
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
set_travel <- function(travel){
  private$travel = travel
}

#' MACRO \code{Human} Method: travelHabit
#'
#' Write me! a method for \code{\link{Human}}
#'
#' @param n number of other patches to visit
#' @param freqMean meanlog
#' @param freqSd sdlog
#' @return does stuff
#' @examples
#' some_function()
travelHabit <- function(n, freqMean = 7, freqSd = 2, lengthMean = 2, lengthSd = 1, tNow = 0){

  N = self$get_PatchesPointer()$get_N() # how many patches
  here = self$get_patchID() # where is my home?

  there = sample(x = c(1:N)[-here], size = n) # where do i often go?
  howOften = 60+round(rlnorm(n,meanlog=freqMean,sdlog=freqSd))
  meanLengthOfTrip = 1+round(rlnorm(n,meanlog=lengthMean,sdlog=lengthSd))

  # set up the next trip
  tTrip = tNow + rexp(n=1,rate=sum(1/howOften))
  ixTrip = sample(x=c(1:n), size=1, prob=1/howOften)
  away = there[ixTrip]

  PAR = list(there = away, ixTravel = ixTrip)
  self$add2Q_takeTrip(tEvent = tTrip, PAR = PAR)

  if(tNow==0){
    # initialize history with home patchID
    self$track_travel(tTravel=tNow,locationH=private$patchID)
  }

  travel = list(
        randomRate = 1/730,
        nPlaces = n,
        totFreq = sum(1/howOften),
        places_there = there,
        places_frequency = 1/howOften,
        places_length = meanLengthOfTrip
      )
  self$set_travel(travel)
}


###################################################################
# Travel history tracking
###################################################################

#' Track \code{Human} Travel History
#'
#' Write me! Defined in MACRO-Human-Movement.R
#'
#' @param tTravel time of trip
#' @param location destination of trip
#' @return does stuff
#' @examples
#' self$trackTravel(tTravel = 1, location = 1L)
track_travel <- function(tTravel, locationH){
  private$locationH = c(private$locationH, locationH)
  private$tTravel = c(private$tTravel, tTravel)
}

#' Return \code{Human} Travel History
#'
#' Write me! Called as \code{self$get_travelHistory()}  Defined in MACRO-Human-Movement.R
#'
#' @return a list
#' * location: vector of integer locations
#' * tTravel: vector of numeric trip times
#' @md
#' @examples
#' some_function()
get_travelHistoryHuman <- function(){
  list(
    location = private$locationH,
    tTravel = private$tTravel
  )
}

#' Return \code{HumanPop} Travel History
#'
#' Write me! Called as \code{self$get_travelHistory()} Defined in MACRO-Human-Movement.R
#'
#' @return a list
#' @examples
#' some_function()
get_travelHistoryHumanPop <- function(){

  travelHistories = vector(mode = "list", length = self$nHumans)
  for(ixH in 1:self$nHumans){
    travelHistories[[ixH]] = private$pop[[ixH]]$get_travelHistory()
  }
  return(travelHistories)

}


###################################################################
# humanIDs: interface with MACRO:
#   * tell the MacroPatch where you are
###################################################################

#' Set \code{Human} go_Patch
#'
#' Update \code{\link{MacroPatch}} list of humanIDs when a human leaves a site \code{old} and goes to a site \code{new}
#'
#' @param old old patch (integer index)
#' @param new new patch (integer index)
#' @return does stuff
#' @examples
#' some_function()
go_Patch <- function(old, new){
  self$get_PatchesPointer()$add_humanIDs(oneID=private$myID,ix=new)
  self$get_PatchesPointer()$remove_humanIDs(oneID=private$myID,ix=old)
}


###################################################################
# Add MACRO Movement Events to 'Human' Class
# 'XX' family of functions for human event queues
###################################################################

###################################################################
# Take a trip to another patch
###################################################################

#' add2Q_takeTrip
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
add2Q_takeTrip <- function(tEvent, PAR = NULL){
  self$addEvent2Q(event = self$event_takeTrip(tEvent = tEvent, PAR = PAR))
}

#' event_takeTrip
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
event_takeTrip <- function(tEvent, PAR){
  list(tEvent = tEvent, PAR = PAR, tag = "takeTrip")
}

#' takeTrip
#'
#' Write me!
#'
#' @param a parameter
#' @param PAR a list of length two
#' * there ID of patch this human will visit
#' * ixTravel index of the patch this human will visit in their travel destinations
#' @md
#' @return does stuff
#' @examples
#' some_function()
takeTrip <- function(tEvent, PAR){

  # take the trip
  away = PAR$there
  private$location = away
  home = private$patchID

  self$track_travel(tTravel = tEvent, locationH = away) # history

  # update home biting weight
  wHome = self$get_PatchesPointer()$get_bWeightHuman(ix = home) - self$get_bWeight()
  self$get_PatchesPointer()$set_bWeightHuman(bWeightHuman = wHome, ix = home)

  # update visiting patch biting weight
  wAway = self$get_PatchesPointer()$get_bWeightHuman(ix = away) + self$get_bWeight()
  self$get_PatchesPointer()$set_bWeightHuman(bWeightHuman = wAway, ix = away)

  # tell the MacroPatch class where you went
  self$go_Patch(old = home, new = away)

  # queue up the voyage home
  tReturn = tEvent + rexp(n = 1, rate = 1 / self$get_travel()$places_length[PAR$ixTravel])
  self$add2Q_returnHome(tEvent = tReturn, PAR = NULL)
}


###################################################################
# Take a trip to another patch
###################################################################

#' add2Q_returnHome
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
add2Q_returnHome <- function(tEvent, PAR = NULL){
  self$addEvent2Q(event = self$event_returnHome(tEvent = tEvent, PAR = PAR))
}

#' event_returnHome
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
event_returnHome = function(tEvent, PAR){
  list(tEvent = tEvent, PAR = PAR, tag = "returnHome")
}

#' returnHome
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
returnHome = function(tEvent, PAR){

  away = private$location
  home = private$patchID
  private$location = home  # go home

  self$track_travel(tTravel = tEvent, locationH = home) # history

  # update home biting weight
  wHome = self$get_PatchesPointer()$get_bWeightHuman(ix = home) + self$get_bWeight()
  self$get_PatchesPointer()$set_bWeightHuman(bWeightHuman = wHome, ix = home)

  # update visiting patch biting weight
  wAway = self$get_PatchesPointer()$get_bWeightHuman(ix = away) - self$get_bWeight()
  self$get_PatchesPointer()$set_bWeightHuman(bWeightHuman = wAway, ix = away)

  # tell the MacroPatch class where you went
  self$go_Patch(old = away, new = home)

  #Schedule next trip
  tTrip = tEvent + rexp(n=1,rate=sum(self$get_travel()$totFreq))
  ixTrip = sample(x = private$travel$nPlaces, size = 1, prob = private$travel$places_frequency)
  away = self$get_travel()$places_there[ixTrip]

  PAR = list(there = away, ixTravel = ixTrip) # trip parameters
  self$add2Q_takeTrip(tEvent = tTrip, PAR = PAR) # add the trip
}
