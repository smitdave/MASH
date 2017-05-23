#################################################################
#
#   MASH
#   R6-ified
#   Human Movement-related Methods for MACRO
#   Sean Wu
#   May 21, 2017
#
#################################################################


###################################################################
# travelHabit: initialize inter-patch travel
###################################################################

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

  return(
    list(
      randomRate = 1/730,
      nPlaces = n,
      totFreq = sum(1/howOften),
      places_there = there,
      places_frequency = 1/howOften,
      places_length = meanLengthOfTrip
    )
  )
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
event_takeTrip <- function(ixH, t, PAR){
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
  self$loc = away
  home = self$patchID

  # update home biting weight
  wHome = self$get_PatchesPointer()$get_bWeightHuman(ix = home) - self$get_bWeightHuman()
  self$get_PatchesPointer()$set_bWeightHuman(bWeightHuman = wHome, ix = home)

  # update visiting patch biting weight
  wAway = self$get_PatchesPointer()$get_bWeightHuman(ix = away) + self$get_bWeightHuman()
  self$get_PatchesPointer()$set_bWeightHuman(bWeightHuman = wAway, ix = away)

  # queue up the voyage home
  tReturn = tEvent + rexp(n = 1, rate = 1 / self$get_travel()$places_length[PAR$ixTravel])
  self$add2Q_returnHome(tEvent = tReturn, PAR = NULL)
}
