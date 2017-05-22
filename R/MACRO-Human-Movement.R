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
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
travelHabit <- function(){

  self$get_PatchesPointer()$get_N()

}

travelHabit = function(here, N, n, t=0){
  #NOTE: Modularize
  #browser()
  there = sample(c(1:N)[-here],n)
  howOften = 60+round(rlnorm(n,7,2))
  meanLengthOfTrip = round(rlnorm(n,2,1))

  # set up the next trip
  nextTripT = t+rexp(1, sum(1/howOften))
  ixTrip = sample(c(1:n), 1, prob=1/howOften)
  away = there[ixTrip]
  add2Q_takeTrip(ixH, nextTripT,list(there=away,ixTrip=ixTrip))

  list(
    randomRate = 1/730,
    nPlaces = n,
    totFreq = sum(1/howOften),
    places = list(there=there, frequency=1/howOften, length=meanLengthOfTrip)
  )
}




Human$set(which = "public",name = "infectHumanPfSI",
          value = function(tEvent, PAR){
            if(!private$Pathogens$Pf$get_infected() & !private$Pathogens$Pf$get_chemoprophylaxis()){
              self$trackHist(tEvent = tEvent, event = "I") # track history
              private$Pathogens$Pf$set_infected(TRUE)

              # newID = self$get_SelfPointer()$increment_PfID()
              # private$Pathogens$Pf$push_PfID(newID)

              private$Pathogens$Pf$push_PfID(self$get_SelfPointer()$increment_PfID())

              private$Pathogens$Pf$push_damID(PAR$damID)
              private$Pathogens$Pf$push_sireID(PAR$sireID)
              if(runif(1) < private$PfSI_PAR$FeverPf){
                  self$add2Q_feverPfSI(tEvent = tEvent)
              }
              self$add2Q_endPfSI(tEvent = tEvent)
            }
          }
)


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
#' @return does stuff
#' @examples
#' some_function()
takeTrip <- function(tEvent, PAR){

  # take the trip
  away = PAR$there
  self$loc = away
  home = self$patchID

  # update home biting weight
  wHome = self$get_PatchesPointer()$get_wHuman(ixP = home) - self$get_biteWeight()
  self$get_PatchesPointer()$set_wHuman(ixP = home,wHuman = wHome)

  # update visiting patch biting weight
  wAway = self$get_PatchesPointer()$get_wHuman(ixP = away) + self$get_biteWeight()
  self$get_PatchesPointer()$set_wHuman(ixP = away,wHuman = wAway)

  # queue up the voyage home
  tReturn = tEvent + rexp(n = 1, rate = 1 / self$get_travel()$places_length[PAR$ixP])
  self$add2Q_returnHome(tEvent = tReturn, PAR = NULL)
}
