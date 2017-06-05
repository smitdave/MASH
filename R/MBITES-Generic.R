#################################################################
#
#   MASH
#   R6-ified
#   MBITES General Method Definitions
#   David Smith, Hector Sanchez, Sean Wu
#   May 2, 2017
#
#################################################################


#' MBITES: Generic Female Bout for \code{MicroMosquitoFemale}
#'
#' Manage lifecycle methods across modules.
#'
#' @section Generic Bout:
#'  * hello.
#'
#'
#'
#'
#'
#' @md
MbitesGeneric_FemaleBout <- function(){

  # update time and state
  private$tNow = private$tNext # update time
  private$state = private$stateNew # update current state
  self$timingExponential() # update tNext

  # movement
  self$moveMe()

  # # bout
  # M = boutFun(M,P,...)
  #
  # # energetics
  # M = energetics(M,P) # MBITES-Energetics.R
  #
  # # landing spot
  # M = landingSpot(M,P) # MBITES-Bouts.R
  #
  # # # Intervetions routines
  # # M=fIRS(M)
  # # M=fAerialSpray(M)
  #
  # # survival
  # M = surviveResting(M,P) # MBITES-Survival.R
  # M = surviveFlight(M,P) # MBITES-Survival.R
  #
  # # check queueing
  # M = queueEstivation(M,P) # MBITES-Estivate.R
  #
  # # log history
  # if(P$HISTORY){M = historyTrack(M)}


}

#' MBITES: Generic Alive Check for \code{MicroMosquito}
#'
#' Check if this mosquito is alive and return a logical value.
#'  * This method is bound to \code{MicroMosquito$isAlive()}.
#' @md
MbitesGeneric_isAlive <- function(){
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
MbitesGeneric_isActive <- function(){
  if(private$state == "E"){
    return(FALSE)
  } else {
    return(self$isAlive())
  }
}


#' Initialize Generic Methods for M-BITES
#'
#' This function initializes generic methods for M-BITES models; please note that the
#' switches for this function modify only the methods that are added to the MicroMosquitoFemale
#' and MicroMosquitoMale classes. Different genotypes still depend on the internal list of parameters
#' to parameterize these functions and functional forms for equations.
#'
#' @param batchSize character switch that should be one of \code{"bms","norm"} for egg batch sizes dependent on bloodmeal size or normally distributed
#' @param eggMatT character switch that should be one of \code{"off","norm"} for egg batch maturation time turned off or normally distributed
#'
#'
#'
#' @return modifies the \code{MicroMosquitoFemale} and \code{MicroMosquitoMale} classes.
#' @export
MbitesGeneric.Setup <- function(overwrite = TRUE){

  # alert user
  message("initializing M-BITES generic shared methods")

  ##############################################################
  # Checks of Life Status
  ##############################################################

  MicroMosquito$set(which = "public",name = "isAlive",
            value = MbitesGeneric_isAlive,
            overwrite = overwrite
  )

  MicroMosquito$set(which = "public",name = "isActive",
            value = MbitesGeneric_isActive,
            overwrite = overwrite
  )

  ##############################################################
  # History & Bionomics
  ##############################################################

  # track history
  Mosquito$set(which = "public",name = "historyTrack",
            value = function(){
              private$history$stateH     = c(private$history$stateH,private$state)     # state trajectory
              private$history$timeH      = c(private$history$timeH,private$tNow)         # transition times
              private$history$ixH        = c(private$history$ixH,private$ix)              # sites visited
              private$history$pSetH      = c(private$history$pSetH,private$inPointSet)    # point sets visited
              if(!self$isAlive()){ # return final state transition & calculate bionomics upon death
                private$history$stateH     = c(private$history$stateH,private$stateNew)     # state trajectory
                private$history$timeH      = c(private$history$timeH,private$tNext)         # transition times
                self$bionomics()
              }
            }
  )

  # calculate bionomics upon death
  Mosquito$set(which = "public",name = "bionomics",
            value = function(){
              with(private$history,{
                if(tail(stateH,n=1) != "D"){
                  stop("mosquito not dead yet")
                }
                if(!is.null(batchH)){
                  private$bionomics$mBatch = mean(batchH) # mean batch size
                  private$bionomics$tBatch = sum(batchH) # total batch size
                }
                private$bionomics$feedAllH =  feedAllH # total number of bloodmeals
                private$bionomics$feedHumanH = feedHumanH # number of human bloodmeals
                private$bionomics$bmInt = diff(feedAllT) # all bloodmeal intervals
                private$bionomics$bmIntH = diff(feedHumanT) # human bloodmeal intervals
                private$bionomics$lifespan = tail(timeH,n=1) - head(timeH,n=1) # lifespan
              })
            }
  )

  # return history
  Mosquito$set(which = "public",name = "historyReturn",
            value = function(){
              return(private$history)
            }
  )

  # return bionomics
  Mosquito$set(which = "public",name = "bionomicsReturn",
            value = function(){
              return(private$bionomics)
            }
  )

  ##############################################################
  # M-BITES
  ##############################################################

  ##############################################################
  # House Entry & Resting Behavior
  ##############################################################

  Mosquito$set(which = "public",name = "getWTS",
            value = function(){
              switch(private$state,
                     F = private$PAR$Fwts,
                     B = private$PAR$Fwts,
                     R = private$PAR$Rwts,
                     L = private$PAR$Lwts,
                     O = private$PAR$Owts,
                     M = private$PAR$Mwts,
                     S = private$PAR$Swts
              )
            }
  )

  Mosquito$set(which = "public",name = "lspotIx",
            value = function(){
              switch(private$lspot,
                i = 1L,
                w = 2L,
                v = 3L,
                r = 4L,
                l = 5L
              )
            }
  )

  Mosquito$set(which = "public",name = "newSpot",
            value = function(){
              if(private$inPointSet == "f"){
                probs = private$PAR$InAndOut[self$lspotIx(),] * self$getWTS(PAR)
                return(sample(x = c("i","w","v","r","l"),size = 1,prob = probs))
              } else {
                return("v")
              }
            }
  )

  Mosquito$set(which = "public",name = "enterHouse",
            value = function(enterHouseP){
              if(runif(1) < enterHouseP){ #mosquito found a gap
                # call VECTOR.CONTROL here
              } else { #mosquito did not find a gap
                private$lspot = newSpot(private, self)
                self$surviveFlight(private, self)
                if(private$lspot == "i"){
                  Recall(enterHouseP, private, self)
                }
              }
            }
  )

  Mosquito$set(which = "public",name = "landingSpot",
            value = function(enterHouseP){
              if(self$isActive()){
                oldSpot = private$lspot
                private$lspot = self$newSpot(private, self) # choose new lspot
                if(oldSpot != "i" & private$lspot == "i"){
                  self$enterHouse(enterHouseP, private, self)
                }
                # if(M$lspot == "l" & M$state == "B"){
                #   M$stateNew = "F" # repeat search bout (boutF)
                # }
                # if(M$lspot == "l" & M$state == "O"){
                #   M$stateNew = "L" # repeat search bout (boutL)
                # }
              }
            }
  )

  ##############################################################
  # Energetics (Blood meals, sugar feeding, and egg production)
  ##############################################################

  #INIT FROM MBITES-Energetics.R



}
