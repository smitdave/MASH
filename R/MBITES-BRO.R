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
MbitesBRO_timingExponential <- function(){
  if(self$isActive()){
    duration = switch(private$state,
          B = private$myPopPointer$get_MBITES_PAR("B.t"),
          R = private$myPopPointer$get_MBITES_PAR("R.t"),
          O = private$myPopPointer$get_MBITES_PAR("O.t")
    )
    private$tNext = private$tNow + rexp(n=1,rate=1/duration)
  }
}


#################################################################
# MBITES-BRO: House entering and Resting
#################################################################

#' MBITES-BRO: Return Landing Spot Weights for \code{MicroMosquitoFemale}
#'
#' Method for return landing spot weights based on behavioral state of mosquito.
#'  * This method is bound to \code{MicroMosquitoFemale$getWTS()}.
#'
#' @md
#' @return vector of landing spot weights
MbitesBRO_getWTS <- function(){
  switch(private$state,
    B = private$myPopPointer$get_MBITES_PAR("Bwts"),
    R = private$myPopPointer$get_MBITES_PAR("Rwts"),
    O = private$myPopPointer$get_MBITES_PAR("Owts")
  )
}



#    l) Leave the area
#    r) Reattempt Without Resting;
#    v) Rest on vegetation
#    w) Rest on the Outside wall of a structure
#    i) Rest on the Inside wall of a structure


# newSpot= function(M,F.wts=Fwts,R.wts=Rwts,L.wts=Lwts){
#   if(M$pState=="F" & M$iwofle==6){
#     5
#   }else{
#     wts=switch(M$pState,
#               "F"=F.wts,
#               "R"=R.wts,
#               "L"=L.wts)
#     probs=InAndOut[M$iwofle,]*wts
#     i.rmultinom(list(id=c(1:6),pr=probs))
#   }
# }
#
# surviveRestingHazard=function(M){
#   if(M$pState!="D"){M}else{
#     haz=switch(M$iwofle,
#       "1"=LANDSCAPE$f$haz1[M$f.i],
#       "2"=LANDSCAPE$f$haz2[M$f.i],
#       "3"=LANDSCAPE$f$haz3[M$f.i],
#       "4"=1,
#       "5"=1,
#       "6"=LANDSCAPE$l$haz[M$l.i]
#     )
#     if(!rbinom(1,1,haz)) M$pState="D"
#   }
# M}
#
# enterHouse=function(M){
#  outside=TRUE
#  while(outside==TRUE & M$pState=="F" & M$iwofle!=5){
#    if(rbinom(1,1,LANDSCAPE$f$eh[M$f.i])){
#      outside=FALSE
#      if(EAVE.TUBE==TRUE) M=fEaveTube(M)
#    } else {
#      M$iwofle=newSpot(M, F.wts=Fwts*c(0,1,1,0,1,0))
#      if(IRS==TRUE) M=fIRS(M)
#      M=surviveRestingHazard(M)
#      M=surviveFlightStress(M)
#    }
#  }
# M}
#
# landingSpot=function(M){
# 	if(M$pState!="D"){
#     oldSpot=M$iwofle
#     M$iwofle=newSpot(M)
#     if(M$iwofle<5 & AREA.REPEL ==TRUE) M=fAreaRepel(M)
#     if(oldSpot>1 & M$iwofle==1) M=enterHouse(M)
#     if(M$iwofle<3 & IRS==TRUE) M=fIRS(M)
#     if(M$iwofle<4 & M$pState!="R") M=surviveRestingHazard(M)
#   }
# M}
#

#################################################################
# Initialize Methods and Fields (Setup)
#################################################################

#' MBITES-BRO: Initialize Additional Methods & Fields in \code{MicroMosquitoPop} and \code{MicroMosquito}
#'
#' Initialize M-BITES BRO (Blood Feeding, Resting, Oviposition) lifecycle model.
#'
#' @param overwrite overwrite methods
#' @examples
#' MbitesBRO.Setup()
#' @export
MbitesBRO.Setup <- function(overwrite = TRUE){

    message("initializing M-BITES generic shared methods")

    MicroMosquitoFemale$set(which = "public",name = "timingExponential",
              value = MbitesBRO_timingExponential,
              overwrite = overwrite
    )

}
