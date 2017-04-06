#################################################################
#
#   MASH/MBITES
#   MBITES Estivation Routines
#   R version
#   Sean Wu
#   January 25, 2017
#
#################################################################

##################################################################
# ESTIVATION
#
#   Note that survival through estivation
#   is determined in the queue estivation function
#   and if a mosquito would not have survived,
#   it is immediately killed.
#
#   The "estivation bout" simply changes
#   bState to "F"
#
##################################################################

queueEstivation <- function(M,P){
  with(P,{

    if(ESTIVATION && isAlive(M)){
      if(rbinom(n=1,size=1,prob=prEstivate(M$tNow,Emax,Eb))){
        if(rbinom(n=1,size=1,prob=E.p)){
          M$stateNew = "E"
          M$tNext = wakeUpTime(eEndm,eEndV)
        } else {
          M$stateNew = "D"
        }
      }
    }

    return(M)
  })
}

prEstivate <- function(tNow, Emax, Eb){
  pmax(0, cos(2*pi*(tNow-Emax)/365)-Eb)
}

wakeUpTime <- function(eEndm, eEndV){
  rnorm(n = 1, mean = eEndm, sd = eEndV)
}
