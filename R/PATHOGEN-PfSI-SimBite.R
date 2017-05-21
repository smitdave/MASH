#################################################################
#
#   MASH
#   R6-ified
#   SimBite module for PfSI
#   Sean Wu
#   May 19, 2016
#
#################################################################


#' Initialize SimBite PfSI Module
#'
#' Generate a list of parameters PfSI_PAR in \code{\link{HumanPop}} and public methods in \code{\link{Human}}
#'
#' @param Pf_c 0.15; transmission efficiency: infected human to mosquito
#' @param Pf_b 0.55; transmission efficiency: infected mosquito to human
#' @param DurationPf 200; duration of infection (How many days does the infection last?)
#' @return Defines a field (list) PfSI_PAR in \code{\link{HumanPop}} and public methods in \code{\link{Human}}
#' @examples
#' SimBite.PfSI_PARameters()
#' @export
SimBitePfSI.Setup <- function(

){

  ###################################################################
  # Simulated biting
  ###################################################################

  # add2Q_simbitePfSI
  Human$set(which = "public",name = "add2Q_simbitePfSI",
            value = function(tEvent, PAR = NULL){
              PAR$mosquitoPfSI = mosquitoPfSI$new(PfID = -1L, tInf = -1L, spz = 1L, damID = -1L, sireID = -1L)
              self$addEvent2Q(event = self$event_simbitePfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  # event_simbitePfSI: simulated bite event
  Human$set(which = "public",name = "event_simbitePfSI",
            value = function(tEvent, PAR = NULL){
              list(tEvent = tEvent, PAR = PAR, tag = "simbitePfSI")
            }
  )

  # simbitePfSI
  Human$set(which = "public",name = "simbitePfSI",
            value = function(tEvent, PAR){
              self$probeHost_PfSI(tEvent, PAR$mosquitoPfSI)
            }
  )




}






###################################################################
# Add Methods to R6 'Human' and 'HumanPop' Class
###################################################################

#' Initialize PfSI-SimBite Module (Pathogen)
#'
#' Initialize methods in \code{\link{Human}} and \code{\link{HumanPop}} classes for simulated biting.
#' This should be run after calling \code{\link{init.PfSI}} if humans are being simulated as a stand-alone component.
#'
#' @param write me
#' @return write me
#' @examples
#' init.simbitePfSI()
#' @export
init.simbitePfSI <- function(){

  # add2Q_simbitePfSI
  Human$set(which = "public",name = "add2Q_simbitePfSI",
            value = function(tEvent, PAR = NULL){
              Pf0 = makePf0(ixH = private$myID,tBite = tEvent)
              self$addEvent2Q(event = event_simbitePfSI(tEvent = tEvent, PAR = Pf0))
            }
  )

  # queueInfections
  HumanPop$set(which = "public",name = "queueInfections_simbitePfSI",
               value = function(tMax, tBite = 1/20){
                 for(i in 1:self$nHum){
                   t = 0
                   if(self$verbose){
                     print(paste0("queueing bites for human: ",i))
                   }
                   while(t < tMax){
                     t = t + rexp(n = 1,rate = tBite)
                     private$pop[[i]]$add2Q_simbitePfSI(tEvent = t)
                   }
                   private$pop[[i]]$trackHist(tEvent = self$tStart,event = "S")
                 }
               }
  )

  # queueVaxx
  HumanPop$set(which = "public",name = "queueVaxx_simbitePfSI",
               value = function(tVaxx, tTreat, fracPop){
                 for(i in 1:floor(fracPop*self$nHum)){
                   if(self$verbose){
                     print(paste0("queueing vaccination for human: ",i))
                   }
                   private$pop[[i]]$add2Q_pevaccinatePfSI(tEvent = tVaxx)
                   private$pop[[i]]$add2Q_treatPfSI(tEvent = tTreat)
                 }
               }
  )

}
