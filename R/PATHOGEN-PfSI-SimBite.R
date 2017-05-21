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

    print(paste0("initializing PfSI SimBite module"))

  ###################################################################
  # Simulated biting on 'Human' Class
  ###################################################################

  # add2Q_simbitePfSI
  Human$set(which = "public",name = "add2Q_simbitePfSI",
            value = function(tEvent, PAR = NULL){
              PAR = list()
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

  ###################################################################
  # Tools to initialize simulated biting for 'HumanPop' class
  ###################################################################

  # queueBites
  HumanPop$set(which = "public",name = "queueBites_simBitePfSI",
               value = function(tMax, bitingRate = 1/20){
                 for(ixH in 1:self$nHum){
                   print(paste0("queueing simulated bites for human: ",ixH))
                   tBite = 0
                   while(tBite < tMax){
                     tBite = tBite + rexp(n = 1,rate = bitingRate)
                     private$pop[[ixH]]$add2Q_simbitePfSI(tEvent = tBite)
                   }
                  #  private$pop[[i]]$trackHist(tEvent = self$tStart,event = "S")
                 }
               }
  )

  # queueVaccination
  HumanPop$set(which = "public",name = "queueVaccination_simBitePfSI",
               value = function(tVaccine, tTreat, fracPop){
                 for(ixH in 1:floor(fracPop*self$nHum)){
                   print(paste0("queueing vaccination for human: ",ixH))
                   private$pop[[ixH]]$add2Q_pevaccinatePfSI(tEvent = tVaxx)
                   private$pop[[ixH]]$add2Q_treatPfSI(tEvent = tTreat)
                 }
               }
  )

}
