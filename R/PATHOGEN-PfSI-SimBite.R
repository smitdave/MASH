#################################################################
#
#   MASH
#   R6-ified
#   SimBite module for PfSI
#   David Smith, Hector Sanchez, Sean Wu
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
#' SimBitePfSI.Setup()
#' @export
SimBitePfSI.Setup <- function(

){

    message("initializing PfSI SimBite module")

  ###################################################################
  # Simulated biting on 'Human' Class
  ###################################################################

  # add2Q_SimBitePfSI
  Human$set(which = "public",name = "add2Q_SimBitePfSI",
            value = function(tEvent, PAR = NULL){
              PAR = list()
              PAR$mosquitoPfSI = mosquitoPfSI$new(PfID = -1L, tInf = -1L, spz = 1L, damID = -1L, sireID = -1L)
              private$EventQueue$addEvent2Q(event = self$event_SimBitePfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  # event_SimBitePfSI: simulated bite event
  Human$set(which = "public",name = "event_SimBitePfSI",
            value = function(tEvent, PAR = NULL){
              list(tEvent = tEvent, PAR = PAR, tag = "SimBitePfSI")
            }
  )

  # SimBitePfSI
  Human$set(which = "public",name = "SimBitePfSI",
            value = function(tEvent, PAR){
              self$probeHost_PfSI(tEvent, PAR$mosquitoPfSI)
            }
  )

  ###################################################################
  # Tools to initialize simulated biting for 'HumanPop' class
  ###################################################################

  # queueBites
  HumanPop$set(which = "public",name = "queueBites_SimBitePfSI",
               value = function(tMax, bitingRate = 1/20){
                 for(ixH in 1:self$nHumans){
                   print(paste0("queueing simulated bites for human: ",ixH))
                   tBite = 0
                   while(tBite < tMax){
                     tBite = tBite + rexp(n = 1,rate = bitingRate)
                     private$pop[[ixH]]$add2Q_SimBitePfSI(tEvent = tBite)
                   }
                 }
               }
  )

  # queueVaccination
  HumanPop$set(which = "public",name = "queueVaccination_SimBitePfSI",
               value = function(tVaccine, tTreat, fracPop){
                 for(ixH in 1:floor(fracPop*self$nHumans)){
                   print(paste0("queueing vaccination for human: ",ixH))
                   private$pop[[ixH]]$add2Q_pevaccinatePfSI(tEvent = tVaccine)
                   private$pop[[ixH]]$add2Q_treatPfSI(tEvent = tTreat)
                 }
               }
  )

  # queueBitesNegBinom_SimBitePfSI
  HumanPop$set(which = "public",name = "queueBitesNegBinom_SimBitePfSI",
               value = queueBitesNegBinom_SimBitePfSI,
               overwrite = TRUE
  )

}
