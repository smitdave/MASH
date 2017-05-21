#################################################################
#
#   MASH
#   R6-ified
#   SimBite module for PfSI
#   Sean Wu
#   May 19, 2016
#
#################################################################


#' Initialize SimBite PfSI Module Parameters (Pathogen)
#'
#' Generate a list of parameters PfSI_PAR in \code{\link{HumanPop}} and public methods in \code{\link{Human}}
#'
#' @param Pf_c 0.15; transmission efficiency: infected human to mosquito
#' @param Pf_b 0.55; transmission efficiency: infected mosquito to human
#' @param DurationPf 200; duration of infection (How many days does the infection last?)
#' @param LatentPf 10; latency (How many days after the infectious bite does the infection start?)
#' @param FeverPf 0.3; probability of fever
#' @param mnFeverPf 10; mean of Gaussian-distributed timing of fever incident (relative to start of the infection)
#' @param vrFeverPf 0.1; standard deviation of Gaussian-distributed timing of fever incident (relative to start of the infection)
#' @param TreatPf 0.5; probability of treatment
#' @param mnTreatPf 1; timing of treatment (relative to start of the fever)
#' @param mnChemoprophylaxisPf 32; Prophylaxis, time to susceptibility (duration of protection due to chemoprophylaxis)
#' @param PEProtectPf 0.99; proportion protected by PE vaccination (probability vaccination successful)
#' @param peBlockPf 1; proportion of infections blocked by PE vaccination
#' @param mnPEPf 270; mean duration of protection from PE vaccination
#' @param vrPEPf 50; standard deviation of protection from PE vaccination
#' @param GSProtectPf 1; proportion protected by GS vaccination (probability vaccination successful)
#' @param gsBlockPf 0.9; proportion of infections blocked by GS vaccination
#' @param mnGSPf 180; mean duration of protection from GS vaccination
#' @param vrGSPf 20; standard deviation of protection from GS vaccination
#' @param rdtSensPf 0.9; RDT sensitivity
#' @param rdtSpecPf 0.1; RDT specificity
#' @param lmSensPf 0.9; Light Microscopy sensitivity
#' @param lmSpecPf 0.1; Light Microscopy specificity
#' @return Defines a field (list) PfSI_PAR in \code{\link{HumanPop}} and public methods in \code{\link{Human}}
#' @examples
#' SimBite.PfSI_PARameters()
#' @export
SimBitePfSI.Setup <- function(
  N = NULL
){

  ###################################################################
  # Simulated biting
  ###################################################################

  # event_simbitePfSI: simulated bite event
  Human$set(which = "public",name = "event_simbitePfSI",
            value = function(tEvent, PAR = NULL){
              list(tEvent = tEvent, PAR = PAR, tag = "simbitePfSI")
            }
  )

  # simbitePfSI
  Human$set(which = "public",name = "simbitePfSI",
            value = function(tEvent, PAR){
              # self$probeHost_PfSI()
            }
  )

  # add2Q_simbitePfSI
  Human$set(which = "public",name = "add2Q_simbitePfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = self$event_infectHumanPfSI(tEvent = tEvent, PAR = PAR))
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
