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
