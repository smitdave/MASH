#################################################################
#
#   MASH
#   R6-ified
#   PfMOI Setup
#   David Smith, Hector Sanchez, Sean Wu
#   June 9, 2017
#
#################################################################

#' Initialize PfMOI MODULE
#'
#' Generate a list of parameters PfMOI_PAR in \code{\link{Human}} and public methods in \code{\link{Human}} for PfSI infection model; also defines public methods
#' in \code{\link{MicroMosquitoFemale}} for PfMOI infection model.
#'
#' @param MosyMaxI maximum number of clonal variants passed in single mosquito to human transmission event (set to \code{-1L} for unlimited)
#' @param Pf_c infected human to mosquito transmission efficiency
#' @param Pf_b infected mosquito to human transmission efficiency
#' @param Pf_r rate of clearance (assuming clonal variants are cleared independently)
#' @param Pf_latent latency (How many days after the infectious bite does the infection start?)
#' @param Pf_fever probability of fever
#' @param Pf_ttF mean of timing of fever incident relative to start of PfMOI infection (log-normally distributed, mean on natural scale)
#' @param Pf_ttFvar standard deviation of timing of fever incident relative to start of PfMOI infection (log-normally distributed, standard deviation on natural scale)
#' @param TreatPf probability of treatment after fever incident
#' @param Pf_ttT average waiting time from fever to treatment (exponentially distributed)
#' @param Pf_ttS constant period of proteection from chemoprophylaxis
#' @param rdtSensPf RDT sensitivity
#' @param rdtSpecPf RDT specificity
#' @param lmSensPf Light Microscopy sensitivity
#' @param lmSpecPf Light Microscopy specificity
#' @return Defines a field (list) PfSI_PAR in \code{\link{HumanPop}} and public methods in \code{\link{Human}}
#' @export
PfMOI.Setup <- function(

  overwrite = TRUE,

  ########################################
  #  Transmission & Infection
  ########################################
  MosyMaxI =  1L,
  Pf_c = 0.15,
  Pf_b = 0.55,

  ########################################
  #  Timing
  ########################################

  Pf_r = 1/200, # Duration of infection
  Pf_latent = 0, # Latency

  # Fever
  Pf_fever = 0.3,
  Pf_ttF = 10,
  Pf_ttFvar = .1,

  # Treatment
  TreatPf = 0.5,
  Pf_ttT = 3,

  # Prophylaxis, time to susceptibility
  Pf_ttS = 3,

  #  Diagnostic Parameters
  rdtSensPf = .9,
  rdtSpecPf = .1,
  lmSensPf = 0.9,
  lmSpecPf = 0.1

){

  message(paste0("initializing PfMOI PATHOGEN module"))

  ###################################################################
  # Add PfMOI Parameters to 'Human' Class
  ###################################################################

  # PfSI_PAR: list of PfSI parameters added to private field of 'Human' class
  Human$set(which = "private",name = "PfMOI_PAR",
            value = list(
              MosyMaxI =  MosyMaxI,
              Pf_c = Pf_c,
              Pf_b = Pf_b,
              Pf_r = Pf_r,
              Pf_latent = Pf_latent,
              Pf_fever = Pf_fever,
              Pf_ttF = Pf_ttF,
              Pf_ttFvar = Pf_ttFvar,
              TreatPf = TreatPf,
              Pf_ttT = Pf_ttT,
              Pf_ttS = Pf_ttS,
              rdtSensPf = rdtSensPf,
              rdtSpecPf = rdtSpecPf,
              lmSensPf = lmSensPf,
              lmSpecPf = lmSpecPf
            ),
            overwrite = overwrite
  )

  # # getter for PfSI_PAR: ix should be a character eg 'Pf_b'
  # Human$set(which = "public",name = "get_PfSI_PAR",
  #           value = Human_get_PfSI_PAR,
  #           overwrite = overwrite
  # )

  # # setter for PfSI_PAR
  # Human$set(which = "public",name = "set_PfSI_PAR",
  #           value = Human_set_PfSI_PAR,
  #           overwrite = overwrite
  # )

  #################################################################
  # PfMOI Event Timing
  #################################################################

  Human$set(which = "public",name = "ttClearPf",
            value = PfMOI_ttClearPf,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "ttInfectionPf",
            value = PfMOI_ttInfectionPf,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "ttFeverPf",
            value = PfMOI_ttFeverPf,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "ttTreatPf",
            value = PfMOI_ttTreatPf,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "ttSusceptiblePf",
            value = PfMOI_ttSusceptiblePf,
            overwrite = overwrite
  )



}
