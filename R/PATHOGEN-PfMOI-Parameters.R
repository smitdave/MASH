#################################################################
#
#   MASH
#   R6-ified
#   PfMOI Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   June 9, 2017
#
#################################################################

#' Initialize PfMOI Module Parameters
#'
#' This is mostly used to modify PfMOI parameters for different human populations after creation by using \code{set_PfMOI_PAR} method of \code{\link{HumanPop}}
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
#' @param PEProtectPf proportion protected by PE vaccination (probability vaccination successful)
#' @param peBlockPf proportion of infections blocked by PE vaccination
#' @param mnPEPf mean duration of protection from PE vaccination
#' @param vrPEPf standard deviation of protection from PE vaccination
#' @param GSProtectPf proportion protected by GS vaccination (probability vaccination successful)
#' @param gsBlockPf proportion of infections blocked by GS vaccination
#' @param mnGSPf mean duration of protection from GS vaccination
#' @param vrGSPf standard deviation of protection from GS vaccination
#' @param rdtSensPf RDT sensitivity
#' @param rdtSpecPf RDT specificity
#' @param lmSensPf Light Microscopy sensitivity
#' @param lmSpecPf Light Microscopy specificity
#' @return return a list
#' @examples
#' PfMOI.Parameters()
#' @export
PfMOI.Parameters <- function(

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
Pf_ttS = 32,

# Proportion Protected by PE Vaccination
PEProtectPf = .99,

# Proportion of infections Blocked
peBlockPf = 1,
mnPEPf = 270,
vrPEPf = 50,

# Proportion Protected by GS Vaccination
GSProtectPf = 1,

# Proportion of infections Blocked
gsBlockPf = .9,
mnGSPf = 180,
vrGSPf = 20,

#  Diagnostic Parameters
rdtSensPf = .9,
rdtSpecPf = .1,
lmSensPf = 0.9,
lmSpecPf = 0.1

  ){

    list(
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
      PEProtectPf = PEProtectPf,
      peBlockPf = peBlockPf,
      mnPEPf = mnPEPf,
      vrPEPf = vrPEPf,
      GSProtectPf = GSProtectPf,
      gsBlockPf = gsBlockPf,
      mnGSPf = mnGSPf,
      vrGSPf = vrGSPf,
      rdtSensPf = rdtSensPf,
      rdtSpecPf = rdtSpecPf,
      lmSensPf = lmSensPf,
      lmSpecPf = lmSpecPf
      )

}
