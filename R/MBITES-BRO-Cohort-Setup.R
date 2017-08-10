#################################################################
#
#   MASH
#   R6-ified
#   MBITES-BRO (Blood Feeding, Resting, Oviposition)
#   Simplified Cohort Simulation Setup
#   David Smith, Hector Sanchez, Sean Wu
#   August 9, 2017
#
#################################################################

##############################################################
# Setup Function
##############################################################

#' MBITES-BRO-Cohort: Initialize Additional Methods & Fields in \code{\link{MicroMosquitoPopFemale}} and \code{\link{MicroMosquitoFemale}}
#'
#' WRITE MEEEEE Initialize M-BITES BRO (Blood Feeding, Resting, Oviposition) lifecycle model.
#'
#' @param overwrite overwrite methods
#' @examples
#' MBITES.BRO.Setup()
#' @export
MBITES.BRO.Cohort.Setup <- function(overwrite = TRUE){

  # alert user
  message("initializing MBITES-BRO-Cohort shared methods")

  MBITES.BRO.Setup(overwrite=TRUE,aquaModule="emerge")
  MBITES.Cohort.Setup(overwrite=TRUE)

  ##############################################################
  # MBITES-BRO-Cohort.R
  ##############################################################

  MicroMosquitoFemale$set(which = "public",name = "humanEncounter",
            value = mbitesBRO_cohort_humanEncounter,
            overwrite = overwrite
  )


  # set a null population for the cohort
  MicroMosquitoPopFemale$set(which = "private",name = "cohortPop",
            value = NULL,
            overwrite = overwrite
  )

}
