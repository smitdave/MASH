#################################################################
#
#   MASH
#   R6-ified
#   M-BITES BRO
#   Setup Module
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   July 19, 2017
#
#################################################################

#################################################################
# Initialize Methods and Fields (Setup)
#################################################################

#' MBITES-BRO: Initialize Additional Methods & Fields in \code{\link{MicroMosquitoPopFemale}} and \code{\link{MicroMosquitoFemale}}
#'
#' Initialize M-BITES BRO (Blood Feeding, Resting, Oviposition) lifecycle model.
#'
#' @param overwrite overwrite methods
#' @examples
#' MBITES.BRO.Setup()
#' @export
MBITES.BRO.Setup <- function(overwrite = TRUE){

    message("initializing M-BITES BRO methods")

    MicroMosquitoFemale$set(which = "public",name = "timingExponential",
              value = mbitesBRO_timingExponential,
              overwrite = overwrite
    )

}
