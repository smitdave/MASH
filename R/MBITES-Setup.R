#################################################################
#
#   MASH
#   R6-ified
#   M-BITES Generic Methods
#   Setup Module
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   July 19, 2017
#
#################################################################


##############################################################
# Setup Function
##############################################################

#' Initialize Generic Methods for M-BITES
#'
#' This function initializes generic methods for M-BITES models; please note that the
#' switches for this function modify only the methods that are added to the MicroMosquitoFemale
#' and MicroMosquitoMale classes. Different genotypes still depend on the internal list of parameters
#' to parameterize these functions and functional forms for equations.
#'
#' @param batchSize character switch that should be one of \code{"bms","norm"} for egg batch sizes dependent on bloodmeal size or normally distributed
#' @param eggMatT character switch that should be one of \code{"off","norm"} for egg batch maturation time turned off or normally distributed
#'
#'
#'
#' @return modifies the \code{MicroMosquitoFemale} and \code{MicroMosquitoMale} classes.
#' @export
mbitesGeneric.Setup <- function(overwrite = TRUE){

  # alert user
  message("initializing M-BITES generic shared methods")

  ##############################################################
  # Checks of Life Status
  ##############################################################

  MicroMosquito$set(which = "public",name = "isAlive",
            value = mbitesGeneric_isAlive,
            overwrite = overwrite
  )

  MicroMosquito$set(which = "public",name = "isActive",
            value = mbitesGeneric_isActive,
            overwrite = overwrite
  )

}
