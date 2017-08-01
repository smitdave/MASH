#################################################################
#
#   MASH
#   MICRO Aquatic Ecology: Emerge
#   Setup Function
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   July 31, 2017
#
#################################################################

#################################################################
# Setup
#################################################################

#' Initialize Emerge MODULE
#'
#' This function initializes methods and fields for the 'Emerge' MODULE of Aquatic Ecology COMPONENT.
#' It modifies \code{\link{AquaticSite}} and \code{\link{Landscape}} classes.
#'
#' @param dunno sdf
#' @return stuff
#' @examples
#' Aqua.Emerge.Setup()
#' @export
MICRO.Emerge.Setup <- function(overwrite = TRUE){

  message("initializing 'Emerge' module for Aquatic Ecology")

  #################################################################
  # One Day 'Emerge'
  #################################################################

  # lambda to ImagoQ
  AquaticSite$set(which = "public",name = "oneDay_EmergeSite",
            value = oneDay_MicroEmergeSite,
            overwrite = overwrite
  )

  # lambda to ImagoQ
  Landscape$set(which = "public",name = "oneDay_Emerge",
            value = oneDay_MicroEmerge,
            overwrite = overwrite
  )

  # ImagoQ to MicroMosquitoPopFemale
  AquaticSite$set(which = "public",name = "addCohort_MicroEmergeSite",
            value = addCohort_MicroEmergeSite,
            overwrite = overwrite
  )

  # ImagoQ to MicroMosquitoPopFemale
  Landscape$set(which = "public",name = "addCohort_MicroEmerge",
            value = addCohort_MicroEmerge,
            overwrite = overwrite
  )



  #################################################################
  # Lambda
  #################################################################

  AquaticSite$set(which = "public",name = "get_lambda",
            value = get_MicroLambda,
            overwrite = overwrite
  )

  AquaticSite$set(which = "public",name = "set_lambda",
            value = set_MicroLambda,
            overwrite = overwrite
  )

}
