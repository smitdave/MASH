#################################################################
#
#   MASH
#   R6-ified
#   Aquatic Ecology for Sites:
#   Specific Functions for "Emerge" Module
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   May 10, 2017
#
#################################################################


#################################################################
# oneDay: control daily Aquatic Ecology dynamics for site
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

  # oneDay dynamics for landscape
  Landscape$set(which = "public",name = "oneDay",
            value = oneDay_MicroEmerge,
            overwrite = overwrite
  )

}


#' MICRO \code{Landscape} Seasonal Emergence
#'
#' Queue the ImagoQ for MICRO Emerge aquatic ecology module.
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
oneDay_MicroEmerge <- function(){
  tNow = self$get_TilePointer()$get_tNow()
}

# called from AquaticSite
oneDay_MicroEmergeSite <- function(tNow){
  lambdaExact = private$lambda[floor(tNow)%%365+1]
  lambdaEmerge = rpois(n = 1,lambda = lambdaExact)

}
