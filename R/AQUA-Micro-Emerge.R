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

  #################################################################
  # One Day 'Emerge'
  #################################################################

  AquaticSite$set(which = "public",name = "oneDay_EmergeSite",
            value = oneDay_MicroEmergeSite,
            overwrite = overwrite
  )

  Landscape$set(which = "public",name = "oneDay_Emerge",
            value = oneDay_MicroEmerge,
            overwrite = overwrite
  )

  Landscape$set(which = "public",name = "emergingAdults_Emerge",
            value = emergingAdults_MicroEmerge,
            overwrite = overwrite
  )



  #################################################################
  # Lambda
  #################################################################

  AquaticSite$set(which = "public",name = "get_lambda",
            value = get_MicroLambda,
            overwrite = TRUE
  )

  AquaticSite$set(which = "public",name = "set_lambda",
            value = set_MicroLambda,
            overwrite = TRUE
  )

}


#################################################################
# Lambda
#################################################################

#' MICRO \code{\link{AquaticSite}} Method: Get Lambda
#'
#' Get either a single day lambda or entire vector
#' This method is bound to \code{AquaticSite$get_lambda()}.
#'
#' @param ixQ if \code{NULL} return the entire vector of lambda, else, return the value corresponding to day \code{ix}
get_MicroLambda <- function(ix = NULL){
  if(is.null(ix)){
    return(private$lambda)
  } else {
    return(private$lambda[ix])
  }
}


#' MICRO \code{\link{AquaticSite}} Method: Set Lambda
#'
#' Set either a single day lambda or entire vector
#' This method is bound to \code{AquaticSite$set_lambda()}.
#'
#' @param lambda the object to insert; if \code{ix = NULL} then it should be vector of lambda values, see \code{\link{aquaEmerge_makeLambda}} for details, else it should be a numeric value.
#' @param ixQ if \code{NULL} set the entire ImagoQ, else, set the slot \code{ixQ}
set_MicroLambda <- function(lambda, ix = NULL){
  if(is.null(ix)){
    private$lambda = lambda
  } else {
    private$lambda[ix] = lambda
  }
}


#################################################################
# One Day 'Emerge'
#################################################################

#' MICRO \code{\link{AquaticSite}} Method: Emerge One Day Dynamics
#'
#' Calculate emerging adults for a single aquatic habitat and add them to that site's ImagoQ.
#' This method is bound to \code{AquaticSite$oneDay_EmergeSite()}.
#'
#' @param tNow integer time to calculate emergence
oneDay_MicroEmergeSite <- function(tNow){

  lambdaExact = private$lambda[floor(tNow)%%365+1]
  lambdaEmerge = rpois(n = 1, lambda = lambdaExact)
  self$add_ImagoQ(newImago = newImago(N = lambdaEmerge, tEmerge = tNow))

}


#' MICRO \code{\link{Landscape}} Method: Emerge One Day Dynamics
#'
#' Calculate emerging adults for a single aquatic habitat and add them to that site's ImagoQ for all sites.
#' This method is bound to \code{Landscape$oneDay_Emerge()}.
#'
oneDay_MicroEmerge <- function(){
  tNow = self$get_TilePointer()$get_tNow()
  for(ixA in 1:self$AquaSitesN){
    private$AquaSites[[ixA]]$oneDay_EmergeSite()
  }
}


#' MICRO \code{\link{Landscape}} Method: Get Emerging Adults from ImagoQ and Zero out ImagoQ
#'
#' This method is bound to \code{Landscape$emergingAdults_Emerge()}
#'
#' @return does stuff
#' @examples
#' some_function()
emergingAdults_MicroEmerge <- function(){
  # use tNow in the TILE and see who is ready to be taken from ImagoQ into the MosyPop.
}


#' MICRO \code{\link{Landscape}} Method: Get Emerging Adults from ImagoQ and Zero out ImagoQ
#'
#' This method is bound to \code{Landscape$emergingAdults_Emerge()}
#'
#' @return does stuff
#' @examples
#' some_function()
addCohort_MicroEmerge <- function(){
  stop("write me please!!!")
}
