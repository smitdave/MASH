#################################################################
#
#   MASH
#   R6-ified
#   Generic Methods & Fields for HUMAN for Pf PATHOGEN component
#   Sean Wu
#   May 19, 2016
#
#################################################################

#' Initialize Additional Methods & Fields in \code{Human} and \code{HumanPop} for P.falciparum PATHOGEN component
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' Pf.HUMANS.Setup()
#' @export
Pf.HUMANS.Setup <- function(){

  print(paste0("initializing shared Pf PATHOGEN component methods & fields"))

  # personal biting propensity
  Human$set(which = "private",name = "biteWeight",
            value = NULL,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "get_biteWeight",
            value = get_biteWeightPf,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "set_biteWeight",
            value = set_biteWeightPf,
            overwrite = TRUE
  )

  #initialize
  Human$set(which = "public",name = "initialize",
            value = initialize_Pf,
            overwrite = TRUE
  )

}

#' Pf: Get \code{Human} Personal Biting Propensity
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' get_biteWeight()
get_biteWeightPf <- function(){
  return(private$biteWeight)
}

#' Pf: Set \code{Human} Personal Biting Propensity
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' set_biteWeight()
set_biteWeightPf <- function(biteWeight){
  private$biteWeight = biteWeight
}

#' Pf: Initialize \code{Human}
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' set_biteWeight()
initialize_Pf <- function(myID, hhID, bDay, biteWeight){
  private$myID = myID
  private$hhID = hhID
  private$bDay = bDay
  private$biteWeight = biteWeight
  private$eventQ[[1]] = self$event_maxDeath()
}
