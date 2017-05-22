#################################################################
#
#   MASH
#   R6-ified
#   Additional Human Methods & Fields for MACRO
#   Sean Wu
#   May 21, 2017
#
#################################################################


#################################################################
# Initalize Methods & Fields in 'Human'
#################################################################

#' MACRO: Initialize Additional Methods & Fields in \code{Human}
#'
#' Write me! \code{\link{Human}}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' MACRO.Humans.Setup()
#' @export
MACRO.Humans.Setup <- function(){

  print(paste0("initializing MACRO component methods & fields"))

  #############################################
  # location: where the human is now
  #############################################

  Human$set(which = "private",name = "location",
            value = NULL,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "get_location",
            value = get_location,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "set_location",
            value = set_location,
            overwrite = TRUE
  )

  #############################################
  # patchID: the human's permanant home
  #############################################

  Human$set(which = "private",name = "patchID",
            value = NULL,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "get_patchID",
            value = get_patchID,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "set_patchID",
            value = set_patchID,
            overwrite = TRUE
  )

  #############################################
  # travel: the human's travel habits
  #############################################

  Human$set(which = "private",name = "travel",
            value = NULL,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "get_travel",
            value = get_travel,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "set_travel",
            value = set_travel,
            overwrite = TRUE
  )

  #############################################
  # PatchesPointer: point to the Patches (a network of patches) in this metapopulation TILE (MACRO)
  #############################################

  Human$set(which = "private",name = "PatchesPointer",
            value = NULL,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "get_PatchesPointer",
            value = get_PatchesPointer,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "set_PatchesPointer",
            value = set_PatchesPointer,
            overwrite = TRUE
  )

  #############################################
  # MACRO-Human-Movement.R
  #############################################

  Human$set(which = "public",name = "add2Q_takeTrip",
            value = add2Q_takeTrip,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "",
            value = event_takeTrip,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "",
            value = takeTrip,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "",
            value = ,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "",
            value = ,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "",
            value = ,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "",
            value = ,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "",
            value = ,
            overwrite = TRUE
  )

}


#################################################################
# Define Methods in 'Human'
#################################################################

# location

#' MACRO: Get \code{Human} location
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
get_location <- function(){
  return(private$location)
}

#' MACRO: Set \code{Human} location
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
set_location <- function(location){
  private$location = location
}

# patchID

#' Get \code{Human} patchID
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
get_patchID <- function(){
  return(private$patchID)
}

#' Set \code{Human} patchID
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
set_patchID <- function(patchID){
  private$patchID = patchID
}

# travel

#' Get \code{Human} travel
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
get_travel <- function(){
  return(private$travel)
}

#' Set \code{Human} travel
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
set_travel <- function(travel){
  private$travel = travel
}

# PatchesPointer

#' Get \code{Human} patchID
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
get_PatchesPointer <- function(){
  return(private$PatchesPointer)
}

#' Set \code{Human} patchID
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
set_PatchesPointer <- function(PatchesPointer){
  private$PatchesPointer = PatchesPointer
}
