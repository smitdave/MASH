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

  print(paste0("initializing MACRO component methods & fields for Human Class"))

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
  # MosquitoPointer: point to the Mosquito population in this metapopulation TILE (MACRO)
  #############################################

  Human$set(which = "private",name = "MosquitoPointer",
            value = NULL,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "get_MosquitoPointer",
            value = get_MosquitoPointer,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "set_MosquitoPointer",
            value = set_MosquitoPointer,
            overwrite = TRUE
  )

  #############################################
  # MACRO-Human-Movement.R
  #############################################

  # travel: the human's travel habits

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

  Human$set(which = "public",name = "travelHabit",
            value = travelHabit,
            overwrite = TRUE
  )

  # Events

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

  #############################################
  # MACRO-Human-Biting.R
  #############################################

  # expectedBites

  Human$set(which = "private",name = "myEIR",
            value = NULL,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "get_myEIR",
            value = get_myEIR,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "set_myEIR",
            value = set_myEIR,
            overwrite = TRUE
  )

  Human$set(which = "public",name = "expectedBites",
            value = expectedBites,
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

# PatchesPointer

#' Get \code{Human} \code{MacroPatch} Pointer
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

#' Set \code{Human} \code{MacroPatch} Pointer
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

# MosquitoPointer

#' Get \code{Human} \code{MacroMosquitoPop} Pointer
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
get_MosquitoPointer <- function(){
  return(private$MosquitoPointer)
}

#' Set \code{Human} \code{MacroMosquitoPop} Pointer
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
set_MosquitoPointer <- function(MosquitoPointer){
  private$MosquitoPointer = MosquitoPointer
}
