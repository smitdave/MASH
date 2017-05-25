#################################################################
#
#   MASH
#   R6-ified
#   Methods & Fields for Pf PATHOGEN component for MACRO to modify MacroPatch
#   David Smith, Hector Sanchez, Sean Wu
#   May 23, 2016
#
#################################################################


#################################################################
# Initalize Methods & Fields in 'MacroPatch'
#################################################################

#' MACRO: Initialize Additional PATHOGEN PatchPf Methods & Fields in \code{MacroPatch}
#'
#' Write me! \code{\link{Human}}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' MACRO.Humans.Setup()
#' @export
MACRO.PatchPf.Setup <- function(){

  print(paste0("initializing MACRO PatchPf PATHOGEN component methods & fields for MacroPatch Class"))

  # PatchPf: patch level Pf populations
  MacroPatch$set(which = "private",name = "PatchPf",
            value = NULL,
            overwrite = TRUE
  )

  MacroPatch$set(which = "public",name = "get_PatchPf",
            value = NULL,
            overwrite = TRUE
  )

  MacroPatch$set(which = "public",name = "set_PatchPf",
            value = NULL,
            overwrite = TRUE
  )

  # # initialize
  # MacroPatch$set(which = "public",name = "initialize",
  #           value = initialize_PatchPf,
  #           overwrite = TRUE
  # )

}


#################################################################
# Define Methods in 'MacroPatch'
#################################################################

#' Pf: Get \code{MacroPatch} Patch Pf Population
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' get_PatchPf()
get_PatchPf = function(ix = NULL){
  if(is.null(ix)){
    return(private$PatchPf)
  } else {
    return(private$PatchPf[[ix]])
  }
}

#' Pf: Set \code{MacroPatch} Patch Pf Population
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' get_PatchPf()
set_PatchPf = function(PatchPf, ix = NULL){
  if(!is.null(ix)){
    private$PatchPf[[ix]] = PatchPf
  } else {
    private$PatchPf = PatchPf
  }
}


#' Pf: Initialize \code{Human}
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' set_biteWeight()
initialize_PatchPf = function(MacroPatch_PAR){
  private$N = N
  stop("WRITE ME")
  private$PatchPf = vector(mode="list",length=N)
  for(ixP in 1:N){
    private$PfTypes[[ixP]] = PatchPf(damID = NULL, sireID = NULL)
  }
}
