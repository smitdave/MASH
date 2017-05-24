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

#' MACRO: Initialize Additional PATHOGEN patchPf Methods & Fields in \code{MacroPatch}
#'
#' Write me! \code{\link{Human}}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' MACRO.Humans.Setup()
#' @export
MACRO.patchPf.Setup <- function(){

  print(paste0("initializing MACRO patchPf PATHOGEN component methods & fields for MacroPatch Class"))

  # patchPf: patch level Pf populations
  MacroPatch$set(which = "private",name = "patchPf",
            value = NULL,
            overwrite = TRUE
  )

  MacroPatch$set(which = "public",name = "get_patchPf",
            value = NULL,
            overwrite = TRUE
  )

  MacroPatch$set(which = "public",name = "set_patchPf",
            value = NULL,
            overwrite = TRUE
  )

  # initialize
  MacroPatch$set(which = "public",name = "initialize",
            value = initialize_patchPf,
            overwrite = TRUE
  )

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
#' get_patchPf()
get_patchPf = function(ix = NULL){
  if(is.null(ix)){
    return(private$patchPf)
  } else {
    return(private$patchPf[[ix]])
  }
}

#' Pf: Set \code{MacroPatch} Patch Pf Population
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' get_patchPf()
set_patchPf = function(patchPf, ix = NULL){
  if(!is.null(ix)){
    private$patchPf[[ix]] = patchPf
  } else {
    private$patchPf = patchPf
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
initialize_patchPf = function(MacroPatch_PAR){
  private$N = N
  stop("WRITE ME")
  private$patchPf = vector(mode="list",length=N)
  for(ixP in 1:N){
    private$PfTypes[[ixP]] = patchPf(damID = NULL, sireID = NULL)
  }
}
