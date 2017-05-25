#################################################################
#
#   MASH
#   R6-ified
#   'Emerge' model of aquatic ecology for MacroPatch
#   David Smith, Hector Sanchez, Sean Wu
#   May 24, 2016
#
#################################################################

#' Initialize Additional Methods & Fields in \code{MacroPatch} for Emerge Module of Aquatic Ecology
#'
#' Write me! See for parameters that are initialized after this; ie: anything that \code{\link{MACRO.Patch.Parameters}} calls for 'Emerge' should be defined here.
#'
#' @param a parameter
#' @return do stuff
#' @examples
#' MACRO.Patch.Emerge.Setup()
#' @export
MACRO.Patch.Emerge.Setup <- function(){

  #################################################################
  # Methods
  #################################################################

    # addCohort_MacroEmerge takes the generic name addCohort because it interfaces with the MacroMosquitoPop class
    MacroPatch$set(which = "private",name = "addCohort",
              value = addCohort_MacroEmerge,
              overwrite = TRUE
    )

    # add adults from lambda to PatchesImagoQ
    MacroPatch$set(which = "private",name = "emergingAdults_MacroEmerge",
              value = emergingAdults_MacroEmerge,
              overwrite = TRUE
    )

    # helper function to get from ImagoQ to addCohort
    MacroPatch$set(which = "private",name = "oneDay_MacroEmerge",
              value = oneDay_MacroEmerge,
              overwrite = TRUE
    )


  #################################################################
  # Getters & Setters
  #################################################################

  # season: lambda emergence for each patch
  MacroPatch$set(which = "private",name = "season",
            value = NULL,
            overwrite = TRUE
  )

  MacroPatch$set(which = "public",name = "get_season",
            value = get_season,
            overwrite = TRUE
  )

  MacroPatch$set(which = "public",name = "set_season",
            value = set_season,
            overwrite = TRUE
  )

  # ImagoQ: queue for emerging adults
  MacroPatch$set(which = "private",name = "PatchesImagoQ",
            value = NULL,
            overwrite = TRUE
  )

  MacroPatch$set(which = "public",name = "get_PatchesImagoQ",
            value = get_PatchesImagoQ,
            overwrite = TRUE
  )

  MacroPatch$set(which = "public",name = "set_PatchesImagoQ",
            value = set_PatchesImagoQ,
            overwrite = TRUE
  )

  # eventually may need push_ImagoQ and push_EggQ when doing mosquito genetics in MACRO; ie it will push to ImagoQ[[PATCH]][[EMERGING_PACKET_OF_MOSY]]

  # EggQ: queue for egg batches (not used in Emerge; here for compatibility with mosquito ecology models only)
  MacroPatch$set(which = "private",name = "PatchesEggQ",
            value = NULL,
            overwrite = TRUE
  )

  MacroPatch$set(which = "public",name = "get_PatchesEggQ",
            value = get_PatchesEggQ,
            overwrite = TRUE
  )

  MacroPatch$set(which = "public",name = "set_PatchesEggQ",
            value = set_PatchesEggQ,
            overwrite = TRUE
  )

}


#################################################################
# Methods
#################################################################

#' MACRO: Calculate Emerging Adults for \code{MacroPatch}
#'
#' Write me! does this for all patches
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
addCohort_MacroEmerge <- function(){
  newM = self$get_MosquitoPointer()$get_M() + self$emergingAdults_MacroEmerge()
  self$get_MosquitoPointer()$set_M(M = newM, ix = NULL)
}

#' MACRO: Calculate Emerging Adults from ImagoQ for \code{MacroPatch}
#'
#' Write me! does this for all patches. Goes from ImagoQ to a vector of adults to be born, and zeros out
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
emergingAdults_MacroEmerge <- function(){

  # grab slots that are ready to emerge
  tNow = self$get_TilePointer()$get_tNow()
  newM = vector(mode="integer",length=private$N)
  for(ixP in 1:private$N){
    newM[ixP] = private$PatchesImagoQ[[ixP]]$N
    self$set_PatchesImagoQ(PatchesImagoQ = newImago(),ixP = ixP)
  }

  return(newM)
}

#' Get \code{MacroPatch} Seasonal Emergence
#'
#' Queue the ImagoQ
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
oneDay_MacroEmerge <- function(){

  tNow = self$get_TilePointer()$get_tNow()
  lambdaExact = vapply(X = private$season,FUN = function(x){x[floor(tNow)%%365+1]},FUN.VALUE = numeric(1))
  lambdaN = rpois(n = length(lambdaExact),lambda = lambdaExact)
  for(ixP in 1:private$N){
    self$set_PatchesImagoQ(PatchesImagoQ = newImago(N = lambdaN[ixP], tEmerge = tNow), ixP = ixP)
    # self$set_PatchesEggQ(PatchesEggQ = newEgg(), ixP = ixP) RESET EGG
  }

}


#################################################################
# Getters & Setters
#################################################################


#' Get \code{MacroPatch} Seasonal Emergence
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
get_season <- function(ixP = NULL){
  if(is.null(ixP)){
    return(private$season)
  } else {
    return(private$season[[ixP]])
  }
}

#' Set \code{MacroPatch} Seasonal Emergence
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
set_season <- function(season, ixP = NULL){
  if(is.null(ixP)){
    private$season = season
  } else {
    private$season[[ixP]] = season
  }
}

#' Get \code{MacroPatch} ImagoQ (Emerging Adults Queue)
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
get_PatchesImagoQ <- function(ixP = NULL){
  if(is.null(ixP)){
    return(private$PatchesImagoQ)
  } else {
    return(private$PatchesImagoQ[[ixP]])
  }
}

#' Set \code{MacroPatch} ImagoQ (Emerging Adults Queue)
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
set_PatchesImagoQ <- function(PatchesImagoQ, ixP = NULL){
  if(is.null(ixP)){
    private$PatchesImagoQ = PatchesImagoQ
  } else {
    private$PatchesImagoQ[[ixP]] = PatchesImagoQ
  }
}

#' Get \code{MacroPatch} EggQ (Egg Batches)
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
get_PatchesEggQ <- function(ixP = NULL){
  if(is.null(ixP)){
    return(private$PatchesEggQ)
  } else {
    return(private$PatchesEggQ[[ixP]])
  }
}

#' Set \code{MacroPatch} EggQ (Egg Batches)
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
set_PatchesEggQ <- function(PatchesEggQ, ixP = NULL){
  if(is.null(ixP)){
    private$PatchesEggQ = PatchesEggQ
  } else {
    private$PatchesEggQ[[ixP]] = PatchesEggQ
  }
}
