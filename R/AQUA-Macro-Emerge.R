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

    # when written, this should clear out the EggQ because its not used in Emerge
    # MacroPatch$set(which = "private",name = "oneDay_emerge"
    #           value = oneDay_emerge_MACRO,
    #           overwrite = TRUE
    # )


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
addCohort <- function(){
  newM = self$get_MosquitoPointer()$get_M() + self$emergingAdults()
  self$get_MosquitoPointer()$set_M(M = newM, ix = NULL)
}

#' MACRO: Calculate Emerging Adults from ImagoQ for \code{MacroPatch}
#'
#' Write me! does this for all patches
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
emergingAdults <- function(){

  # EVERY TIME STEP,CHECK READY QUEUES AND EMERGE THEM, ZERO OUT EMERGED SPOTS

  # as for manage_PatchesImagoQ queue length management; we need to check when in the original Emerge module of MASH.MBPT we did the management and duplicate it

  # # grab slots that are ready to emerge
  # tNow = self$get_TilePointer()$get_tNow()
  # newM = vector(mode="integer",length=private$N)
  # for(ixP in 1:private$N){
  #
  #   newAdultsIx = which(private$PatchesImagoQ[[ixP]]$tEmerge <= tNow)
  #   newM[[ixP]] = vapply(X = private$PatchesImagoQ[[ixP]][newAdultsIx],FUN = function(x){x$N},FUN.VALUE = numeric(1))
  #
  #
  #
  #   private$PatchesImagoQ[[ixP]][newAdultsIx]
  # }
  #
  # addUp = function(i){
  #   ix = Patches$aqua[[i]]$id
  #   sum(Patches$ImagoQ[ix])
  # }
  # sapply(1:Patches$N, addUp)
}

#' Get \code{MacroPatch} Seasonal Emergence
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
oneDay_emerge_MACRO <- function(tNow){
  lambdaExact = sapply(LANDSCAPE$aquaSites,function(x){x$season[floor(tNow)%%365+1]}) #exact emergence rates

  nSites = length(lambdaExact)
  rpois(n = nSites,lambda = lambdaExact)
}


#################################################################
# Queue Management
#################################################################

#' Extend \code{MacroPatch} ImagoQ (Emerging Adults Queue)
#'
#' Extend the ImagoQ for patch \code{ixP}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
extend_PatchesImagoQ <- function(ixP, N=10L){
  private$PatchesImagoQ[[ixP]] = c(private$PatchesImagoQ[[ixP]],allocImagoQ(N))
}

#' Manage \code{MacroPatch} ImagoQ (Emerging Adults Queue)
#'
#' If all the ImagoQ slots for patch \code{ixP} are full then call \code{\link{extend_PatchesImagoQ}}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
manage_PatchesImagoQ <- function(ixP, N=10L){
  if(all(vapply(X = private$PatchesImagoQ[[ixP]],FUN = function(x){x$N},FUN.VALUE = numeric(1)) != 0)){
    self$extend_PatchesImagoQ(ixP=ixP,N=N)
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
