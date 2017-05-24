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
  MacroPatch$set(which = "private",name = "ImagoQ",
            value = NULL,
            overwrite = TRUE
  )

  MacroPatch$set(which = "public",name = "get_ImagoQ",
            value = get_ImagoQ,
            overwrite = TRUE
  )

  MacroPatch$set(which = "public",name = "set_ImagoQ",
            value = set_ImagoQ,
            overwrite = TRUE
  )

  # eventually may need push_ImagoQ and push_EggQ when doing mosquito genetics in MACRO; ie it will push to ImagoQ[[PATCH]][[EMERGING_PACKET_OF_MOSY]]

  # EggQ: queue for egg batches (not used in Emerge; here for compatibility with mosquito ecology models only)
  MacroPatch$set(which = "private",name = "EggQ",
            value = NULL,
            overwrite = TRUE
  )

  MacroPatch$set(which = "public",name = "get_EggQ",
            value = get_EggQ,
            overwrite = TRUE
  )

  MacroPatch$set(which = "public",name = "set_EggQ",
            value = set_EggQ,
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


  addUp = function(i){
    ix = Patches$aqua[[i]]$id
    sum(Patches$ImagoQ[ix])
  }
  sapply(1:Patches$N, addUp)
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
get_season <- function(ix = NULL){
  if(is.null(ix)){
    return(private$season)
  } else {
    return(private$season[[ix]])
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
set_season <- function(season, ix = NULL){
  if(is.null(ix)){
    private$season = season
  } else {
    private$season[[ix]] = season
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
get_ImagoQ <- function(ix = NULL){
  if(is.null(ix)){
    return(private$ImagoQ)
  } else {
    return(private$ImagoQ[[ix]])
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
set_ImagoQ <- function(ImagoQ, ix = NULL){
  if(is.null(ix)){
    private$ImagoQ = ImagoQ
  } else {
    private$ImagoQ[[ix]] = ImagoQ
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
get_EggQ <- function(ix = NULL){
  if(is.null(ix)){
    return(private$EggQ)
  } else {
    return(private$EggQ[[ix]])
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
set_EggQ <- function(EggQ, ix = NULL){
  if(is.null(ix)){
    private$EggQ = EggQ
  } else {
    private$EggQ[[ix]] = EggQ
  }
}
