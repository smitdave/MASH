#################################################################
#
#   MASH
#   MICRO: MBITES Search Movement
#   David Smith, Hector Sanchez, Sean Wu
#   June 1, 2017
#
#################################################################

#################################################################
# Initialize Methods
#################################################################

#' MICRO Search Kernels: Initialize Additional Methods & Fields in \code{\link{MicroMosquitoPopFemale}} and \code{\link{MicroMosquitoFemale}}
#'
#' Initialize MICRO Search Kernels module of mosquito search behavior.
#'
#' @param MBITES what M-BITES module to run?
#'  * BRO: Blood Feeding, Resting, Oviposition module
#'  * BROM: Blood Feeding, Resting, Oviposition, Mating module
#'  * FULL: Full life cycle M-BITES module
#' @md
#' @return does stuff
#' @examples
#' SEARCH.MicroKernel.Setup()
#' @export
SEARCH.MicroKernel.Setup <- function(MBITES = "BRO", overwrite = TRUE){

  message(paste0("initializing MICRO component methods & fields for MicroMosquitoPop & MicroMosquito Class for M-BITES module: ",MBITES))

  #################################################################
  # Male Movement
  #################################################################

  # male movement does not change betwen female lifecycle modules
  MicroMosquitoPopMale$set(which = "public",name = "get_movement",
              value = get_MicroKernel_movement_Male,
              overwrite = overwrite
  )
  MicroMosquitoMale$set(which = "public",name = "moveMe",
              value = MicroKernel_moveMe_Male,
              overwrite = overwrite
  )

  # generic movement methods
  MicroMosquitoPopFemale$set(which = "public",name = "SampleMove",
              value = MicroKernel_SampleMvOb_MicroMosquitoPopFemale,
              overwrite = overwrite
  )

  # MicroMosquitoPopMale$set(which = "public",name = "SampleMove",
  #             value = MicroKernel_SampleMvOb_MicroMosquitoPopMale,
  #             overwrite = overwrite
  # )

  #################################################################
  # Female Movement
  #################################################################

  if(MBITES == "BRO"){

    MicroMosquitoPopFemale$set(which = "public",name = "get_movement",
                value = get_MicroKernel_movement_BRO,
                overwrite = overwrite
    )
    MicroMosquitoFemale$set(which = "public",name = "moveMe",
                value = MicroKernel_moveMe_BRO,
                overwrite = overwrite
    )

  } else if(MBITES == "BROM"){

    MicroMosquitoPopFemale$set(which = "public",name = "get_movement",
                value = get_MicroKernel_movement_BROM,
                overwrite = overwrite
    )
    MicroMosquitoFemale$set(which = "public",name = "moveMe",
                value = MicroKernel_moveMe_BROM,
                overwrite = overwrite
    )

  } else if(MBITES == "FULL"){

    MicroMosquitoPopFemale$set(which = "public",name = "get_movement",
                value = get_MicroKernel_movement_FULL,
                overwrite = overwrite
    )
    MicroMosquitoFemale$set(which = "public",name = "moveMe",
                value = MicroKernel_moveMe_FULL,
                overwrite = overwrite
    )

  } else {
    stop("argument MBITES must be a character in 'BRO', 'BROM', or 'FULL'")
  }

}


#################################################################
# 'MicroKernel_SampleMvOb' Method
#################################################################

#' MICRO Search Kernels: \code{\link{MicroMosquito}} Sample MvOb
#'
#' This method is a helper for \code{\link{MicroKernel_moveMe}} and samples the appropriate MvOb in MvAll in the enclosing \code{\link{MicroMosquitoPopFemale}} object.
#'  * This function is bound to \code{MicroMosquitoPopFemale$SampleMove}
#'
#' @param ixS: current site index \code{ix} of this mosquito
#' @param state: current behavioral state of this mosquito
#' @param inPointSet: current point set this mosquito is in
#' @return new index
#' @md
MicroKernel_SampleMvOb_MicroMosquitoPopFemale <- function(ixS, state, inPointSet){

  MvOb = self$get_movement(ixS,state,inPointSet)

  x = runif(1)

  ixNew = with(MvOb,{
    if(x <= PR[1]){ #no movement
      ix
    } else {
      if(x <= PR[1] + PR[2]){ #near movement
        ixNear = sample(x = 1:length(near$id),size = 1,prob = near$pr)
        near$id[ixNear]
      } else {
        if(x <= sum(PR)){ #around movement
          stop("'around' movement not yet implemented")
        } else { #moveFar movement
          stop("'moveFar' movement not yet implemented")
        }
      }
    }
  })

  return(ixNew)
}

# need to write the above function for males
# MicroKernel_SampleMvOb_MicroMosquitoPopMale


#################################################################
# MicroMosquitoFemale and MicroMosquitoMale 'moveMe' Methods
#################################################################

#' MICRO Search Kernels: \code{\link{MicroMosquitoFemale}} Movement Function for Full M-BITES Lifecycle Model
#'
#' Move one mosquito based on site and next behavioral state. This method is bound to \code{MicroMosquitoFemale$moveMe()}
#'
MicroKernel_moveMe_FULL <- function(){

  # MvOb = private$FemalePopPointer$get_movement(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  pSetNew = switch(private$state,
      F = {"f"},
      L = {"l"},
      S = {"s"},
      M = {"m"},
      {private$inPointSet}
    )

  # private$ix = MicroKernel_SampleMvOb(MvOb)
  private$ix = private$FemalePopPointer$SampleMove(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  private$inPointSet = pSetNew
}

#' MICRO Search Kernels: \code{\link{MicroMosquitoFemale}} Movement Function for Full M-BITES Lifecycle Model
#'
#' Move one mosquito based on site and next behavioral state.
#'  * This method is bound to \code{MicroMosquitoFemale$moveMe()}
#'
#' @md
MicroKernel_moveMe_BRO <- function(){

  # # MvOb = private$FemalePopPointer$get_movement(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  # pSetNew = switch(private$state,
  #     B = {"f"},
  #     O = {"l"},
  #     {private$inPointSet}
  #   )
  #
  # # private$ix = MicroKernel_SampleMvOb(MvOb)
  # private$ix = private$FemalePopPointer$SampleMove(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  # private$inPointSet = pSetNew

  switch(private$state,

      B = {
          private$ix = private$FemalePopPointer$SampleMove(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
          private$inPointSet = "f"
        },
      O = {
          private$ix = private$FemalePopPointer$SampleMove(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
          private$inPointSet = "l"
        },
      {return(NULL)}
    )

}

#' MICRO Search Kernels: \code{\link{MicroMosquitoFemale}} Movement Function for Full M-BITES Lifecycle Model
#'
#' Move one mosquito based on site and next behavioral state. This method is bound to \code{MicroMosquitoFemale$moveMe()}
#'
MicroKernel_moveMe_BROM <- function(){

  # MvOb = private$FemalePopPointer$get_movement(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  pSetNew = switch(private$state,
      B = {"f"},
      O = {"l"},
      M = {"m"},
      {private$inPointSet}
    )

  # private$ix = MicroKernel_SampleMvOb(MvOb)
  private$ix = private$FemalePopPointer$SampleMove(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  private$inPointSet = pSetNew
}

#' MICRO Search Kernels: \code{\link{MicroMosquitoMale}} Movement Function for Full M-BITES Lifecycle Model
#'
#' Move one mosquito based on site and next behavioral state. This method is bound to \code{MicroMosquitoMale$moveMe()}
#'
MicroKernel_moveMe_Male <- function(){

  # MvOb = private$MalePopPointer$get_movement(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  pSetNew = switch(private$state,
      M = {"m"},
      S = {"s"},
      {private$inPointSet}
    )

  # private$ix = MicroKernel_SampleMvOb(MvOb)
  private$ix = private$FemalePopPointer$SampleMove(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  private$inPointSet = pSetNew
}


#################################################################
# MicroMosquitoPopFemale and MicroMosquitoPopMale 'get_movement' Methods
#################################################################

#' MICRO Search Kernels: \code{\link{MicroMosquitoPopFemale}} Access MvAll Object for Full M-BITES Lifecycle Model
#'
#' Replace generic \code{MicroMosquitoPopFemale$get_movement()} method for MicroKernel module; it will be bound to \code{MicroMosquitoPopFemale$get_movement()}
#'
get_MicroKernel_movement_FULL<- function(ixS, state, inPointSet){
  switch(state,
    F = {
        if(inPointSet=="f"){return(private$movement$F2F[[ixS]])}
        if(inPointSet=="s"){return(private$movement$S2F[[ixS]])}
        if(inPointSet=="m"){return(private$movement$M2F[[ixS]])}
        if(inPointSet=="l"){return(private$movement$L2F[[ixS]])}
      },
    L = {
        if(inPointSet=="f"){return(private$movement$F2L[[ixS]])}
        if(inPointSet=="s"){return(private$movement$S2L[[ixS]])}
        if(inPointSet=="m"){return(private$movement$M2L[[ixS]])}
        if(inPointSet=="l"){return(private$movement$L2L[[ixS]])}
      },
    S = {
        if(inPointSet=="f"){return(private$movement$F2S[[ixS]])}
        if(inPointSet=="s"){return(private$movement$S2S[[ixS]])}
        if(inPointSet=="m"){return(private$movement$M2S[[ixS]])}
        if(inPointSet=="l"){return(private$movement$L2S[[ixS]])}
      },
    M = {
        if(inPointSet=="f"){return(private$movement$F2M[[ixS]])}
        if(inPointSet=="s"){return(private$movement$S2M[[ixS]])}
        if(inPointSet=="m"){return(private$movement$M2M[[ixS]])}
        if(inPointSet=="l"){return(private$movement$L2M[[ixS]])}
      },
    {return(NULL)}
  )
}

#' MICRO Search Kernels: \code{\link{MicroMosquitoPopFemale}} Access MvAll Object for M-BITES BRO Lifecycle Model
#'
#' Replace generic \code{MicroMosquitoPopFemale$get_movement()} method for MicroKernel module; it will be bound to \code{MicroMosquitoPopFemale$get_movement()}
#'
get_MicroKernel_movement_BRO <- function(ixS, state, inPointSet){
  switch(state,
    B = {
        if(inPointSet=="f"){return(private$movement$F2F[[ixS]])}
        if(inPointSet=="l"){return(private$movement$L2F[[ixS]])}
      },
    O = {
        if(inPointSet=="f"){return(private$movement$F2L[[ixS]])}
        if(inPointSet=="m"){return(private$movement$M2L[[ixS]])}
      },
    {return(NULL)}
  )}

#' MICRO Search Kernels: \code{\link{MicroMosquitoPopFemale}} Access MvAll Object for M-BITES BROM Lifecycle Model
#'
#' Replace generic \code{MicroMosquitoPopFemale$get_movement()} method for MicroKernel module; it will be bound to \code{MicroMosquitoPopFemale$get_movement()}
#'
get_MicroKernel_movement_BROM <- function(ixS, state, inPointSet){
  switch(state,
    B = {
        if(inPointSet=="f"){return(private$movement$F2F[[ixS]])}
        if(inPointSet=="m"){return(private$movement$M2F[[ixS]])}
        if(inPointSet=="l"){return(private$movement$L2F[[ixS]])}
      },
    O = {
        if(inPointSet=="f"){return(private$movement$F2L[[ixS]])}
        if(inPointSet=="m"){return(private$movement$M2L[[ixS]])}
        if(inPointSet=="l"){return(private$movement$L2L[[ixS]])}
      },
    M = {
        if(inPointSet=="f"){return(private$movement$F2M[[ixS]])}
        if(inPointSet=="m"){return(private$movement$M2M[[ixS]])}
        if(inPointSet=="l"){return(private$movement$L2M[[ixS]])}
      },
    {return(NULL)}
  )
}

#' MICRO Search Kernels: \code{\link{MicroMosquitoPopMale}} Access MvAll Object for M-BITES BROM Lifecycle Model
#'
#' Replace generic \code{MicroMosquitoPopMale$get_movement()} method for MicroKernel module; it will be bound to \code{MicroMosquitoPopMale$get_movement()}
#'
get_MicroKernel_movement_Male <- function(ixS, state, inPointSet){
  switch(state,
    M = {
        if(inPointSet=="m"){return(private$movement$M2M[[ixS]])}
        if(inPointSet=="s"){return(private$movement$S2M[[ixS]])}
      },
    S = {
        if(inPointSet=="m"){return(private$movement$M2S[[ixS]])}
        if(inPointSet=="s"){return(private$movement$S2S[[ixS]])}
      },
    {return(NULL)}
  )
}
