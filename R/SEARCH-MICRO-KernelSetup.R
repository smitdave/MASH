#################################################################
#
#   MASH
#   R6-ified
#   MICRO-Search Kernels
#   David Smith, Hector Sanchez, Sean Wu
#   June 1, 2017
#
#################################################################


#################################################################
# Calculate MICRO Search Kernels
#################################################################

#' MICRO Search Kernels: Calculate Euclidean Distance Matrix
#'
#' Given pointers \code{S} and \code{D} to two lists of \code{MicroSite} objects,
#' Calculate the Euclidean distance matrix between them.
#'
#' @param S pointer to list of starting sites (for example, Landscape$get_AquaSites())
#' @param D pointer to list of destination sites
#' @return matrix
#' @export
MicroKernel_DistanceMat <- function(S, D) {
  Sxy = t(vapply(X = S,FUN = function(x){x$get_siteXY()},FUN.VALUE = numeric(2)))
  Dxy = t(vapply(X = D,FUN = function(x){x$get_siteXY()},FUN.VALUE = numeric(2)))
  dMat = matrix(0,nrow = length(S),ncol = length(D))

  for(i in 1:nrow(Sxy)){
    for(j in 1:nrow(Dxy)){
      dMat[i,j] = sqrt((Sxy[i,1]-Dxy[j,1])^2 + (Sxy[i,2]-Dxy[j,2])^2)
    }
  }
  return(dMat)
}

#' MICRO Search Kernels: Calculate Power Kernel
#'
#' Given pointers \code{S} and \code{D} to two lists of \code{MicroSite} objects,
#' Calculate one-step Markov transition matrix between sites.
#'
#' @param S pointer to starting sites (for example, Landscape$get_AquaSites())
#' @param D pointer to destination sites
#' @param sigma a param
#' @param eps a param
#' @param beta a param
#' @return matrix
#' @export
MicroKernel_PowerKernel <- function(S, D, sigma = 3, eps = 0.1, beta = 0){
  dW = vapply(X = D,FUN = function(x){x$get_searchWt()},FUN.VALUE = numeric(1))
  dS2D = MicroKernel_DistanceMat(S,D)
  S2D = matrix(0,nrow=length(S),ncol=length(D))

  for(ix in 1:length(S)){
    allProb = dW^(-beta*dS2D[ix,]) * (eps + dS2D[ix,])^-sigma
    S2D[ix,] = allProb / sum(allProb)
  }

  return(S2D)
}

#' MICRO Search Kernels: Sort ID and Probabilities
#'
#' Return sorted list of IDs and probabilities
#'
#' @param id vector of IDs
#' @param pr vector of probabilities
#' @return list
#'  * id: sorted IDs
#'  * pr: sorted and normalized probabilities
#' @md
#' @export
MicroKernel_prSort <- function(id, pr){
  #.prSort: Return sorted list of ids and probabilities
  ot = order(pr,decreasing = TRUE)
  return(list(
    id = id[ot],
    pr = pr[ot]/sum(pr)
  ))
}

#' MICRO Search Kernels: Exact Movement Object Between Two Point Sets
#'
#' Generate an exact movement object (one-step Markov transition matrix between two sets of sites).
#' The movement object is a list of lists where each element \code{ix} is the \code{\link{MicroKernel_PowerKernel}}
#' evaluated at starting location \code{ix}. If the destinations are the same sites as the starting locations
#' the mosquito is forced to change sites (ie; there are no one-step movements to the same site).
#'
#' @param S pointer to starting sites (for example, Landscape$get_AquaSites())
#' @param D pointer to destination sites
#' @param sigma a param
#' @param eps a param
#' @param beta a param
#' @return movement object; each element has the following structure:
#'  * ix: ID of starting site
#'  * PR: probabilities of near, around, and moveFar movement
#'  * near: the near movement object
#'    * id: sorted ID of destination sites
#'    * pr: sorted probabilities of destination sites
#'  * around:
#'  * moveFar:
#' @md
#' @export
MicroKernel_exactMvOb <- function(S,D,sigma=3,eps=0.1,beta=0){

  ixS = vapply(X = S,FUN = function(x){x$get_ix()},FUN.VALUE = integer(1)) #id of starting sites
  prS2D = MicroKernel_PowerKernel(S,D,sigma,eps,beta) #movement matrix between S to D
  MvOb = vector(mode="list",length = length(S)) #empty movement object
  nD = length(D) #number of D sites

  if(identical(S,D)){ # movement within same class of site
    for(i in 1:length(S)){
      id = (1:nD)[-i]
      pr = prS2D[i,-i]
      sortedPr = MicroKernel_prSort(id,pr)
      MvOb[[i]] = list(
        ix = ixS[i],
        PR = c(0,1,0), # PR = c(pr[i],1-pr[i],0),
        near = sortedPr,
        around = NULL,
        moveFar = NULL
      )
    }
  } else { # movement between classes of sites
    for(i in 1:length(S)){
      sortedPr = MicroKernel_prSort(1:nD,prS2D[i,])
      MvOb[[i]] = list(
        ix = ixS[i],
        PR = c(0,1,0), # PR = c(pr[i],1-pr[i],0),
        near = sortedPr,
        around = NULL,
        moveFar = NULL
      )
    }
  }

  return(MvOb)
}

#' MICRO Search Kernels: Exact Movement Object Between All Point Sets
#'
#' Generate an exact movement object (one-step Markov transition matrix between all sets of sites).
#' This function returns nested list of the following form for all site classes in landscape:
#'  * S2D: output of \code{\link{MicroKernel_exactMvOb}} applied to starting site point set S and destination point set D
#' The output, MvAll, will be assigned to \code{private$movement} field of \code{MicroMosquitoPop}. The specific accessor can be found at \code{\link{get_MicroKernel_movement}}.
#'
#' @param Landscape a microsimulation \code{\link{Landscape}} object
#' @param sigma a param
#' @param eps a param
#' @param beta a param
#' @return list of movement objects between all site classes (see above and \code{\link{MicroKernel_exactMvOb}}) for details.
#' @md
#' @export
MicroKernel_exactAll <- function(Landscape,sigma=3,eps=0.1,beta=0){

  MvAll = list()

  # move to feeding site
  MvAll$F2F = MicroKernel_exactMvOb(S = Landscape$get_FeedingSites(),D = Landscape$get_FeedingSites(),sigma,eps,beta)
  MvAll$L2F = MicroKernel_exactMvOb(S = Landscape$get_AquaSites(),D = Landscape$get_FeedingSites(),sigma,eps,beta)
  if(!is.null(Landscape$SugarSitesN)){
    MvAll$S2F = MicroKernel_exactMvOb(S = Landscape$get_SugarSites(),D = Landscape$get_FeedingSites(),sigma,eps,beta)
  }
  if(!is.null(Landscape$MatingSitesN)){
    MvAll$M2F = MicroKernel_exactMvOb(S = Landscape$get_MatingSites(),D = Landscape$get_FeedingSites(),sigma,eps,beta)
  }

  # move to aquatic habitat
  MvAll$F2L = MicroKernel_exactMvOb(S = Landscape$get_FeedingSites(),D = Landscape$get_AquaSites(),sigma,eps,beta)
  MvAll$L2L = MicroKernel_exactMvOb(S = Landscape$get_AquaSites(),D = Landscape$get_AquaSites(),sigma,eps,beta)
  if(!is.null(Landscape$SugarSitesN)){
    MvAll$S2L = MicroKernel_exactMvOb(S = Landscape$get_SugarSites(),D = Landscape$get_AquaSites(),sigma,eps,beta)
  }
  if(!is.null(Landscape$MatingSitesN)){
    MvAll$M2L = MicroKernel_exactMvOb(S = Landscape$get_MatingSites(),D = Landscape$get_AquaSites(),sigma,eps,beta)
  }

  # move to sugar site
  if(!is.null(Landscape$SugarSitesN)){
    MvAll$F2S = MicroKernel_exactMvOb(S = Landscape$get_FeedingSites(),D = Landscape$get_SugarSites(),sigma,eps,beta)
    MvAll$L2S = MicroKernel_exactMvOb(S = Landscape$get_AquaSites(),D = Landscape$get_SugarSites(),sigma,eps,beta)
    MvAll$S2S = MicroKernel_exactMvOb(S = Landscape$get_SugarSites(),D = Landscape$get_SugarSites(),sigma,eps,beta)
    if(!is.null(Landscape$MatingSitesN)){
      MvAll$M2S = MicroKernel_exactMvOb(S = Landscape$get_MatingSites(),D = Landscape$get_SugarSites(),sigma,eps,beta)
    }
  }

  # move to mating site
  if(!is.null(Landscape$MatingSitesN)){
    MvAll$F2M = MicroKernel_exactMvOb(S = Landscape$get_FeedingSites(),D = Landscape$get_MatingSites(),sigma,eps,beta)
    MvAll$M2M = MicroKernel_exactMvOb(S = Landscape$get_MatingSites(),D = Landscape$get_MatingSites(),sigma,eps,beta)
    MvAll$L2M = MicroKernel_exactMvOb(S = Landscape$get_AquaSites(),D = Landscape$get_MatingSites(),sigma,eps,beta)
    if(!is.null(Landscape$SugarSitesN)){
      MvAll$S2M = MicroKernel_exactMvOb(S = Landscape$get_SugarSites(),D = Landscape$get_MatingSites(),sigma,eps,beta)
    }
  }

  return(MvAll)
}

#' MICRO Search Kernels: \code{\link{MicroMosquito}} Sample MvOb
#'
#' This method is a helper for \code{\link{MicroKernel_moveMe}} and samples the appropriate MvOb in MvAll in the enclosing \code{\link{MicroMosquitoPop}} object.
#'
#' @param MvOb a movement object, see \code{\link{MicroKernel_exactMvOb}}
#' @return new index
#' @export
MicroKernel_SampleMvOb <- function(MvOb){

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
          browser("'around' movement not yet implemented")
        } else { #moveFar movement
          browser("'moveFar' movement not yet implemented")
        }
      }
    }
  })

  return(ixNew)
}


#################################################################
# MicroMosquitoFemale and MicroMosquitoMale 'moveMe' Methods
#################################################################

#' MICRO Search Kernels: \code{\link{MicroMosquitoFemale}} Movement Function for Full M-BITES Lifecycle Model
#'
#' Move one mosquito based on site and next behavioral state. This method is bound to \code{MicroMosquitoFemale$moveMe()}
#'
MicroKernel_moveMe_FULL <- function(){

  MvOb = private$FemalePopPointer$get_movement(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  pSetNew = switch(private$state,
      F = {"f"},
      L = {"l"},
      S = {"s"},
      M = {"m"},
      {private$state}
    )

  private$ix = MicroKernel_SampleMvOb(MvOb)
  private$inPointSet = pSetNew
}

#' MICRO Search Kernels: \code{\link{MicroMosquitoFemale}} Movement Function for Full M-BITES Lifecycle Model
#'
#' Move one mosquito based on site and next behavioral state. This method is bound to \code{MicroMosquitoFemale$moveMe()}
#'
MicroKernel_moveMe_BRO <- function(){

  MvOb = private$FemalePopPointer$get_movement(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  pSetNew = switch(private$state,
      B = {"f"},
      O = {"l"},
      {private$state}
    )

  private$ix = MicroKernel_SampleMvOb(MvOb)
  private$inPointSet = pSetNew
}

#' MICRO Search Kernels: \code{\link{MicroMosquitoFemale}} Movement Function for Full M-BITES Lifecycle Model
#'
#' Move one mosquito based on site and next behavioral state. This method is bound to \code{MicroMosquitoFemale$moveMe()}
#'
MicroKernel_moveMe_BROM <- function(){

  MvOb = private$FemalePopPointer$get_movement(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  pSetNew = switch(private$state,
      B = {"f"},
      O = {"l"},
      M = {"m"},
      {private$state}
    )

  private$ix = MicroKernel_SampleMvOb(MvOb)
  private$inPointSet = pSetNew
}

#' MICRO Search Kernels: \code{\link{MicroMosquitoMale}} Movement Function for Full M-BITES Lifecycle Model
#'
#' Move one mosquito based on site and next behavioral state. This method is bound to \code{MicroMosquitoMale$moveMe()}
#'
MicroKernel_moveMe_Male <- function(){

  MvOb = private$MalePopPointer$get_movement(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  pSetNew = switch(private$state,
      M = {"m"},
      S = {"s"},
      {private$state}
    )

  private$ix = MicroKernel_SampleMvOb(MvOb)
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


#################################################################
# Initialize Methods
#################################################################

#' MICRO Search Kernels: Initialize Additional Methods & Fields in \code{MicroMosquitoPop} and \code{MicroMosquito}
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

  MicroMosquitoPopMale$set(which = "public",name = "get_movement",
              value = get_MicroKernel_movement_Male,
              overwrite = overwrite
  )
  MicroMosquitoMale$set(which = "public",name = "moveMe",
              value = MicroKernel_moveMe_Male,
              overwrite = overwrite
  )


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
