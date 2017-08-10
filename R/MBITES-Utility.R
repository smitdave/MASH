#################################################################
#
#   MASH
#   M-BITES
#   Utilities for M-BITES
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 10, 2017
#
#################################################################

#' M-BITES Utility: Calculate Empirical Probability of Oviposition at \code{\link{AquaticSite}}
#'
#' Return vector of scaled frequencies of oviposition at aquatic habitats.
#'
#' @param history list of mosquito histories
#' @param nAqua number of aquatic habitats
#' @md
#' @export
ovipositionEq_utility <- function(history, nAqua){

  aquaVec = rep(0L,times=nAqua)

  aquaVecAll = parallel::mclapply(X = history,FUN = ovipositionEq_oneHistory_utility, aquaVec = aquaVec)
  aquaVec = Reduce(f = "+",x = aquaVecAll)

  return(aquaVec/sum(aquaVec))
}

# aquaIx_oneMosy: calculate count vector of visits to each aquatic habitat:
# function needs to be vectorized to run over the mosyPop list.

#' M-BITES Utility: Calculate Counts of Oviposition at \code{\link{AquaticSite}}
#'
#' Return vector of counts recording number of ovipositions at each site on landscape. Called by \code{\link{ovipositionEq_utility}}
#'
#' @param oneHistory single mosquito history
#' @param aquaVec vector of length equal to number of aquatic sites on \code{\link{Landscape}}
#' @md
#' @export
ovipositionEq_oneHistory_utility <- function(oneHistory, aquaVec){

  with(oneHistory,{
    if(!"O" %in% stateH){
      return(aquaVec)
    } else {
      oviIx = which(stateH == "O")
      for(ix in oviIx){
        aquaVec[ixH[oviIx]] = aquaVec[ixH[oviIx]] + 1L
      }
      return(aquaVec)
    }
  })
}
