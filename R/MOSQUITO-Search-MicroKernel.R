#################################################################
#
#   MASH
#   R6-ified
#   MICRO-Search Kernels
#   David Smith, Hector Sanchez, Sean Wu
#   June 1, 2017
#
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
  dW = vapply(X = S,FUN = function(x){x$get_searchWt()},FUN.VALUE = numeric(1))
  dS2D = MicroKernel_DistanceMat(S,D)
  S2D = matrix(0,nrow=length(S),ncol=length(D))

  for(ix in 1:length(S)){
    allProb = dW^(-beta*dS2D[i,]) * (eps + dS2D[i,])^-sigma
    S2D[i,] = allProb / sum(allProb)
  }

  return(S2D)
}
