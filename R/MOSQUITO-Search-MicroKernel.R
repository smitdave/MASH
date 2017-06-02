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
#' @param S pointer to starting sites (for example, Landscape$get_AquaSites())
#' @param D pointer to destination sites
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
