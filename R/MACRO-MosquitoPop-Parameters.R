#################################################################
#
#   MASH
#   R6-ified
#   MACRO MosquitoPop Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   May 26, 2017
#
#################################################################


#' Initialize MosquitoPop Parameters for \code{MacroMosquitoPop}
#'
#' make a list of pars for \code{\link{MacroMosquitoPop}}
#'
#' @param some stuff
#' @return return a list \code{MacroMosquitoPop_PAR}
#' @examples
#' MACRO.MosquitoPop.Parameters(N = 5)
#' @export
MACRO.MosquitoPop.Parameters <- function(N, M_density=10, p=0.9, f=0.3, Q=0.9, v=20, maxEIP=30, psi=NULL){

  list(
    p=p,
    f=f,
    Q=rep(Q,N),
    v=v,
    maxEIP=maxEIP,
    psi=psi,
    M_density=rep(M_density,N)
  )
}
