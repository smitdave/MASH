#################################################################
#
#   MASH
#   MICRO Aquatic Ecology: EL4P
#   EL4P fitting
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 7, 2017
#
#################################################################

#' Calculate Lambda for EL4P Aquatic Ecology Module
#'
#' Calculate parameter \code{lambda} (daily adult female emergence over entire landscape) required to sustain \emph{Plasmodium falciparum} transmission at given value of \emph{R0} based on
#' classical Ross-MacDonald assumptions. This is used in EL4P fitting routines such that mean emergence over the landscape will be fitted to this equilibrium value.
#'  * Detailed derivations found in \url{https://doi.org/10.1186/1475-2875-3-13}
#'
#' @param R0 desired intensity of pathogen transmission
#' @param EIP length of the entomological incubation period
#' @param lifespan average lifespan of mosquito
#' @param S stability index; number of bites on humans over the average mosquito lifespan
#' @param b mosquito to human transmission efficiency
#' @param c human to mosquito transmission efficiency
#' @return equilibrium lambda
#' @md
#' @export
calcLambda_MicroEL4P <- function(R0, EIP, lifespan, S){

  P = exp(-EIP/lifespan)

  return(
    (R0*nH*r) / ((S^2)*b*c*P)
  )
}


#' Initialize EL4P Aquatic Ecoogy Module Parameters
#'
#' Generate named list of EL4P parameters, and calculates lambda (see \code{\link{calcLambda_MicroEL4P}} for details).
#'
#' @param nAqua number of aquatic habitats on landscape
#' @param nHumans number of humans on landscape
#' @param R0 desired pathogen transmission level at equilibrium
#' @param M vector of densities of mosquitoes at each aquatic habitat
#' @param eqAqua vector of probability of oviposition at each aquatic habitat
#' @param EIP length of the entomological incubation period
#' @param lifespan average lifespan of mosquito
#' @param G mean total lifetime egg production of adult female
#' @param nu mean egg batch size for single oviposition
#' @param S stability index; number of bites on humans over the average mosquito lifespan
#' @param alpha.m mean of density independent survival parameter \code{alpha} from pupae to adult
#' @param p expected fraction of cohort that advances to next life stage (1/p is expected time spent in each stage L1,L2,L3,L4,P)
#' @param K.a shape parameter for gamma distributed weights on K
#' @param K.b scale parameter for gamma distributed weights on K
#' @md
#' @export
EL4P.Parameters <- function(
    nAqua,
    nHumans,
    R0,
    M,
    eqAqua,
    EIP,
    lifespan,
    G,
    nu,
    S,
    alpha.m = 0.8,
    p = 0.9,
    K.a = 1,
    K.b = 1
  ){

    out = list(
      nAqua = nAqua,
      nHumans = nHumans,
      R0 = R0,
      M = M,
      eqAqua = eqAqua,
      EIP = EIP,
      lifespan = lifespan,
      G = G,
      nu = nu,
      S = S,
      alpha.m = alpha.m,
      p = p,
      K.a = K.a,
      K.b = K.b
    )
    out$lambda = calcLambda_MicroEL4P(R0,EIP,lifespan,S)

    return(out)
}




# # calculate initial parameter values
# W = rgamma(n = nA,shape = a,scale = b)
# K = (lambda*W) / sum(W)
# pp = -log(P^((1-p)/5))
# alpha = abs(rnorm(n = nA,mean = pp,sd = 0.004))
# psi = alpha/K
