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
#' @param alpha.a density independent survival parameter \code{alpha} from pupae to adult \deqn{-log(alpha.a^{\frac{1-p}{5}})}
#' @param alpha.sd standard deviation of density independent survival parameter \code{alpha}
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
    alpha.a = 0.8,
    alpha.sd = 0.004,
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
      alpha.a = alpha.a,
      alpha.sd = alpha.sd,
      p = p,
      K.a = K.a,
      K.b = K.b
    )
    out$lambda = calcLambda_MicroEL4P(R0,EIP,lifespan,S)

    return(out)
}

#' Fit EL4P Aquatic Ecology Module to \code{\link{Landscape}}
#'
#' Fit the EL4P Aquatic Ecology module on the exact LANDSCAPE to match a desired level of daily emergence at equilibrium.
#' This means that the site-specific density dependent mortality parameter \code{psi} will be fit
#' to K for each aquatic habitat on the landscape, and then each site will be run to equilibrium. Compare with \code{\link{setupAquaPop_EL4PsamplePoints}} which will
#' fit \code{psi} based on a sampling grid of K.
#'
#' @md
#' @export
EL4P.Exact.Fit <- function(){

}

#' Fit EL4P Aquatic Ecology Module to Sample Grid
#'
#' Fit the EL4P Aquatic Ecology module on a sampling grid of K values to match a desired level of daily emergence at equilibrium.
#' This means that the site-specific density dependent mortality parameter \code{psi} will be fit
#' to K based on a sampling grid of values for K in log-space. If \code{plot = TRUE}, the linear regression of \code{psi} against logged values of K
#' should show exact linear dependence, indicating fitted \code{psi} will produce desired level of lambda at equilibrium. Compare with \code{\link{setupAquaPop_EL4Pexact}} which will
#' fit \code{psi} based on an exact LANDSCAPE. This will also return the coefficients of a linear regression of K on psi (see \code{\link{psi2K_cf}}) to give the functional relationship between K and psi.
#'
#' @param mesh.N number of sample points of K (density of mesh of K values used to fit psi)
#' @param var.tol target minimum variance in lambda for emergence for equilibrium to be assumed
#' @param plot produce diagnostic plots of fitting algorithm
#'
#' @md
#' @export
EL4P.Sample.Fit <- function(mesh.N, var.tol = 0.1, plot = FALSE){

  # sample initial values
  K.w = rgamma(n = mesh.N, shape = K.a, scale = K.b) # weights on K
  K = (lambda * K.w) / sum(K.w) # K for each pool (aquatic habitat)
  alpha.m = -log(alpha.a^((1-p)/5)) # mean of alpha mortality parameter
  alpha = abs(rnorm(n = mesh.N, mean = alpha.m, sd = alpha.sd)) # density-independet mortality
  psi.init = alpha/K # initial value of psi (density-dependent mortality)

  # do some plots
  if(plot){

  }

  # generate aquatic populations
  AquaPops = vector(mode="list",length=mesh.N)
  for(i in 1:mesh.N){
    AquaPops[[i]] = MASH::EL4P(numGenotypes=1,psi_new=psi.init[i],alpha_new=alpha[i],p_new=p)
  }

  # fit psi on a mesh of values for K
  K.range = range(K)


}


#################################################################
#
# EL4P Fitting Routines
# set values of psi so that lambda = K at (p,G)
# G :: lifetime egg production, per adult (tBatchC from MBITES-BASIC)
#
#################################################################

#' Fit Psi on Mesh of K Values
#'
#' Generate a mesh of K values in log space and fit psi to mesh via numerical optization, then run input EL4P populations to equilibrium.
#' This sets values of psi so that lambda \eqn{\lambda=K} at given parameter values.
#'
#' @param K.l lower bound of K mesh
#' @param K.u upper bound of K mesh
#' @param AquaPops list of EL4P objects (see \code{\link{EL4P}})
#' @param psi.min lower bound for fitting psi via one dimensional optimization
#' @param psi.max upper bound for fitting psi via one dimensional optimization
#' @param tMax maximum time to to run aquatic populations to equilibrium
#' @param var.tol target minimum variance in lambda for aquatic populations equilibrium
#' @return named list
#' * EL4P_pops: aquatic populations run to equilibrium values
#' * psi.hat: fitted values of psi
#' * K.mesh: sampled values of K
#' @md
#' @export
meshK_MicroEL4P <- function(K.l, K.u, AquaPops, psi.min = 0, psi.max = 10, tMax = 500, var.tol = 0.1){

  # sample K on mesh in log space; transform to linear space
  K.mesh = exp(seq(log(K.l),log(K.u),length.out=length(AquaPops)))
  # psi.hat = vector(mode="numeric",length=length(AquaPops))

  # fit EL4P: set values of psi so lambda = K at (p,G)
  # for(ix in 1:length(AquaPops)){
  #   print(paste0("fitting psi for site ix: ",ix, ", target K: ",K.mesh[ix]))
  #   psi.optim = optimize(f = psiFit_MicroEL4P,interval = c(psi.min,psi.max),EL4P=AquaPops[[ix]],M=M[ix],eqAqua=eqAqua[ix],K=K[ix],G=G,lifespan=lifespan)
  #   psi.hat[ix] = psi.optim$minimum
  # }

  # fit EL4P; set values of psi so lambda = K at (p,G)
  print(paste0("fitting psi for all sample values of K"))
  psi.hat = parallel::mcmapply(FUN = function(psi.init, EL4P, M, eqAqua, K, G, lifespan, psi.min, psi.max){
    psi.optim.out = stats::optimize(f = psiFit_MicroEL4P,interval = c(psi.min,psi.max),EL4P = EL4P,M = M,eqAqua = eqAqua,K = K,G = G,lifespan = lifespan)
    return(psi.optim.out$minimum)
  },psi.init = psi.init, EL4P = AquaPops, M = M, eqAqua = eqAqua, K = K, MoreArgs = list(G=G,lifespan=lifespan,psi.min=psi.min,psi.max=psi.max),SIMPLIFY = FALSE,USE.NAMES = FALSE)

  # run all EL4P pools to equilibrium values
  print(paste0("run EL4P pools to equilibrium values"))


}

#' Objective Function for Fitting Psi
#'
#' Given a input psi, \code{x}, run a single aquatic population and output objective function; squared error of lambda around the given value of K \eqn{\left ( \lambda-K \right )^{2}}.
#' This will typically be called by \code{\meshK_MicroEL4P{meshK_EL4P}} or \code{optimize}; generally it is the objective function that will be passed to \code{optimize(...)}
#'
#' @param x value of psi from \code{optimize(...)}
#' @param EL4P an EL4P pool (see \code{\link{EL4P}})
#' @param M equilibrium mosquito density at this aquatic habitat
#' @param eqAqua equilibrium mosquito density at this aquatic habitat
#' @param G mean total lifetime egg production of adult female
#' @param lifespan average lifespan of mosquito
#' @param K carrying capacity at this aquatic habitat
#' @param tMax time to run EL4P pool before returning squared error of empirical lambda around value of K
#' @return value of objective function at \code{x}
#' @export
psiFit_MicroEL4P <- function(x, EL4P,  M, eqAqua, K, G, lifespan, tMax = 150){

  psi.iter = abs(x) # value of psi at this iteration of optimization
  EL4P$set_psi(psi_new = psi.iter) # set psi in EL4P pool

  # initial burn-in of EL4P pool with psi.iter
  for(i in 1:30){
    EL4P$oneStep_GEL4P(M = M,eqAqua = eqAqua,G = G,lifespan = lifespan)
  }

  # run the EL4P pool to tMax
  for(i in 1:tMax){
    EL4P$oneStep_GEL4P(M = M,eqAqua = eqAqua,G = G,lifespan = lifespan)
    M = ((exp(-1/lifespan))*M) + EL4P$get_totLambda() # simulate adult population dynamics from pool
  }

  return((EL4P$get_totLambda() - K)^2)
}
















# placeholder
