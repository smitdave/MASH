#################################################################
#
#   MASH
#   MICRO Aquatic Ecology: EL4P
#   EL4P fitting
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 7, 2017
#
#################################################################

#################################################################
# EL4P Parameters
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

# MAYBE M SHOULD JUST BE A SCALAR OVER THE ENTIRE LANDSCAPE? CHECK THE DETERMINISTIC SOLUTIONS

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
#' @param alpha_a density independent survival parameter \code{alpha} from pupae to adult \deqn{-log(alpha_a^{\frac{1-p}{5}})}
#' @param alpha_sd standard deviation of density independent survival parameter \code{alpha}
#' @param p expected fraction of cohort that advances to next life stage (1/p is expected time spent in each stage L1,L2,L3,L4,P)
#' @param K_a shape parameter for gamma distributed weights on K
#' @param K_b scale parameter for gamma distributed weights on K
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
    alpha_a = 0.8,
    alpha_sd = 0.004,
    p = 0.9,
    K_a = 1,
    K_b = 1
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
      alpha_a = alpha_a,
      alpha_sd = alpha_sd,
      p = p,
      K_a = K_a,
      K_b = K_b
    )
    out$lambda = calcLambda_MicroEL4P(R0,EIP,lifespan,S)

    return(out)
}


#################################################################
# Main EL4P Fitting Functions
#################################################################

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
#' @param mesh_N number of sample points of K (density of mesh of K values used to fit psi)
#' @param var_tol target minimum variance in lambda for emergence for equilibrium to be assumed
#' @param plot produce diagnostic plots of fitting algorithm
#'
#' @md
#' @export
EL4P.Sample.Fit <- function(mesh_N, var_tol = 0.1, plot = FALSE){

  # sample initial values
  K_w = rgamma(n = mesh_N, shape = K_a, scale = K_b) # weights on K
  K = (lambda * K_w) / sum(K_w) # K for each pool (aquatic habitat)
  alpha_m = -log(alpha_a^((1-p)/5)) # mean of alpha mortality parameter
  alpha = abs(rnorm(n = mesh_N, mean = alpha_m, sd = alpha_sd)) # density-independet mortality
  psi_init = alpha/K # initial value of psi (density-dependent mortality)

  # do some plots
  if(plot){

  }

  # generate aquatic populations
  AquaPops = vector(mode="list",length=mesh_N)
  for(i in 1:mesh_N){
    AquaPops[[i]] = MASH::EL4P(numGenotypes=1,psi_new=psi_init[i],alpha_new=alpha[i],p_new=p)
  }

  # fit psi on a mesh of values for K
  rangeK = range(K)
  meshK_out = meshK_MicroEL4P(K_l=rangeK[1],K_u=rangeK[2],AquaPops=AquaPops,var_tol=var_tol)




}


#################################################################
# EL4P Fitting Auxiliary Functions
#################################################################

#' Fit Psi on Mesh of K Values
#'
#' Generate a mesh of K values in log space and fit psi to mesh via numerical optization, then run input EL4P populations to equilibrium.
#' This sets values of psi so that lambda \eqn{\lambda=K} at given parameter values.
#'
#' @param K_l lower bound of K mesh
#' @param K_u upper bound of K mesh
#' @param AquaPops list of EL4P objects (see \code{\link{EL4P}})
#' @param psi_min lower bound for fitting psi via one dimensional optimization
#' @param psi_max upper bound for fitting psi via one dimensional optimization
#' @param tMax maximum time to to run aquatic populations to equilibrium
#' @param var_tol target minimum variance in lambda for aquatic populations equilibrium
#' @return named list
#' * eq_pops: EL4P pool populations at equilibrium values
#' * psi_hat: fitted values of psi
#' * meshK: sampled values of K
#' @md
#' @export
meshK_MicroEL4P <- function(K_l, K_u, AquaPops, psi_min = 0, psi_max = 10, tMax = 500, var_tol = 0.1){

  # sample K on mesh in log space; transform to linear space
  meshK = exp(seq(log(K_l),log(K_u),length.out=length(AquaPops)))

  # fit EL4P; set values of psi so lambda = K at given parameter values
  print(paste0("fitting psi for all sample values of K"))
  psi_hat = parallel::mcmapply(FUN = function(psi_init, EL4P, M, eqAqua, K, G, lifespan, psi_min, psi_max){
    psi_optim_out = stats::optimize(f = psiFit_MicroEL4P,interval = c(psi_min,psi_max),EL4P = EL4P,M = M,eqAqua = eqAqua,K = K,G = G,lifespan = lifespan)
    return(psi_optim_out$minimum)
  },psi_init = psi_init, EL4P = AquaPops, M = M, eqAqua = eqAqua, K = meshK,
  MoreArgs = list(G=G,lifespan=lifespan,psi_min=psi_min,psi_max=psi_max),SIMPLIFY = FALSE,USE.NAMES = FALSE,mc.cores = parallel::detectCores()-2L)

  # run all EL4P pools to equilibrium values
  print(paste0("run EL4P pools to equilibrium values"))
  eq_pops = parallel::mcmapply(FUN = run2eq_MicroEL4P,psi = psi_hat, EL4P = AquaPops, M = M, eqAqua = eqAqua,
    MoreArgs = list(G=G,lifespan=lifespan,tMax=tMax,var_tol=var_tol),SIMPLIFY=FALSE,USE.NAMES=FALSE,mc.cores=parallel::detectCores()-2L)

  return(list(
      eq_pops = eq_pops, # EL4P pool populations at equilibrium values
      psi_hat = psi_hat, # fitted values of psi
      meshK = meshK # sampling mesh of K
    ))
}

#' Objective Function for Fitting Psi
#'
#' Given a input psi, \code{x}, run a single aquatic population and output objective function; squared error of lambda around the given value of K \eqn{\left ( \lambda-K \right )^{2}}.
#' This will typically be called by \code{\link{meshK_MicroEL4P}} or \code{optimize}; generally it is the objective function that will be passed to \code{optimize(...)}
#'
#' @param x value of psi from \code{optimize(...)}
#' @param EL4P an EL4P pool (see \code{\link{EL4P}})
#' @param M equilibrium mosquito density at this aquatic habitat
#' @param eqAqua vector of probability of oviposition at each aquatic habitat
#' @param K carrying capacity at this aquatic habitat
#' @param G mean total lifetime egg production of adult female
#' @param lifespan average lifespan of mosquito
#' @param tMax time to run EL4P pool before returning squared error of empirical lambda around value of K
#' @return value of objective function at \code{x}
#' @export
psiFit_MicroEL4P <- function(x, EL4P,  M, eqAqua, K, G, lifespan, tMax = 150){

  psi_iter = abs(x) # value of psi at this iteration of optimization
  EL4P$set_psi(psi_iter) # set psi in EL4P pool

  # initial burn-in of EL4P pool with psi.iter
  for(i in 1:30){
    EL4P$oneStep_GEL4P(M = M,eqAqua = eqAqua,G = G,lifespan = lifespan)
  }

  # run the EL4P pool to tMax
  for(i in 1:tMax){
    EL4P$oneStep_GEL4P(M = M,eqAqua = eqAqua,G = G,lifespan = lifespan)
    M = ((exp(-1/lifespan))*M) + EL4P$get_totalLambda() # simulate adult population dynamics from pool
  }

  return((EL4P$get_totalLambda() - K)^2)
}

#' Run a Single Aquatic Population to Equilibrium
#'
#' Run a single aquatic population to equilibrium (where variance in emergence, lambda is less than \col{tol}).
#' This function first runs the population through a burnin period (see \code{\link{burnin_GEL4P}}) then runs aquatic dynamics with simulated adult
#' dynamics from derived Ross-MacDonald parameters (see \code{\link{G2K_GEL4P}}). Then the dynamics are run while variance in lambda is above \code{tol}
#' (see \code{\link{checkDX_GEL4P}).
#'
#' @param psi value of density-dependent mortality psi parameter for this EL4P pool (output of \code{optimize} called on \code{\link{psiFit_MicroEL4P}})
#' @param EL4P
#' @param M
#' @param eqAqua
#' @param G
#' @param lifespan
#' @param tMax
#' @param tMax_checkDX
#' @param var_tol
#' @return return the EL4P pool populations at equilibrium values
#' @export
run2eq_MicroEL4P <- function(psi, EL4P, M, eqAqua, G, lifespan, tMax = 800, var_tol = 0.1){

  EL4P$set_psi(psi) # set new value of psi
  EL4P$burnIn_GEL4P(M,eqAqua,G,lifespan,tMax) # run aquatic stages burn-in
  EL4P$G2K_GEL4P(eqAqua,G,lifespan,tMax) # run with simulated adult dynamics
  pop_out = EL4P$get_allGenotypes() # store pop after running G2K_GEL4P
  tMax_checkDX = 100
  lambda_dx = EL4P$checkDX_GEL4P(eqAqua,G,lifespan,tMax_checkDX) # output lambda history

  # run until variance in lambda stabilizes
  while(var(lambda_dx) > var_tol){
    EL4P$set_pop(pop_out)
    EL4P$G2K_GEL4P(eqAqua,G,lifespan,tMax_checkDX)
    pop_out = EL4P$get_allGenotypes()
    lambda_dx = EL4P$checkDX_GEL4P(eqAqua,G,lifespan,tMax_checkDX)
    tMax_checkDX = tMax_checkDX + 100
  }

  return(pop_out)
}


#################################################################
# Psi vs. K
#################################################################

#' Regress K on Psi and Return Coefficients
#'
#' Run a linear regression of K on iverse of psi and extract coefficients; optionally plot the regression. Psi and K should follow a linear relationship, and
#' the coefficients of the linear regression will give the parameter of the 1-dimensional response surface that relates the two parameters.
#'
#' @param meshK
#' @param psi
#' @param plot
#' @return regression coefficients
psi2K_cf <- function(){

}

#' Convert K and Regression Coefficient into psi
#'
#' Write docs.
#'
#' @param K
#' @param cf
#' @return psi
#' @export
K2psi <- function(){

}

#' Plot Regression of K on Psi
#'
#' Write docs.
#'
#' @param lmFit NULL
#' @param K NULL
#' @param psi NULL
#' @param main NULL
#' @return nothing
psi2K_plot <- function(){

}






# placeholder
