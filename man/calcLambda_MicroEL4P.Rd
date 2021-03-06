% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AQUA-Micro-EL4P-Calibrate.R
\name{calcLambda_MicroEL4P}
\alias{calcLambda_MicroEL4P}
\title{Calculate Lambda for EL4P Aquatic Ecology Module}
\usage{
calcLambda_MicroEL4P(R0, EIP, lifespan, S, nH, b = 0.55, c = 0.15,
  r = 1/38)
}
\arguments{
\item{R0}{desired intensity of pathogen transmission}

\item{EIP}{length of the entomological incubation period}

\item{lifespan}{average lifespan of mosquito}

\item{S}{stability index; number of bites on humans over the average mosquito lifespan}

\item{nH}{number of humans on landscape}

\item{b}{mosquito to human transmission efficiency}

\item{c}{human to mosquito transmission efficiency}

\item{r}{rate of recovery in humans (1/average length of infection)}
}
\value{
equilibrium lambda
}
\description{
Calculate parameter \code{lambda} (daily adult female emergence over entire landscape) required to sustain \emph{Plasmodium falciparum} transmission at given value of \emph{R0} based on
classical Ross-MacDonald assumptions. This is used in EL4P fitting routines and called in \code{\link{EL4P.Parameters}} such that mean emergence over the landscape will be fitted to this equilibrium value.
\itemize{
\item Detailed derivations found in \url{https://doi.org/10.1186/1475-2875-3-13}
}
}
