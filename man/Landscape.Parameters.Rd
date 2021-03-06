% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MICRO-Landscape-Parameters.R
\name{Landscape.Parameters}
\alias{Landscape.Parameters}
\title{MICRO: Generate Parameters for \code{\link{Landscape}} Object}
\usage{
Landscape.Parameters(nFeed, nAqua, pointGen = "poisson", module, modulePars,
  hazV = 0, hazW = 0, hazI = 0, haz = 0, searchFeed = NULL,
  searchAqua = NULL, enterP = NULL, xLim = c(0, 1), yLim = c(0, 1),
  aquaSD = 0.025, ...)
}
\arguments{
\item{nFeed}{number of feeding sites}

\item{nAqua}{number of aquatic habitats}

\item{pointGen}{character to select spatial point pattern generation function
\itemize{
\item "poisson": \code{\link{pointsPoisson}}
\item "clustered": \code{\link{pointsClustered}}
\item "overdispersed": \code{\link{pointsOverdispersed}}
\item "lattice": \code{\link{pointsLattice}}
}}

\item{module}{character
\itemize{
\item "emerge": initialize parameters for Emerge module of Aquatic Ecology
\item "EL4P": initialize parameters for EL4P module of Aquatic Ecology
}}

\item{modulePars}{additional list of named parameters to be passed to Aquatic Ecology module specific parameter generating functions
\itemize{
\item Emerge: should be named list of parameters for \code{\link{makeLambda_MicroEmerge}}
\item EL4P: NULL
}}

\item{hazV}{mean value for feeding site vegetation landing hazard (if 0 it is set to 0 for all sites)}

\item{hazW}{mean value for feeding site outside wall landing hazard (if 0 it is set to 0 for all sites)}

\item{hazI}{mean value for feeding site indoor wall landing hazard (if 0 it is set to 0 for all sites)}

\item{haz}{mean value for aquatic habitat landing hazard (if 0 it is set to 0 for all sites)}

\item{searchFeed}{vector of searchWt for feeding sites (if \code{NULL} initialize to Gamma(1,1) distribution)}

\item{searchAqua}{vector of searchWt for aquatic habitats (if \code{NULL} initialize to Gamma(1,1) distribution)}

\item{enterP}{vector of house entry probabilities or single numeric value for all sites (if \code{NULL} initialize to Beta(9,1) distribution)}

\item{xLim}{x-axis bounds for simulated points}

\item{yLim}{y-axis bounds for simulated points}

\item{aquaSD}{standard deviation of aquatic habitat scatter around feeding sites}

\item{...}{additional named arguments for pointGen()}
}
\value{
a named list of parameters
\itemize{
\item FeedingSite_PAR: see \code{\link{Landscape.Feeding.Parameters}} for details
\item AquaticSite_PAR: see \code{\link{Landscape.Aqua.Parameters}} for details
}
}
\description{
This function is a specific instantiation of a generic system to generate parameters for a
chosen landscape. Any user-specified function can be written to generate parameters, as long as the
return list is in the same format.
}
\examples{

}
