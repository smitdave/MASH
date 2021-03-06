% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AQUA-Macro-Emerge.R
\name{makeLambda_Macro}
\alias{makeLambda_Macro}
\title{MACRO: Generate Seasonal Emergence (Lambda) for \code{Emerge} model of Aquatic Ecology}
\usage{
makeLambda_Macro(aquaPars)
}
\arguments{
\item{aquaPars}{a list of the following structure
\itemize{
\item lambda: vector of length equal to number of patches \code{\link{MacroPatch}} where each element is the number of emerging adult females per human per day averaged over one year (required)
\item lambdaWeight: vector of weights applied to each site (if not specified or set to \code{NULL} initialize to Gamma(1,1) distribution)
\item offset: vector of seasonal offsets in peak emergence applied to each site (if not specified or set to \code{NULL} initialize to 0 for all sites)
}}
}
\value{
list \code{lambda} where each element is the daily emergence for that \code{\link{MacroPatch}}
}
\description{
Generate lambda for all patches.
}
\examples{
makeLambda_Macro(aquaPars = list(lambda=c(5,10,15)))
}
