% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PATHOGEN-PfMOI-SimBite.R
\name{SimBitePfMOI.Setup}
\alias{SimBitePfMOI.Setup}
\title{Initialize SimBite PfMOI Module}
\usage{
SimBitePfMOI.Setup(PfMOI_PAR = NULL, overwrite = TRUE)
}
\arguments{
\item{PfMOI_PAR}{optional parameter list generated by \code{\link{PfMOI.Parameters}}, if \code{NULL} this will be set to default values}
}
\value{
Defines a field (list) PfMOI_PAR in \code{\link{HumanPop}} and public methods in \code{\link{Human}}
}
\description{
Generate a list of parameters PfMOI_PAR in \code{\link{HumanPop}} and public methods in \code{\link{Human}}
}
\examples{
SimBitePfMOI.Setup()
}
