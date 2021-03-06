% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PATHOGEN-PfMOI-Methods.R
\name{init_MICRO_PfMOI}
\alias{init_MICRO_PfMOI}
\title{PfMOI \code{HumanPop} Method: Initialize MICRO PfMOI Infections}
\usage{
init_MICRO_PfMOI(PfMOI = NULL, tStart = 0)
}
\arguments{
\item{PfMOI}{a vector of length equal to \code{HumanPop$nHumans} giving the MOI of each person. If \code{NULL} all people are set to MOI of 0.}
}
\description{
Initialize PfMOI infections with MOI given by vector of MOI for population for MICRO or SimBitePfMOI
This method is bound to \code{HumanPop$init_MICRO_PfMOI()}
}
