% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PATHOGEN-PfMOI-Methods.R
\name{endPfMOI}
\alias{endPfMOI}
\title{PfMOI \code{Human} Event: PfMOI Infection Event}
\usage{
endPfMOI(tEvent, PAR)
}
\arguments{
\item{tEvent}{time of clearance}

\item{PAR}{named list
\itemize{
\item PfID: PfID of the infection to end
}}
}
\description{
Clear a PfMOI infection corresponding to the given PfID.
This method is bound to \code{Human$endPfMOI()}
}
