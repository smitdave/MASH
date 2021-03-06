% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PATHOGEN-PfSI-Methods.R
\name{endPfSI}
\alias{endPfSI}
\title{PfSI \code{Human} Event: PfSI Clearance Event}
\usage{
endPfSI(tEvent, PAR)
}
\arguments{
\item{tEvent}{time of clearance}

\item{PAR}{\code{NULL}}
}
\description{
Clear a PfSI infection. If the human is infected, set susceptible and track history.
This method is bound to \code{Human$endPfSI()}
}
