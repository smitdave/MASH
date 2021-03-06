% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PATHOGEN-PfSI-Methods.R
\name{infectiousBite_PfSI}
\alias{infectiousBite_PfSI}
\title{PfSI \code{Human} Method: Infectious Bite on Human}
\usage{
infectiousBite_PfSI(tBite, PAR)
}
\arguments{
\item{tBite}{time of bite}

\item{mosquitoPfSI}{\code{\link{mosquitoPfSI}} object passed from mosquito to human}
}
\description{
This method is called from \code{\link{probeHost_PfSI}}.
If the infectious bite results in successful transmission, this function queues a human infection event, see \code{\link{add2Q_infectHumanPfSI}}
This method is bound to \code{Human$infectiousBite_PfSI()}
}
