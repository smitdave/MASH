% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PATHOGEN-PfMOI-Methods.R
\name{add2Q_gswanePfMOI}
\alias{add2Q_gswanePfMOI}
\title{PfMOI \code{Human} Event: Add PfMOI GS Waning Protection Event to Event Queue}
\usage{
add2Q_gswanePfMOI(tEvent, PAR = NULL)
}
\arguments{
\item{tEvent}{time of vaccination}

\item{PAR}{\code{NULL}}
}
\description{
Add PfMOI GS waning protection event to the event queue.
This method is called from \code{\link{gsvaccinatePfMOI}}
This method adds event \code{\link{event_gswanePfMOI}} to the event queue.
This method is bound to \code{Human$add2Q_gswanePfMOI()}
}
