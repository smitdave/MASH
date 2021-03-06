% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AQUA-Micro-Emerge-Methods.R
\name{set_lambda_MicroEmerge}
\alias{set_lambda_MicroEmerge}
\title{MICRO \code{\link{AquaticSite}} Method: Set Lambda}
\usage{
set_lambda_MicroEmerge(lambda, ix = NULL)
}
\arguments{
\item{lambda}{the object to insert; if \code{ix = NULL} then it should be vector of lambda values, see \code{\link{makeLambda_Macro}} for details, else it should be a numeric value.}

\item{ixQ}{if \code{NULL} set the entire ImagoQ, else, set the slot \code{ixQ}}
}
\description{
Set either a single day lambda or entire vector
This method is bound to \code{AquaticSite$set_lambda()}.
}
