#################################################################
#
#   MASH
#   R6-ified
#   PfMOI Methods
#   David Smith, Hector Sanchez, Sean Wu
#   May 21, 2017
#
#################################################################


#################################################################
# Event Timing
#################################################################

#' PfMOI \code{Human} Method: Duration of Infection
#'
#' How many days does a single PfMOI infection last? See \code{\link{PfMOI.Parameters}} parameter \code{Pf_r} for baseline clearance rate.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttClearPfMOI()}
#'
ttClearPfMOI <- function(){
  rexp(n=1, rate=private$PfMOI_PAR$Pf_r)
}

#' PfMOI \code{Human} Method: Duration of Latency
#'
#' How many days after the infectious bite does a PfMOI infection start? See \code{\link{PfMOI.Parameters}} parameter \code{Pf_latent} constant latent period.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttInfectPfMOI()}
#'
ttInfectPfMOI <- function(){
  private$PfMOI_PAR$Pf_latent
}

#' PfMOI \code{Human} Method: Timing of Fever Incident
#'
#' What is the timing of fever relative to the start of a PfMOI infection? See \code{\link{PfMOI.Parameters}} parameter \code{Pf_ttF} mean (on natural scale) and parameter \code{Pf_ttFvar} standard deviation (on natural scale) of log-normally distributed time to fever.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttFeverPfMOI()}
#'
ttFeverPfMOI <- function(){
  rlnorm(1,log(private$PfMOI_PAR$Pf_ttF),private$PfMOI_PAR$Pf_ttFvar)
}

#' PfMOI \code{Human} Method: Timing of Treatment
#'
#' What is the timing of treatment relative to the start of a fever incident? See \code{\link{PfMOI.Parameters}} parameter \code{Pf_ttT} for mean waiting time of exponentially distributed waiting period.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttTreatPfMOI()}
#'
ttTreatPfMOI <- function(){
  rexp(1, 1/private$PfMOI_PAR$Pf_ttT)
}


#' PfMOI \code{Human} Method: Duration of Protection from Chemoprophylaxis
#'
#' After administration of treatment what is time to susceptibility? See \code{\link{PfMOI.Parameters}} parameter \code{Pf_ttS} constant timing period.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttSusceptiblePfMOI()}
#'
ttSusceptiblePfMOI <- function(){private$PfMOI_PAR$Pf_ttS}


###################################################################
# PfSI Diagnostics
###################################################################

#' PfMOI \code{Human} Method: Rapid Diagnostic Test
#'
#' Administer RDT to this human.
#'  * if infected: true positive is detected with probability \code{rdtSensPf}, see \code{\link{PfMOI.Parameters}}
#'  * if susceptible: false positive is detected with probability \code{rdtSpecPf}, see \code{\link{PfMOI.Parameters}}
#' @md
rdtTest_PfSI <- function(tEvent, PAR){
  if(private$Pathogens$Pf$get_MOI()>0){
    runif(1) < private$PfMOI_PAR$rdtSensPf
  } else {
    runif(1) < private$PfMOI_PAR$rdtSpecPf
  }
}

#' PfMOI \code{Human} Method: Light Microscopy Test
#'
#' Administer light microscopy to this human.
#'  * if infected: true positive is detected with probability \code{lmSensPf}, see \code{\link{PfMOI.Parameters}}
#'  * if susceptible: false positive is detected with probability \code{lmSpecPf}, see \code{\link{PfMOI.Parameters}}
#' @md
lmTest_PfSI <- function(tEvent, PAR){
  if(private$Pathogens$Pf$get_MOI()>0){
    runif(1) < private$PfMOI_PAR$lmSensPf
  } else {
    runif(1) < private$PfMOI_PAR$lmSpecPf
  }
}
