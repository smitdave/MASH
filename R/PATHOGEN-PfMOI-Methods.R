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
# PfMOI Event Timing
#################################################################

#' PfMOI \code{Human} Method: Duration of Infection
#'
#' How many days does a single PfMOI infection last? See \code{\link{PfMOI.Parameters}} parameter \code{Pf_r} for baseline clearance rate.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttClearPf()}
#'
PfMOI_ttClearPf <- function(){
  rexp(n=1, rate=private$PfMOI_PAR$Pf_r)
}

#' PfMOI \code{Human} Method: Duration of Latency
#'
#' How many days after the infectious bite does a PfMOI infection start? See \code{\link{PfMOI.Parameters}} parameter \code{Pf_latent} constant latent period.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttInfectPf()}
#'
PfMOI_ttInfectionPf <- function(){
  private$PfMOI_PAR$Pf_latent
}

#' PfMOI \code{Human} Method: Timing of Fever Incident
#'
#' What is the timing of fever relative to the start of a PfMOI infection? See \code{\link{PfMOI.Parameters}} parameter \code{Pf_ttF} mean (on natural scale) and parameter \code{Pf_ttFvar} standard deviation (on natural scale) of log-normally distributed time to fever.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttFeverPf()}
#'
PfMOI_ttFeverPf <- function(){
  rlnorm(1,log(private$PfMOI_PAR$Pf_ttF),private$PfMOI_PAR$Pf_ttFvar)
}

#' PfMOI \code{Human} Method: Timing of Treatment
#'
#' What is the timing of treatment relative to the start of a fever incident? See \code{\link{PfMOI.Parameters}} parameter \code{Pf_ttT} for mean waiting time of exponentially distributed waiting period.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttTreatPf()}
#'
PfMOI_ttTreatPf <- function(){
  rexp(1, 1/private$PfMOI_PAR$Pf_ttT)
}

#' PfMOI \code{Human} Method: Duration of Protection from Chemoprophylaxis
#'
#' After administration of Chemoprophylaxis what is time to susceptibility? See \code{\link{PfMOI.Parameters}} parameter \code{mnChemoprophylaxisPf} constant timing period.
#' This method is called from \code{\link{event_endprophylaxisPfMOI}}
#' This method is bound to \code{Human$ttSusceptiblePf()}
#'
PfMOI_ttSusceptiblePf <- function(){
  return(private$PfMOI_PAR$Pf_ttS)
}

#' PfMOI \code{Human} Method: Duration of protection by PE Vaccination
#'
#' After administration of PE Vaccination what is duration of protection (reduced infected mosquito to human transmission efficiency)? See \code{\link{PfMOI.Parameters}} parameter \code{mnPEPf} and \code{vrPEPf} for normally distributed duration of protection.
#' This method is called from \code{\link{event_pewanePfMOI}}
#' This method is bound to \code{Human$ttPEWanePf()}
#'
PfMOI_ttPEWanePf <- function(){
  return(rnorm(n = 1, mean = private$PfMOI_PAR$mnPEPf, sd = private$PfMOI_PAR$vrPEPf))
}

#' PfMOI \code{Human} Method: Duration of protection by GS Vaccination
#'
#' Duration of protection by GS Vaccination? See \code{\link{PfMOI.Parameters}} parameter \code{mnGSPf} and \code{vrGSPf} for normally distributed duration of protection.
#' This method is called from \code{\link{event_gswanePfMOI}}
#' This method is bound to \code{Human$ttGSWanePf()}
#'
PfMOI_ttGSWanePf <- function(){
  return(rnorm(n = 1, mean = private$PfMOI_PAR$mnGSPf, sd = private$PfMOI_PAR$vrGSPf))
}

###################################################################
# PfMOI Diagnostics
###################################################################

#' PfMOI \code{Human} Method: Rapid Diagnostic Test
#'
#' Administer RDT to this human.
#'  * if infected: true positive is detected with probability \code{rdtSensPf}, see \code{\link{PfMOI.Parameters}}
#'  * if susceptible: false positive is detected with probability \code{rdtSpecPf}, see \code{\link{PfMOI.Parameters}}
#' @md
rdtTest_PfMOI <- function(tEvent, PAR){
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
lmTest_PfMOI <- function(tEvent, PAR){
  if(private$Pathogens$Pf$get_MOI()>0){
    runif(1) < private$PfMOI_PAR$lmSensPf
  } else {
    runif(1) < private$PfMOI_PAR$lmSpecPf
  }
}

###################################################################
# PfMOI: Mosquito to Human infectious bite
# Add methods to 'Human' Class
###################################################################


#' PfMOI \code{Human} Method: Host Probing
#'
#' This method is called by a mosquito when she probes a human host, but may also be called by \code{\link{SimBitePfMOI}} as a filler.
#' If the biting mosquito is infectious, the method calls \code{\link{infectiousBite_PfMOI}}, otherwise does nothing.
#' This method is bound to \code{Human$probeHost_PfMOI()}
#'
#' @param tBite time of bite
#' @param mosquitoPfMOI \code{\link{mosquitoPfMOI}} object passed from mosquito to human
probeHost_PfMOI <- function(tBite, mosquitoPfMOI){

  MOI = mosquitoPfMOI$get_MOI()

  if(MOI>0){
    N = private$PfMOI_PAR$MosyMaxI

    for(m in 1:mosquitoPfMOI$get_MOI()){
      N = N - 1L
      PAR = mosquitoPfMOI$get_clone(m)
      self$infectiousBite_PfMOI(tBite, PAR)
      if(N==0){break()}
    }

  }

}

#' PfMOI \code{Human} Method: Infectious Bite on Human
#'
#' This method is called from \code{\link{probeHost_PfMOI}}.
#' If the infectious bite results in successful transmission, this function queues a human infection event, see \code{\link{add2Q_infectHumanPfMOI}}
#' This method is bound to \code{Human$infectiousBite_PfMOI()}
#'
#' @param tBite time of bite
#' @param PAR single clonal variant returned from \code{mosquitoPfMOI$get_clone()}
infectiousBite_PfMOI <- function(tBite, PAR){
  if(runif(1) < private$Pathogens$Pf$get_b()){

    PAR = list(damID = PAR$damID, sireID = PAR$sireID)
    tInfStart = tBite + self$ttInfectionPf()
    self$add2Q_infectHumanPfMOI(tEvent = tInfStart, PAR = PAR)
  }
}


###################################################################
# PfMOI: Human to Mosquito infectious bite
# Add methods to 'MicroMosquitoFemale' Classe
###################################################################

# infectMosquito_PfMOI <- function(){
#
# }

# # From bloodstream infection to infect the mosquito
#
# infectMosquito_PfMOI = function(ixH, t, x, y){with(HUMANS[[ixH]]$Pathogens$Pf,{
#   if(MOI>0 & rbinom(1,1,HUMANS[[ixH]]$Pathogens$Pf$c)){
#       return(makePfM(ixH, t, x, y))
#   } else {
#     return(list(infected=FALSE))
#   }
# })}
#
# pathOBJ_PfMOI = function(b=Pf_b, c=Pf_c){
#   list(
#     MOI = 0,
#     pfid = 0,
#     chemoprophylaxis = FALSE,
#     b = b,
#     c = c,
#     eventT = -1,
#     events = "init",
#     MOIvsT = 0
#   )
# }


###################################################################
# Start a PfMOI Infection
###################################################################

#' PfMOI \code{Human} Event: Add PfMOI Infection Event to Event Queue
#'
#' Add a PfMOI infection to the event queue.
#' This method is called from \code{\link{infectiousBite_PfMOI}}
#' This method adds event \code{\link{event_infectHumanPfMOI}} to the event queue.
#' This method is bound to \code{Human$add2Q_infectHumanPfMOI()}
#'
#' @param tEvent time of infection
#' @param PAR write me!
add2Q_infectHumanPfMOI <- function(tEvent, PAR = NULL){
  self$addEvent2Q(event = self$event_infectHumanPfMOI(tEvent = tEvent, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI Infection Event
#'
#' Generate PfMOI infection event to place in event queue.
#' This method is called from \code{\link{add2Q_infectHumanPfMOI}}
#' This method is bound to \code{Human$event_infectHumanPfMOI()}
#'  * tag: \code{\link{infectHumanPfMOI}}
#' @md
#' @param tEvent time of infection
#' @param PAR write me!
event_infectHumanPfMOI <- function(){
  list(tEvent = tEvent, PAR = PAR, tag = "infectHumanPfMOI")
}

#' PfMOI \code{Human} Event: PfMOI Infection Event
#'
#' Simulate a PfMOI infection. If the human is not under chemoprophlaxis, begin queuing events for this clonal variant's infection process.
#' This method is bound to \code{Human$infectHumanPfMOI()}
#'  * A Bernoulli event is drawn to determine if this infection produces fever; if so \code{\link{add2Q_feverPfMOI}} is called.
#'  * The end of this PfMOI infection is queued by \code{\link{add2Q_endPfMOI}}
#' @md
#' @param tEvent time of infection
#' @param PAR write me!
infectHumanPfMOI <- function(tEvent, PAR){

  if(!private$Pathogens$get_chemoprophylaxis()){

    PfID = NULL # generate a new global PfID here from HumanPop

    private$Pathogens$increment_MOI() # increment MOI
    private$Pathogens$push_PfID(PfID)

    if(runif(1) < private$PfMOI_PAR$FeverPf){
      self$add2Q_feverPfMOI(tEvent = tEvent)
    }

    self$add2Q_endPfMOI(tEvent = tEvent)

  }

  private$Pathogens$track_history(tEvent = tEvent, event = "I") # write me

}


###################################################################
# End a PfMOI Infection
###################################################################

#' PfMOI \code{Human} Event: Add PfMOI Clearance Event to Event Queue
#'
#' Add PfMOI infection clearance event to the event queue.
#' This method is called from \code{\link{infectHumanPfMOI}}
#' This method adds event \code{\link{event_endPfMOI}} to the event queue.
#' This method is bound to \code{Human$add2Q_endPfMOI()}
#'
#' @param tEvent time of infection
#' @param PAR named list
#'  * PfID: PfID of the infection to end
#' @md
add2Q_endPfMOI <- function(tEvent, PAR = NULL){
  self$addEvent2Q(event = self$event_endPfMOI(tEvent = tEvent, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI Clearance Event
#'
#' Generate PfMOI clearance event to place in event queue.
#' This method is called from \code{\link{add2Q_endPfMOI}}
#' This method is bound to \code{Human$event_endPfMOI()}
#'  * tag: \code{\link{endPfMOI}}
#'  * tEvent: treatment time is calculated as tTreat = tEvent + \code{\link{PfMOI_ttClearPf}}
#' @md
#' @param tEvent time of clearance
#' @param PAR named list
#'  * PfID: PfID of the infection to end
#' @md
event_endPfMOI = function(tEvent, PAR = NULL){
  tEnd = tEvent + self$ttClearPf()
  list(tEvent = tEnd, PAR = PAR, tag = "endPfMOI")
}

#' PfMOI \code{Human} Event: PfMOI Infection Event
#'
#' Clear a PfMOI infection corresponding to the given PfID.
#' This method is bound to \code{Human$endPfMOI()}
#' @param tEvent time of clearance
#' @param PAR named list
#'  * PfID: PfID of the infection to end
#' @md
endPfMOI <- function(tEvent, PAR){
  ix = which(private$Pathogens$get_PfID() == PAR$PfID)
  # might need to check for length(ix) > 0, maybe not
  private$Pathogens$clear_Infection(ix = ix)
  private$Pathogens$track_history(tEvent = tEvent, event = "C") # write me
}


###################################################################
# Fever
###################################################################

#' PfMOI \code{Human} Event: Add PfMOI Fever Event to Event Queue
#'
#' Add PfMOI fever event to the event queue.
#' This method is called from \code{\link{infectHumanPfMOI}}
#' This method adds event \code{\link{event_feverPfMOI}} to the event queue.
#' This method is bound to \code{Human$add2Q_feverPfMOI()}
#'
#' @param tEvent time of fever
#' @param PAR \code{NULL}
#' @md
add2Q_feverPfMOI <- function(tEvent, PAR = NULL){
  self$addEvent2Q(event = self$event_feverPfMOI(tEvent = tEvent, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI Fever Event
#'
#' Generate PfMOI fever event to place in event queue.
#' This method is called from \code{\link{add2Q_feverPfMOI}}
#' This method is bound to \code{Human$event_feverPfMOI()}
#'  * tag: \code{\link{feverPfMOI}}
#'  * tEvent: treatment time is calculated as tTreat = tEvent + \code{\link{PfMOI_ttFeverPf}}
#' @md
#' @param tEvent time of fever
#' @param PAR \code{NULL}
#' @md
event_feverPfMOI <- function(tEvent, PAR = NULL){
  tFever = tEvent + self$ttFeverPf()
  list(tEvent = tEnd, PAR = PAR, tag = "feverPfMOI")
}

#' PfMOI \code{Human} Event: PfMOI Infection Event
#'
#' Clear a PfMOI infection corresponding to the given PfID.
#' This method is bound to \code{Human$endPfMOI()}
#' @param tEvent time of clearance
#' @param PAR \code{NULL}
#' @md
feverPfMOI <- function(tEvent, PAR){

  if(runif(1) < private$PfMOI_PAR$TreatPf){
    self$add2Q_treatPfMOI(tEvent = tEvent)
  }

  private$Pathogens$track_history(tEvent = tEvent, event = "F") # write me
}


###################################################################
# Treatment
###################################################################

#' PfMOI \code{Human} Event: Add PfMOI Treatment Event to Event Queue
#'
#' Add PfMOI treatment event to the event queue.
#' This method is called from \code{\link{feverPfMOI}}
#' This method adds event \code{\link{event_treatPfMOI}} to the event queue.
#' This method is bound to \code{Human$add2Q_treatPfMOI()}
#'
#' @param tEvent time of fever
#' @param PAR \code{NULL}
add2Q_treatPfMOI <- function(tEvent, PAR = NULL){
  self$addEvent2Q(event = self$event_treatPfMOI(tEvent = tEvent, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI Treatment Event
#'
#' Generate PfMOI treatment event to place in event queue.
#' This method is called from \code{\link{add2Q_treatPfMOI}}
#' This method is bound to \code{Human$event_treatPfMOI()}
#'  * tag: \code{\link{treatPfMOI}}
#'  * tEvent: treatment time is calculated as tTreat = tEvent + \code{\link{PfMOI_ttTreatPf}}
#' @md
#' @param tEvent time of treatment
#' @param PAR \code{NULL}
event_treatPfMOI <- function(tEvent, PAR = NULL){
  tTreat = tEvent + self$ttTreatPf()
  list(tEvent = tTreat, PAR = PAR, tag = "treatPfMOI")
}

#' PfMOI \code{Human} Event: PfMOI Treatment Event
#'
#' Simulate a PfMOI treatment event. If the human is infected, set susceptible and track history; also initiate period of chemoprophlaxis, see \code{\link{add2Q_endprophylaxisPfMOI}}
#' This method is bound to \code{Human$treatPfMOI()}
#' @param tEvent time of treatment
#' @param PAR \code{NULL}
treatPfMOI <- function(tEvent, PAR){

  private$Pathogens$set_MOI(MOI = 0L)
  private$Pathogens$set_chemoprophylaxis(TRUE)

  private$Pathogens$track_history(tEvent = tEvent, event = "S")
  private$Pathogens$track_history(tEvent = tEvent, event = "P")

  # Initiate a period of protection from chemoprophlaxis
  self$add2Q_endprophylaxisPfMOI(tEvent)
}


###################################################################
# End of Chemoprophylaxis
###################################################################

#' PfMOI \code{Human} Event: Add PfMOI End of Chemoprophylaxis Event to Event Queue
#'
#' Add PfMOI end of chemoprophlaxis event to the event queue.
#' This method is called from \code{\link{treatPfMOI}}
#' This method adds event \code{\link{event_endprophylaxisPfMOI}} to the event queue.
#' This method is bound to \code{Human$add2Q_endprophylaxisPfMOI()}
#'
#' @param tEvent time of event
#' @param PAR \code{NULL}
add2Q_endprophylaxisPfMOI <- function(tEvent, PAR = NULL){
  self$addEvent2Q(event = self$event_endprophylaxisPfMOI(tEvent = tEvent, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI End of Chemoprophylaxis Event
#'
#' Generate PfMOI end of chemoprophlaxis event to place in event queue.
#' This method is called from \code{\link{add2Q_endprophylaxisPfMOI}}
#' This method is bound to \code{Human$event_endprophylaxisPfMOI()}
#'  * tag: \code{\link{endprophylaxisPfMOI}}
#'  * tEvent: treatment time is calculated as tSusceptible = tEvent + \code{\link{PfMOI_ttSusceptiblePf}}
#' @md
#' @param tEvent time to end chemoprophlaxis
#' @param PAR \code{NULL}
event_endprophylaxisPfMOI <- function(tEvent, PAR = NULL){
  tSusceptible = tEvent + self$ttSusceptiblePf()
  list(tEvent = tSusceptible, PAR = PAR, tag = "endprophylaxisPfMOI")
}

#' PfMOI \code{Human} Event: PfMOI End of Chemoprophylaxis Event
#'
#' End PfMOI chemoprophlaxis protection.
#' This method is bound to \code{Human$endprophylaxisPfMOI()}
#' @param tEvent time to end chemoprophlaxis
#' @param PAR \code{NULL}
endprophylaxisPfMOI <- function(tEvent, PAR){
  # End Prophylaxis
  private$Pathogens$set_chemoprophylaxis(FALSE)
  private$Pathogens$track_history(tEvent = tEvent, event = "PP")

}


###################################################################
# HUMAN PE vaccination functions
###################################################################

#' PfMOI \code{Human} Event: Add PfMOI PE Vaccination Event to Event Queue
#'
#' Add PfMOI PE vaccination event to the event queue.
#' This method is called from \code{\link{queueVaccination_SimBitePfMOI}}
#' This method adds event \code{\link{event_pevaccinatePfMOI}} to the event queue.
#' This method is bound to \code{Human$add2Q_pevaccinatePfMOI()}
#'
#' @param tEvent time of vaccination
#' @param PAR \code{NULL}
add2Q_pevaccinatePfMOI <- function(tEvent, PAR = NULL){
  self$addEvent2Q(event = self$event_pevaccinatePfMOI(tEvent = tEvent, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI PE Vaccination Event
#'
#' Generate PfMOI PE vaccination event to place in event queue.
#' This method is called from \code{\link{add2Q_pevaccinatePfMOI}}
#' This method is bound to \code{Human$event_pevaccinatePfMOI()}
#'  * tag: \code{\link{pevaccinatePfMOI}}
#' @md
#' @param tEvent begin PE protection
#' @param PAR \code{NULL}
event_pevaccinatePfMOI <- function(tEvent, PAR = NULL){
  list(tEvent = tEvent, PAR = PAR, tag = "pevaccinatePfMOI")
}

#' PfMOI \code{Human} Event: PfMOI PE Vaccination Event
#'
#' Begin PfMOI PE vaccination protection.
#' This method is bound to \code{Human$endprophylaxisPfMOI()}
#'  * protection: infected mosquito to human transmission efficiency is modified by \code{peBlockPf}, see \code{\link{PfMOI.Parameters}}
#'  * waning efficacy: queue \code{\link{add2Q_pewanePfMOI}}
#' @md
#' @param tEvent begin PE protection
#' @param PAR \code{NULL}
pevaccinatePfMOI <- function(tEvent, PAR){
  if(runif(1) < private$PfMOI_PAR$PEProtectPf){
    private$Pathogens$Pf$set_b(private$PfMOI_PAR$Pf_b * (1-private$PfMOI_PAR$peBlockPf))
    self$add2Q_pewanePfMOI(tEvent = tEvent)
    self$track_History(tEvent = tEvent, event = "PEvaxx")
  }
}

#' PfMOI \code{Human} Event: Add PfMOI PE Waning Protection Event to Event Queue
#'
#' Add PfMOI PE waning protection event to the event queue.
#' This method is called from \code{\link{pevaccinatePfMOI}}
#' This method adds event \code{\link{event_pewanePfMOI}} to the event queue.
#' This method is bound to \code{Human$add2Q_pewanePfMOI()}
#'
#' @param tEvent time of vaccination
#' @param PAR \code{NULL}
add2Q_pewanePfMOI <- function(tEvent, PAR = NULL){
  self$addEvent2Q(event = self$event_pewanePfMOI(tEvent = tEvent, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI PE Waning Protection Event
#'
#' Generate PfMOI PE waning protection event to place in event queue.
#' This method is called from \code{\link{add2Q_pevaccinatePfMOI}}
#' This method is bound to \code{Human$event_pevaccinatePfMOI()}
#'  * tag: \code{\link{pevaccinatePfMOI}}
#'  * tEvent: loss of efficacy is calculated as tWane = tEvent + \code{\link{PfMOI_ttPEWanePf}}
#' @md
#' @param tEvent end PE protection
#' @param PAR \code{NULL}
event_pewanePfMOI <- function(tEvent, PAR = NULL){
  tWane = tEvent + self$ttPEWanePf()
  list(tEvent = tWane, PAR = PAR, tag = "pewanePfMOI")
}

#' PfMOI \code{Human} Event: PfMOI PE Waning Protection Event
#'
#' End PfMOI PE protection.
#' This method is bound to \code{Human$pewanePfMOI()}
#'  * protection: infected mosquito to human transmission efficiency is set back to \code{Pf_b}, see \code{\link{PfMOI.Parameters}}
#' @md
#' @param tEvent end PE protection
#' @param PAR \code{NULL}
pewanePfMOI <- function(tEvent, PAR){
  private$Pathogens$Pf$set_b(private$PfMOI_PAR$Pf_b)
  self$track_History(tEvent = tEvent, event = "PEwane")
}


###################################################################
# HUMAN GS vaccination functions
###################################################################

#' PfMOI \code{Human} Event: Add PfMOI GS Vaccination Event to Event Queue
#'
#' Add PfMOI GS vaccination event to the event queue.
#' This method is called from \code{\link{queueVaccination_SimBitePfMOI}}
#' This method adds event \code{\link{event_gsvaccinatePfMOI}} to the event queue.
#' This method is bound to \code{Human$add2Q_gsvaccinatePfMOI()}
#'
#' @param tEvent time of vaccination
#' @param PAR \code{NULL}
add2Q_gsvaccinatePfMOI <- function(tEvent, PAR = NULL){
  self$addEvent2Q(event = self$event_gsvaccinatePfMOI(tEvent = tEvent, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI GS Vaccination Event
#'
#' Generate PfMOI GS vaccination event to place in event queue.
#' This method is called from \code{\link{add2Q_gsvaccinatePfMOI}}
#' This method is bound to \code{Human$event_gsvaccinatePfMOI()}
#'  * tag: \code{\link{gsvaccinatePfMOI}}
#' @md
#' @param tEvent begin gs protection
#' @param PAR \code{NULL}
event_gsvaccinatePfMOI <- function(tEvent, PAR = NULL){
  list(tEvent = tEvent, PAR = PAR, tag = "gsvaccinatePfMOI")
}

#' PfMOI \code{Human} Event: PfMOI GS Vaccination Event
#'
#' Begin PfMOI GS vaccination protection.
#' This method is bound to \code{Human$endprophylaxisPfMOI()}
#'  * protection: infected human to mosquito transmission efficiency is modified by \code{gsBlockPf}, see \code{\link{PfMOI.Parameters}}
#'  * waning efficacy: queue \code{\link{add2Q_gswanePfMOI}}
#' @md
#' @param tEvent begin gs protection
#' @param PAR \code{NULL}
gsvaccinatePfMOI <- function(tEvent, PAR){
  if(runif(1) < private$PfMOI_PAR$GSProtectPf){
    private$Pathogens$Pf$set_c(private$PfMOI_PAR$Pf_c * (1-private$PfMOI_PAR$gsBlockPf))
    self$add2Q_gswanePfMOI(tEvent = tEvent)
    self$track_History(tEvent = tEvent, event = "GSvaxx")
  }
}

#' PfMOI \code{Human} Event: Add PfMOI GS Waning Protection Event to Event Queue
#'
#' Add PfMOI GS waning protection event to the event queue.
#' This method is called from \code{\link{gsvaccinatePfMOI}}
#' This method adds event \code{\link{event_gswanePfMOI}} to the event queue.
#' This method is bound to \code{Human$add2Q_gswanePfMOI()}
#'
#' @param tEvent time of vaccination
#' @param PAR \code{NULL}
add2Q_gswanePfMOI <- function(tEvent, PAR = NULL){
  self$addEvent2Q(event = self$event_gswanePfMOI(tEvent = tEvent, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI GS Waning Protection Event
#'
#' Generate PfMOI GS waning protection event to place in event queue.
#' This method is called from \code{\link{add2Q_gsvaccinatePfMOI}}
#' This method is bound to \code{Human$event_gsvaccinatePfMOI()}
#'  * tag: \code{\link{gsvaccinatePfMOI}}
#'  * tEvent: loss of efficacy is calculated as tWane = tEvent + \code{\link{PfMOI_ttGSWanePf}}
#' @md
#' @param tEvent end gs protection
#' @param PAR \code{NULL}
event_gswanePfMOI <- function(tEvent, PAR = NULL){
  tWane = tEvent + self$ttGSWanePf()
  list(tEvent = tWane, PAR = PAR, tag = "gswanePfMOI")
}

#' PfMOI \code{Human} Event: PfMOI GS Waning Protection Event
#'
#' End PfMOI GS protection.
#' This method is bound to \code{Human$gswanePfMOI()}
#'  * protection: infected human to mosquito transmission efficiency is set back to \code{Pf_c}, see \code{\link{PfMOI.Parameters}}
#' @md
#' @param tEvent end GS protection
#' @param PAR \code{NULL}
gswanePfMOI <- function(tEvent, PAR){
  private$Pathogens$Pf$set_c(private$PfMOI_PAR$Pf_c)
  self$track_History(tEvent = tEvent, event = "GSwane")
}
