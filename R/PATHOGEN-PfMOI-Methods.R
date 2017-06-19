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
# PfMOI_PAR Getters & Setters
#################################################################

#' PfMOI \code{Human} Method: Get PfMOI_PAR
#'
#' Get either entire list or one named element of PfMOI_PAR. See \code{\link{PfMOI.Parameters}} for definitions.
#' This method is bound to \code{Human$get_PfMOI_PAR_PAR()}
#'
Human_get_PfMOI_PAR <- function(ix = NULL){
  if(is.null(ix)){
    return(private$PfMOI_PAR)
  } else {
    return(private$PfMOI_PAR[[ix]])
  }
}

#' PfMOI \code{Human} Method: Set PfMOI_PAR
#'
#' Set either entire list of PfMOI_PAR. See \code{\link{PfMOI.Parameters}} for definitions.
#' This method is bound to \code{Human$set_PfMOI_PAR_PAR()}
#'
Human_set_PfMOI_PAR <- function(PfMOI_PAR){
  private$PfMOI_PAR = PfMOI_PAR
}


###################################################################
# Add PfMOI Utilities to 'HumanPop' Class
###################################################################

#' PfMOI \code{HumanPop} Method: Increment PfID
#'
#' Whenever a new liver-stage infection manifests in a human we increment the PfID counter and return the new PfID.
#' This method is bound to \code{HumanPop$increment_PfID()}
#'
PfMOI_increment_PfID <- function(){
  private$PfID = private$PfID + 1L
  return(private$PfID)
}

#' PfMOI \code{HumanPop} Method: Initialize MICRO PfMOI Infections
#'
#' Initialize PfMOI infections with MOI given by vector of MOI for population for MICRO or SimBitePfMOI
#' This method is bound to \code{HumanPop$init_MICRO_PfMOI()}
#'
#' @param PfMOI a vector of length equal to \code{HumanPop$nHumans} giving the MOI of each person. If \code{NULL} all people are set to MOI of 0.
init_MICRO_PfMOI <- function(PfMOI = NULL, tStart = 0){

  private$PfID = 1L
  if(is.null(private$Pathogens$Pf)){ # only add the PfSI object if NULL
    self$set_humanPfMOI()
  }

  if(is.null(PfMOI)){
    PfMOI = rep(0,times=self$nHumans)
  } else {
    if(length(PfMOI)!=length(self$nHumans)){
      stop("length of PfMOI must equal the number of humans in HumanPop object")
    }

    for(ixH in 1:length(PfMOI)){
      myMOI = PfMOI[ixH]
      if(myMOI == 0){
        next()
      }
      for(ixI in 1:myMOI){
        private$pop[[ixH]]$infectHumanPfMOI(tEvent = tStart, PAR = list(damID = -1L, sireID = -1L))
      }
    }

  }

}

#' PfMOI \code{HumanPop} Method: Set PfMOI Paramters for a \code{\link{HumanPop}}
#'
#' Set PfMOI_PAR for a HumanPop; this is useful for simulating multiple populations with different parameter values.
#' This function is bound to \code{HumanPop$set_PfMOI_PAR()}
#' @param PfMOI_PAR new parameter list, see \code{\link{PfMOI.Parameters}}
HumanPop_set_PfMOI_PAR <- function(PfMOI_PAR){
  for(ixH in 1:self$nHumans){
    private$pop[[ixH]]$set_PfMOI_PAR(PfMOI_PAR)
  }
}

# #' PfMOI \code{HumanPop} Method: Get PfMOI Histories
# #'
# #' Get all PfMOI histories, stored in class \code{\link{humanPfMOI}}
# #' This function is bound to \code{HumanPop$get_PfMOI_history()}
# HumanPop_get_PfMOI_history <- function(){
#   PfMOI_history = vector(mode="list",length=self$nHumans)
#   for(ixH in 1:self$nHumans){
#     PfMOI_history[[ixH]] = private$pop[[ixH]]$
#   }
# }

###################################################################
# Add PfMOI Pathogen Object to 'Human' & 'HumanPop' Class
###################################################################

#' PfMOI \code{Human} Method: Set Human-stage PfMOI Object
#'
#' Set the \code{\link{humanPfMOI}} object in a human.
#' This method is bound to \code{Human$set_humanPfMOI()}
#'
Human_set_humanPfMOI <- function(PfID, tInf = NULL, MOI = 0L, b = 0.55, c = 0.15, damID = NULL, sireID = NULL){
  private$Pathogens$Pf = humanPfMOI$new(PfID = PfID, tInf = tInf, MOI = MOI, b = b, c = c, damID = damID, sireID = sireID)
}

#' PfMOI \code{Human} Method: Get Human-stage PfMOI Object
#'
#' Get the \code{\link{humanPfMOI}} object in a human.
#' This method is bound to \code{Human$get_humanPfMOI()}
#'
Human_get_humanPfMOI <- function(){
  return(private$Pathogens$Pf)
}

#' PfMOI \code{HumanPop} Method: Set Human-stage PfMOI Object
#'
#' Set the \code{\link{humanPfMOI}} object in a human poulation.
#' This method is bound to \code{HumanPop$set_humanPfMOI()}
#'
#' @param b infected mosquito to human transmission efficiency
#' @param c infected human to mosquito transmission efficiency
HumanPop_set_humanPfMOI <- function(b = NULL, c = NULL){

  # sanity checks
  if(is.null(b)){
    private$pop[[1]]$get_PfMOI_PAR()$Pf_b
    b = rep(x = private$pop[[1]]$get_PfMOI_PAR("Pf_b"),times = self$nHumans)
  } else {
    if(length(b)!=self$nHumans){
      stop(paste0("length of b: ",length(b)," must be equal to size of human population: ",self$nHumans))
    }
  }
  if(is.null(c)){
    c = rep(x = private$pop[[1]]$get_PfMOI_PAR("Pf_c"),times = self$nHumans)
  } else {
    if(length(c)!=self$nHumans){
      stop(paste0("length of c: ",length(c)," must be equal to size of human population: ",self$nHumans))
    }
  }

  # set blank Pathogens$Pf slots for PfMOI
  for(ixH in 1:self$nHumans){
    private$pop[[ixH]]$set_humanPfMOI(PfID = NULL, b = b[ixH], c = c[ixH])
  }

}



#################################################################
# PfMOI Event Timing
#################################################################

#' PfMOI \code{Human} Method: Duration of Infection
#'
#' How many days does a single PfMOI infection last? See \code{\link{PfMOI.Parameters}} parameter \code{DurationPf} for baseline clearance rate.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttClearPf()}
#'
PfMOI_ttClearPf <- function(){
  rexp(n=1, rate=1/private$PfMOI_PAR$DurationPf)
}

#' PfMOI \code{Human} Method: Duration of Latency
#'
#' How many days after the infectious bite does a PfMOI infection start? See \code{\link{PfMOI.Parameters}} parameter \code{LatentPf} constant latent period.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttInfectPf()}
#'
PfMOI_ttInfectionPf <- function(){
  private$PfMOI_PAR$LatentPf
}

#' PfMOI \code{Human} Method: Timing of Fever Incident
#'
#' What is the timing of fever relative to the start of a PfMOI infection? See \code{\link{PfMOI.Parameters}} parameter \code{mnFeverPf} mean (on natural scale) and parameter \code{vrFeverPf} standard deviation (on natural scale) of log-normally distributed time to fever.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttFeverPf()}
#'
PfMOI_ttFeverPf <- function(){
  rlnorm(1,log(private$PfMOI_PAR$mnFeverPf),private$PfMOI_PAR$vrFeverPf)
}

#' PfMOI \code{Human} Method: Timing of Treatment
#'
#' What is the timing of treatment relative to the start of a fever incident? See \code{\link{PfMOI.Parameters}} parameter \code{mnTreatPf} for mean waiting time of exponentially distributed waiting period.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttTreatPf()}
#'
PfMOI_ttTreatPf <- function(){
  rexp(1, 1/private$PfMOI_PAR$mnTreatPf)
}

#' PfMOI \code{Human} Method: Duration of Protection from Chemoprophylaxis
#'
#' After administration of Chemoprophylaxis what is time to susceptibility? See \code{\link{PfMOI.Parameters}} parameter \code{mnChemoprophylaxisPf} constant timing period.
#' This method is called from \code{\link{event_endprophylaxisPfMOI}}
#' This method is bound to \code{Human$ttSusceptiblePf()}
#'
PfMOI_ttSusceptiblePf <- function(){
  return(private$PfMOI_PAR$mnChemoprophylaxisPf)
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

    for(m in 1:N){
      N = N - 1L
      PAR = mosquitoPfMOI$get_clone(m)
      self$infectiousBite_PfMOI(tBite, PAR)
      if(N==0){return(NULL)}
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
#' @param PAR single clonal variant returned from \code{mosquitoPfMOI$get_clone()}
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
#' @param PAR single clonal variant returned from \code{mosquitoPfMOI$get_clone()}
event_infectHumanPfMOI <- function(tEvent, PAR){
  list(tEvent = tEvent, PAR = PAR, tag = "infectHumanPfMOI")
}

#' PfMOI \code{Human} Event: PfMOI Infection Event
#'
#' Simulate a PfMOI infection. If the human is not under chemoprophylaxis, begin queuing events for this clonal variant's infection process.
#' This method is bound to \code{Human$infectHumanPfMOI()}
#'  * A Bernoulli event is drawn to determine if this infection produces fever; if so \code{\link{add2Q_feverPfMOI}} is called.
#'  * The end of this PfMOI infection is queued by \code{\link{add2Q_endPfMOI}}
#' @md
#' @param tEvent time of infection
#' @param PAR single clonal variant returned from \code{mosquitoPfMOI$get_clone()}
infectHumanPfMOI <- function(tEvent, PAR){

  if(!private$Pathogens$Pf$get_chemoprophylaxis()){

    # generate a new global PfID here from HumanPop
    PfID = self$get_HumansPointer()$increment_PfID()

    # add new infection to my infection queue
    private$Pathogens$Pf$add_Infection(PfID = PfID, damID = PAR$damID, sireID = PAR$sireID)

    if(runif(1) < private$PfMOI_PAR$FeverPf){
      self$add2Q_feverPfMOI(tEvent = tEvent)
    }

    self$add2Q_endPfMOI(tEvent = tEvent)
    private$Pathogens$Pf$track_history(eventT = tEvent, event = "I") # write me
  }



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
  ix = which(private$Pathogens$Pf$get_PfID() == PAR$PfID)
  # might need to check for length(ix) > 0, maybe not
  private$Pathogens$Pf$clear_Infection(ix = ix)
  private$Pathogens$Pf$track_history(eventT = tEvent, event = "C") # write me
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
  list(tEvent = tFever, PAR = PAR, tag = "feverPfMOI")
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

  private$Pathogens$Pf$track_history(eventT = tEvent, event = "F") # write me
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
#' Simulate a PfMOI treatment event. If the human is infected, set susceptible and track history; also initiate period of chemoprophylaxis, see \code{\link{add2Q_endprophylaxisPfMOI}}
#' This method is bound to \code{Human$treatPfMOI()}
#' @param tEvent time of treatment
#' @param PAR \code{NULL}
treatPfMOI <- function(tEvent, PAR){

  private$Pathogens$Pf$set_MOI(MOI = 0L)
  private$Pathogens$Pf$set_chemoprophylaxis(TRUE)
  self$rmTagFromQ(tag = "endPfMOI") # take out all clearance events

  private$Pathogens$Pf$track_history(eventT = tEvent, event = "S")
  private$Pathogens$Pf$track_history(eventT = tEvent, event = "P")

  # Initiate a period of protection from chemoprophylaxis
  self$add2Q_endprophylaxisPfMOI(tEvent)
}


###################################################################
# End of Chemoprophylaxis
###################################################################

#' PfMOI \code{Human} Event: Add PfMOI End of Chemoprophylaxis Event to Event Queue
#'
#' Add PfMOI end of chemoprophylaxis event to the event queue.
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
#' Generate PfMOI end of chemoprophylaxis event to place in event queue.
#' This method is called from \code{\link{add2Q_endprophylaxisPfMOI}}
#' This method is bound to \code{Human$event_endprophylaxisPfMOI()}
#'  * tag: \code{\link{endprophylaxisPfMOI}}
#'  * tEvent: treatment time is calculated as tSusceptible = tEvent + \code{\link{PfMOI_ttSusceptiblePf}}
#' @md
#' @param tEvent time to end chemoprophylaxis
#' @param PAR \code{NULL}
event_endprophylaxisPfMOI <- function(tEvent, PAR = NULL){
  tSusceptible = tEvent + self$ttSusceptiblePf()
  list(tEvent = tSusceptible, PAR = PAR, tag = "endprophylaxisPfMOI")
}

#' PfMOI \code{Human} Event: PfMOI End of Chemoprophylaxis Event
#'
#' End PfMOI chemoprophylaxis protection.
#' This method is bound to \code{Human$endprophylaxisPfMOI()}
#' @param tEvent time to end chemoprophylaxis
#' @param PAR \code{NULL}
endprophylaxisPfMOI <- function(tEvent, PAR){
  # End Prophylaxis
  private$Pathogens$Pf$set_chemoprophylaxis(FALSE)
  private$Pathogens$Pf$track_history(eventT = tEvent, event = "PP")

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
    self$track_history(eventT = tEvent, event = "PEvaxx")
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
  self$track_history(eventT = tEvent, event = "PEwane")
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
    self$track_history(eventT = tEvent, event = "GSvaxx")
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
  self$track_history(eventT = tEvent, event = "GSwane")
}
