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
#' This method is bound to \code{Human$ttClearPfMOI()}
#'
PfMOI_ttClearPf <- function(){
  rexp(n=1, rate=private$PfMOI_PAR$Pf_r)
}

#' PfMOI \code{Human} Method: Duration of Latency
#'
#' How many days after the infectious bite does a PfMOI infection start? See \code{\link{PfMOI.Parameters}} parameter \code{Pf_latent} constant latent period.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttInfectPfMOI()}
#'
PfMOI_ttInfectionPf <- function(){
  private$PfMOI_PAR$Pf_latent
}

#' PfMOI \code{Human} Method: Timing of Fever Incident
#'
#' What is the timing of fever relative to the start of a PfMOI infection? See \code{\link{PfMOI.Parameters}} parameter \code{Pf_ttF} mean (on natural scale) and parameter \code{Pf_ttFvar} standard deviation (on natural scale) of log-normally distributed time to fever.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttFeverPfMOI()}
#'
PfMOI_ttFeverPf <- function(){
  rlnorm(1,log(private$PfMOI_PAR$Pf_ttF),private$PfMOI_PAR$Pf_ttFvar)
}

#' PfMOI \code{Human} Method: Timing of Treatment
#'
#' What is the timing of treatment relative to the start of a fever incident? See \code{\link{PfMOI.Parameters}} parameter \code{Pf_ttT} for mean waiting time of exponentially distributed waiting period.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttTreatPfMOI()}
#'
PfMOI_ttTreatPf <- function(){
  rexp(1, 1/private$PfMOI_PAR$Pf_ttT)
}


#' PfMOI \code{Human} Method: Duration of Protection from Chemoprophylaxis
#'
#' After administration of treatment what is time to susceptibility? See \code{\link{PfMOI.Parameters}} parameter \code{Pf_ttS} constant timing period.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttSusceptiblePfMOI()}
#'
PfMOI_ttSusceptiblePf <- function(){private$PfMOI_PAR$Pf_ttS}


###################################################################
# PfMOI Diagnostics
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
      self$infectiousBite_PfSI(tBite, PAR)
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
# Start a PfSI Infection
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
add2Q_infectHumanPfMOI <- function(){

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

}

#' PfMOI \code{Human} Event: PfMOI Infection Event
#'
#' Simulate a PfMOI infection. If the human is not under chemoprophlaxis, begin queuing events for this clonal variant's infection process.
#' This method is bound to \code{Human$infectHumanPfMOI()}
#'  * A Bernoulli event is drawn to determine if this infection produces fever; if so \code{\link{add2Q_feverPfMOI}} is called.
#'  * The end of this PfSI infection is queued by \code{\link{add2Q_endPfMOI}}
#' @md
#' @param tEvent time of infection
#' @param PAR write me!
infectHumanPfMOI <- function(){

}

# infectHuman_PfMOI = function(ixH, t, pfid){
#
#   if(NOISY == TRUE){print("infectHuman")}
#
#   if(HUMANS[[ixH]]$Pathogens$Pf$chemoprophylaxis == FALSE){
#       PfMOIHistory(ixH, t, "I")
#       HUMANS[[ixH]]$Pf$MOI <<- HUMANS[[ixH]]$Pf$MOI + 1
#       HUMANS[[ixH]]$Pf$PfID <<- c(HUMANS[[ixH]]$Pf$PfID, pfid)
#       add2Q_endPfMOI(ixH, t, PfID)
#
#       if(rbinom(1,1,FeverPf)){
#         add2Q_feverPfMOI(ixH, t)
#       }
#       add2Q_endPfMOI(ixH, t, pfid)
#     }
# }
#
# add2Q_startPfMOI = function(ixH, t, pfid){
#   addEvent2Q(ixH, event_startPfMOI(t, pfid))
# }
#
# event_startPfMOI = function(t, pfid){
#   #if(NOISY == TRUE) {print(c(t=t,"adding infection")); browser()}
#   if(NOISY == TRUE) {print(c(t=t,"adding infection"))}
#   list(t=t, PAR=pfid, F=infectHuman_PfMOI, tag="infectHuman_PfMOI")
# }
#
# ###################################################################
# # End an infection
# ###################################################################
#
# add2Q_endPfMOI = function(ixH, t, pfid){
#   addEvent2Q(ixH, event_endPfMOI(t, pfid))
# }
#
# event_endPfMOI = function(t,pfid){
#   if(NOISY == TRUE) print("adding clear infection")
#   tE = t+ttClearPf()
#   list(t=tE, PAR=pfid, F = endPfMOI, tag = "endPfMOI")
# }
#
# endPfMOI <- function(ixH, t, pfid){
#   ix = which(HUMANS[[ixH]]$Pf$PfID == pfid)
#   if(length(ix)>0){
#     HUMANS[[ixH]]$Pf$PfID <<- HUMANS[[ixH]]$Pf$PfID[-ix]
#     HUMANS[[ixH]]$Pf$MOI <<- HUMANS[[ixH]]$Pf$MOI-1
#     PfMOIHistory(ixH, t, "E")
#     HUMANS[[ixH]]$Pf$infected <<- FALSE
#   }
# }
#
# endPfMOI = function(ixH, t, PAR=NULL){
#   # Clear
#   if(HUMANS[[ixH]]$Pathogens$Pf$infected == TRUE){
#     if(NOISY==TRUE) print("Clear Infection")
#     PfMOIHistory(ixH, t, "S")
#     HUMANS[[ixH]]$Pathogens$Pf$infected <<- FALSE
#   }
# }
#
#
# ###################################################################
# # Fever
# ###################################################################
#
# add2Q_feverPfMOI = function(ixH, t){
#   addEvent2Q(ixH, event_feverPfMOI(t))
# }
#
# event_feverPfMOI = function(t){
#   if(NOISY == TRUE) print("adding fever")
#   ttF = t + ttFeverPf()
#   list(t=ttF, PAR=NULL, F=fever_PfMOI, tag = "fever_PfMOI")
# }
#
# fever_PfMOI = function(ixH, t, PAR=NULL){
#   #Fever
#   if(NOISY==TRUE) print("Fever")
#   PfMOIHistory(ixH, t, "F")
#   if(rbinom(1,1,HUMANS[[i]]$TreatPf)){
#     add2Q_treatPfMOI(ixH, t)
#   }
# }
#
# ###################################################################
# # Treatment
# ###################################################################
#
# treat_PfMOI = function(ixH, t, PAR){
#   # Treat
#   if(NOISY==TRUE) print("Treat")
#   if(HUMANS[[ixH]]$Pathogens$Pf$infected == TRUE){
#     HUMANS[[ixH]]$Pathogens$Pf$infected <<- FALSE
#     PfMOIHistory(ixH, t, "S")
#   }
#
#   HUMANS[[ixH]]$Pathogens$Pf$chemoprophylaxis <<- TRUE
#   PfMOIHistory(ixH, t, "P")
#   # Initiate a period of protection from chemoprophlaxis
#   add2Q_endprophylaxisPfMOI(ixH, t)
# }
#
#
# add2Q_treatPfMOI = function(ixH, t){
#   addEvent2Q(ixH, event_treatPfMOI(t))
# }
#
# event_treatPfMOI = function(t){
#   if(NOISY==TRUE) print("adding treatment")
#   ttT = t+ttTreatPf()
#   list(t=ttT, PAR=NULL, F=treat_PfMOI, tag = "treat_PfMOI")
# }
#
# ###################################################################
# # End of Chemoprophylaxis
# ###################################################################
#
# endprophylaxis_PfMOI = function(ixH, t, PAR){
#   # End Prophylaxis
#   if(NOISY==TRUE) print("End Prophylaxis")
#   PfMOIHistory(ixH, t, "S")
#   HUMANS[[ixH]]$Pathogens$Pf$chemoprophylaxis <<- FALSE
# }
#
#
# add2Q_endprophylaxisPfMOI = function(ixH, t){
#   addEvent2Q(ixH, event_endprophylaxisPfMOI(t))
# }
#
# event_endprophylaxisPfMOI = function(t){
#   if(NOISY==TRUE) print("adding end chemoprophylaxis")
#   ttS = t + ttSusceptiblePf()
#   list(t=ttS, PAR=NULL, F=endprophylaxis_PfMOI, tag = "endprophylaxis_PfMOI")
# }
#
# ###################################################################
# # HUMAN PE vaccination functions
# ###################################################################
#
# add2Q_pevaccinatePfMOI = function(ixH, t){
#   addEvent2Q(ixH, event_pevaccinatePfMOI(t))
# }
#
# event_pevaccinatePfMOI = function(t){
#   list(t=t, PAR=NULL, F=pevaccinate_PfMOI, tag = "pevaccinate_PfMOI")
# }
#
#
# pevaccinate_PfMOI = function(ixH, t, PAR){
#   if(rbinom(1,1,PEProtectPf)){
#     HUMANS[[ixH]]$Pathogens$Pf$b <<- Pf_b * (1-peBlockPf)
#     add2Q_pewanePfMOI(ixH, t)
#   }
# }
#
# add2Q_pewanePfMOI = function(ixH, t){
#   ttw = t + ttPEWanePf()
#   addEvent2Q(ixH, event_pewanePfMOI(ttw))
# }
#
# event_pewanePfMOI = function(t){
#   list(t=t, PAR=NULL, F=pewane_PfMOI, tag="pewane_PfMOI")
# }
#
# pewane_PfMOI = function(ixH, t, PAR){
#   HUMANS[[ixH]]$Pathogens$Pf$b <<- Pf_b
# }
#
# ###################################################################
# # HUMAN GS vaccination functions
# ###################################################################
#
# add2Q_gsvaccinatePfMOI = function(ixH, t){
#   addEvent2Q(ixH, event_gsvaccinatePfMOI(t))
# }
#
# event_gsvaccinatePfMOI = function(t){
#   list(t=t, PAR=NULL, F=gsvaccinate_PfMOI, tag = "gsvaccinate_PfMOI")
# }
#
# add2Q_gswanePfMOI = function(ixH, t){
#   ttw = t + ttGSWane()
#   addEvent2Q(ixH, event_gswanePfMOI(ttw))
# }
#
# event_gswanePfMOI = function(t){
#   list(t=t, PAR=NULL, F=gswane_PfMOI, tag = "gswane_PfMOI")
# }
#
# gsvaccinate_PfMOI = function(ixH, t, PAR){
#   if(rbinom(1,1,GSProtectPf)){
#     HUMANS[[ixH]]$Pathogens$Pf$c <<- Pf_c*(1-gsBlockPf)
#     add2Q_gswanePfMOI(ixH, t)
#   }
# }
#
# gswane_PfMOI = function(ixH, t, PAR){
#  HUMANS[[ixH]]$Pathogens$Pf$c <<- Pf_c
# }
#
# ###################################################################
# # PfMOI Diagnostics
# ###################################################################
#
# rdtTest_PfMOI = function(ixH){
#  ifelse(HUMANS[[ixH]]$Pathogens$Pf$MOI>0,rbinom(1,1,rdtSensPf), rbinom(1,1,rdtSpecPf))
# }
#
# lmTest_PfMOI = function(ixH){
#  ifelse(HUMANS[[ixH]]$Pathogens$Pf$MOI>0,rbinom(1,1,lmSensPf), rbinom(1,1,rdtSpecPf))
# }
#
# ###################################################################
# # PfMOI History
# ###################################################################
#
# PfMOIHistory = function(ixH, t, event){
#   if(KeepPfHistory == TRUE){
#     if(NOISY == TRUE) ("Tracking History")
#     HUMANS[[ixH]]$Pathogens$Pf$eventT <<- c(HUMANS[[ixH]]$Pathogens$Pf$eventT,t)
#     HUMANS[[ixH]]$Pathogens$Pf$events <<- c(HUMANS[[ixH]]$Pathogens$Pf$events,event)
#     HUMANS[[ixH]]$Pathogens$Pf$MOIvsT <<- c(HUMANS[[ixH]]$Pathogens$Pf$MOIvsT,HUMANS[[ixH]]$Pathogens$Pf$MOI)
#   }
# }
#
