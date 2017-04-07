########################################
#  PFSI
#  Sean Wu
#  April 5, 2017
########################################

###################################################################
# From infectious bite to infection
###################################################################

# probeHost_PfSI: probeHost called from probing(); defined in MBITES-HostEncounter.R
# infect a human
probeHost_PfSI <- function(tBite, ixH, ixS, ixM, Pf){
  if(NOISY == TRUE){print("probeHost")}
  if(any(Pf$spz)){ # sample a clonal variant if multiple
    PfClonalVar = which(Pf$spz)
    PfIx = sample(x = PfClonalVar, size = 1)
    infectiousBite_PfSI(tBite = tBite, ixH = ixH, ixS = ixS, ixM = ixM, PfM = Pf$PfM[[PfIx]])
  }
}

infectiousBite_PfSI <- function(tBite, ixH, ixS, ixM, PfM){
  if(NOISY == TRUE){print("infectiousBite")}
  if(rbinom(1,1,HUMANS[[ixH]]$Pathogens$Pf$b)){
    tInfStart = tBite + ttInfectionPf() # when does latent -> infected occur
    if(NOISY == TRUE){print("add2Pedigree")}
    addPf2Pedigree(tStart = tInfStart, tBite = tBite, ixH = ixH, ixS = ixS, ixM = ixM, PfM = PfM)
    add2Q_startPfSI(ixH, tInfStart, PfID)
  }
}

Pf0=list()
Pf0$spz = 1
PfM0 = list(parentID=0,ixM=0,tm=0,ixS=0)
Pf0$PfM[[1]] = PfM0

add2Q_simbitePfSI <- function(ixH, t, PAR=Pf0){
  addEvent2Q(ixH, event_simbitePfSI(t))
}

event_simbitePfSI <- function(t, PAR=Pf0){
  if(NOISY == TRUE) print("adding simbite")
  list(t=t, PAR=Pf0, F=simbite_PfSI, tag="simbite_PfSI")
}

simbite_PfSI <- function(ixH, t, ixS, ixM, PAR=Pf0){
  with(PAR,{
    probeHost_PfSI(ixH, t, ixS, ixM, Pf0)
  })
}

###################################################################
# Queue up infectious bites
###################################################################

add2Q_startPfSI <- function(ixH, t, pfid){
  addEvent2Q(ixH, event_startPfSI(t, pfid))
}

event_startPfSI <- function(t, pfid){
  #if(NOISY == TRUE) {print(c(t=t,"adding infection")); browser()}
  if(NOISY == TRUE) {print(c(t=t,"adding infection"))}
  list(t=t, PAR=list(pfid=pfid), F=infectHuman_PfSI, tag="infectHuman_PfSI")
}

infectHuman_PfSI <- function(ixH, t, PAR){
  if(NOISY == TRUE){print("infectHuman")}
  #Infect
  with(PAR,{
    if(HUMANS[[ixH]]$Pathogens$Pf$infected == FALSE & HUMANS[[ixH]]$Pathogens$Pf$chemoprophylaxis == FALSE){
        if(NOISY==TRUE) print("Infect")
        PfSIHistory(ixH, t, "I")
        HUMANS[[ixH]]$Pathogens$Pf$infected <<- TRUE
        HUMANS[[ixH]]$Pathogens$Pf$t0 <<- t
        HUMANS[[ixH]]$Pathogens$Pf$pfid <<- pfid
        if(rbinom(1,1,FeverPf)){
          add2Q_feverPfSI(ixH, t)
        }
        add2Q_endPfSI(ixH, t, pfid)
      }
  })

}


###################################################################
# End an infection
###################################################################

add2Q_endPfSI <- function(ixH, t, pfid){
  addEvent2Q(ixH, event_endPfSI(t, pfid))
}

event_endPfSI <- function(t,pfid){
  if(NOISY == TRUE) print("adding clear infection")
  tE = t+ttClearPf()
  list(t=tE, PAR=pfid, F = endPfSI, tag = "endPfSI")
}

endPfSI <- function(ixH, t, PAR=NULL){
  # Clear
  if(HUMANS[[ixH]]$Pathogens$Pf$infected == TRUE){
    if(NOISY==TRUE) print("Clear Infection")
    PfSIHistory(ixH, t, "S")
    HUMANS[[ixH]]$Pathogens$Pf$infected <<- FALSE
  }
}


###################################################################
# Fever
###################################################################

add2Q_feverPfSI <- function(ixH, t){
  addEvent2Q(ixH, event_feverPfSI(t))
}

event_feverPfSI <- function(t){
  if(NOISY == TRUE) print("adding fever")
  ttF = t + ttFeverPf()
  list(t=ttF, PAR=NULL, F=fever_PfSI, tag = "fever_PfSI")
}

fever_PfSI <- function(ixH, t, PAR=NULL){
  #Fever
  if(NOISY==TRUE) print("Fever")
  PfSIHistory(ixH, t, "F")
  if(rbinom(1,1,TreatPf)){
    add2Q_treatPfSI(ixH, t)
  }
}


###################################################################
# Treatment
###################################################################

add2Q_treatPfSI <- function(ixH, t){
  addEvent2Q(ixH, event_treatPfSI(t))
}

event_treatPfSI <- function(t){
  if(NOISY==TRUE) print("adding treatment")
  ttT = t+ttTreatPf()
  list(t=ttT, PAR=NULL, F=treat_PfSI, tag = "treat_PfSI")
}

treat_PfSI <- function(ixH, t, PAR){
  # Treat
  if(NOISY==TRUE) print("Treat")
  if(HUMANS[[ixH]]$Pathogens$Pf$infected == TRUE){
    HUMANS[[ixH]]$Pathogens$Pf$infected <<- FALSE
    PfSIHistory(ixH, t, "S")
  }

  HUMANS[[ixH]]$Pathogens$Pf$chemoprophylaxis <<- TRUE
  PfSIHistory(ixH, t, "P")
  # Initiate a period of protection from chemoprophlaxis
  add2Q_endprophylaxisPfSI(ixH, t)
}


###################################################################
# End of Chemoprophylaxis
###################################################################

add2Q_endprophylaxisPfSI <- function(ixH, t){
  addEvent2Q(ixH, event_endprophylaxisPfSI(t))
}

event_endprophylaxisPfSI <- function(t){
  if(NOISY==TRUE) print("adding end chemoprophylaxis")
  ttS = t + ttSusceptiblePf()
  list(t=ttS, PAR=NULL, F=endprophylaxis_PfSI, tag = "endprophylaxis_PfSI")
}

endprophylaxis_PfSI <- function(ixH, t, PAR){
  # End Prophylaxis
  if(NOISY==TRUE) print("End Prophylaxis")
  PfSIHistory(ixH, t, "S")
  HUMANS[[ixH]]$Pathogens$Pf$chemoprophylaxis <<- FALSE
}


###################################################################
# HUMAN PE vaccination functions
###################################################################

add2Q_pevaccinatePfSI <- function(ixH, t){
  addEvent2Q(ixH, event_pevaccinatePfSI(t))
}

event_pevaccinatePfSI <- function(t){
  list(t = t, PAR = NULL, F = pevaccinate_PfSI, tag = "pevaccinate_PfSI")
}

pevaccinate_PfSI <- function(ixH, t, PAR){
  if(rbinom(1,1,PEProtectPf)){
    HUMANS[[ixH]]$Pathogens$Pf$b <<- Pf_b * (1-peBlockPf)
    add2Q_pewanePfSI(ixH, t)
  }
}

add2Q_pewanePfSI <- function(ixH, t){
  ttw = t + ttPEWanePf()
  addEvent2Q(ixH, event_pewanePfSI(ttw))
}

event_pewanePfSI <- function(t){
  list(t=t, PAR=NULL, F=pewane_PfSI, tag="pewane_PfSI")
}

pewane_PfSI <- function(ixH, t, PAR){
  HUMANS[[ixH]]$Pathogens$Pf$b <<- Pf_b
}


###################################################################
# HUMAN GS vaccination functions
###################################################################

add2Q_gsvaccinatePfSI <- function(ixH, t){
  addEvent2Q(ixH, event_gsvaccinatePfSI(t))
}

event_gsvaccinatePfSI <- function(t){
  list(t=t, PAR=NULL, F=gsvaccinate_PfSI, tag = "gsvaccinate_PfSI")
}

gsvaccinate_PfSI <- function(ixH, t, PAR){
  if(rbinom(1,1,GSProtectPf)){
    HUMANS[[ixH]]$Pathogens$Pf$c <<- Pf_c*(1-gsBlockPf)
    add2Q_gswanePfSI(ixH, t)
  }
}

add2Q_gswanePfSI <- function(ixH, t){
  ttw = t + ttGSWane()
  addEvent2Q(ixH, event_gswanePfSI(ttw))
}

event_gswanePfSI <- function(t){
  list(t=t, PAR=NULL, F=gswane_PfSI, tag = "gswane_PfSI")
}

gswane_PfSI <- function(ixH, t, PAR){
 HUMANS[[ixH]]$Pathogens$Pf$c <<- Pf_c
}


###################################################################
# PfSI Diagnostics
###################################################################

rdtTest_PfSI <- function(ixH){
 ifelse(HUMANS[[ixH]]$Pathogens$Pf$infected==TRUE,rbinom(1,1,rdtSensPf), rbinom(1,1,rdtSpecPf))
}

lmTest_PfSI <- function(ixH){
 ifelse(HUMANS[[ixH]]$Pathogens$Pf$infected==TRUE,rbinom(1,1,lmSensPf), rbinom(1,1,rdtSpecPf))
}


###################################################################
# PfSI History
###################################################################

PfSIHistory <- function(ixH, t, event){
  if(NOISY == TRUE) ("PfSIHistory")
  if(KeepPfHistory == TRUE){
    if(NOISY == TRUE) ("Tracking History")
    HUMANS[[ixH]]$Pathogens$Pf$eventT <<- c(HUMANS[[ixH]]$Pathogens$Pf$eventT,t)
    HUMANS[[ixH]]$Pathogens$Pf$events <<- c(HUMANS[[ixH]]$Pathogens$Pf$events,event)
  }
}

###################################################################
# From bloodstream infection to infect the mosquito
###################################################################

infectMosquito_PfSI <- function(ixH, t, ixS){
  with(HUMANS[[ixH]]$Pathogens$Pf,{
    if(infected==TRUE & rbinom(1,1,HUMANS[[ixH]]$Pathogens$Pf$c)){
      return(makePfM(ixH, t, ixS))
    } else {
      return(list(infected=FALSE))
    }
  })
}

pathOBJ_PfSI <- function(b=Pf_b, c=Pf_c){
  list(
    infected = FALSE,
    chemoprophylaxis = 0,
    b = b,
    c = c,
    pfid = NULL,
    t0 = NULL,
    eventT = -1,
    events = "init"
  )
}
