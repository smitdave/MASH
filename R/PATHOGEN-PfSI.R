########################################
#  PFSI Setup
#  Sean Wu
#  April 5, 2017
########################################

#' Initialize PfSI Module Parameters (Pathogen)
#'
#' Generate a list of parameters and define functions in the global environment for PfSI. All arguments have default values which are listed below before the definition.
#'
#' @param Pf_c 0.15; transmission efficiency: infected human to mosquito
#' @param Pf_b 0.55; transmission efficiency: infected mosquito to human
#' @param DurationPf 200; duration of infection (How many days does the infection last?)
#' @param LatentPf 10; latency (How many days after the infectious bite does the infection start?)
#' @param FeverPf 0.3; probability of fever
#' @param mnFeverPf 10; mean of Gaussian-distributed timing of fever incident (relative to start of the infection)
#' @param vrFeverPf 0.1; standard deviation of Gaussian-distributed timing of fever incident (relative to start of the infection)
#' @param TreatPf 0.5; probability of treatment
#' @param mnTreatPf 1; timing of treatment (relative to start of the fever)
#' @param mnChemoprophylaxisPf 32; Prophylaxis, time to susceptibility (duration of protection due to chemoprophylaxis)
#' @param PEProtectPf 0.99; proportion protected by PE vaccination (probability vaccination successful)
#' @param peBlockPf 1; proportion of infections blocked by PE vaccination
#' @param mnPEPf 270; mean duration of protection from PE vaccination
#' @param vrPEPf 50; standard deviation of protection from PE vaccination
#' @param GSProtectPf 1; proportion protected by GS vaccination (probability vaccination successful)
#' @param gsBlockPf 0.9; proportion of infections blocked by GS vaccination
#' @param mnGSPf 180; mean duration of protection from GS vaccination
#' @param vrGSPf 20; standard deviation of protection from GS vaccination
#' @param rdtSensPf 0.9; RDT sensitivity
#' @param rdtSpecPf 0.1; RDT specificity
#' @param lmSensPf 0.9; Light Microscopy sensitivity
#' @param lmSpecPf 0.1; Light Microscopy specificity
#' @return define functions and parameters in global environment
#' @examples
#' PFSI.SETUP()
#' @export
PFSI.SETUP <- function(

  ########################################
  #  Parameters
  ########################################

  # Transmission efficiency
  Pf_c   = 0.15,
  Pf_b   = 0.55,

  # Duration of Infection
  # (How many days does the infection last?)
  DurationPf = 200,

  # Latency:
  # (How many days after the infectious
  #  bite does the infection start?)
  LatentPf = 10,

  # Probability of Fever
  FeverPf = 0.3,

  # Timing of Fever Incident
  # (relative to start of the infection)
  mnFeverPf = 10,
  vrFeverPf = .1,

  # Probability of Treating
  TreatPf = .5,

  # Timing of Treatment
  # (relative to start of the fever)
  mnTreatPf = 1,

  # Prophylaxis, time to susceptibility
  mnChemoprophylaxisPf = 32,

  # Proportion Protected by PE Vaccination
  PEProtectPf = .99,

  # Proportion of infections Blocked
  peBlockPf = 1,
  mnPEPf = 270,
  vrPEPf = 50,

  # Proportion Protected by GS Vaccination
  GSProtectPf = 1,

  # Proportion of infections Blocked
  gsBlockPf = .9,
  mnGSPf = 180,
  vrGSPf = 20,

  #  RDT Probe and Accuracy
  rdtSensPf  = .9,
  rdtSpecPf  = .1,

  #  Light Microscopy Probe and Sensitivity
  lmSensPf  = .9,
  lmSpecPf  = .1

){

  # Define functions
  probeHost <<- probeHost_PfSI
  infectMosquito <<- infectMosquito_PfSI

  # Transmission efficiency
  Pf_c   <<- Pf_c
  Pf_b   <<- Pf_b

  # Duration of Infection
  # (How many days does the infection last?)
  DurationPf <<- DurationPf
  ttClearPf <<- function(){rexp(1, 1/DurationPf)}

  # Latency:
  # (How many days after the infectious
  #  bite does the infection start?)
  LatentPf <<- LatentPf
  ttInfectionPf <<- function(){LatentPf}

  # Probability of Fever
  FeverPf <<- FeverPf

  # Timing of Fever Incident
  # (relative to start of the infection)
  mnFeverPf <<- mnFeverPf
  vrFeverPf <<- vrFeverPf
  ttFeverPf <<- function(){mnFeverPf}

  # Probability of Treating
  TreatPf <<- TreatPf

  # Timing of Treatment
  # (relative to start of the fever)
  mnTreatPf <<- mnTreatPf
  ttTreatPf <<- function(){mnTreatPf}

  # Prophylaxis, time to susceptibility
  mnChemoprophylaxisPf <<- mnChemoprophylaxisPf
  ttSusceptiblePf <<- function(){mnChemoprophylaxisPf}

  # Proportion Protected by PE Vaccination
  PEProtectPf <<- PEProtectPf

  # Proportion of infections Blocked
  peBlockPf <<- peBlockPf
  mnPEPf <<- mnPEPf
  vrPEPf <<- vrPEPf
  ttPEWanePf <<- function(){
    rnorm(1, mnPEPf, vrPEPf)
  }

  # Proportion Protected by GS Vaccination
  GSProtectPf <<- GSProtectPf

  # Proportion of infections Blocked
  gsBlockPf <<- gsBlockPf
  mnGSPf <<- mnGSPf
  vrGSPf <<- vrGSPf
  ttGSWanePf <<- function(){
    rnorm(1, PfmeanGSProtect, PfvarGSprotect)
  }

  #  RDT Probe and Accuracy
  rdtSensPf <<- rdtSensPf
  rdtSpecPf <<- rdtSpecPf
  rdtTest <<- rdtTestPfSI

  #  Light Microscopy Probe and Sensitivity
  lmSensPf <<- lmSensPf
  lmSpecPf <<- lmSpecPf
  lmTest <<- lmTestPfSI

}


###################################################################
# From infectious bite to infection
###################################################################

# probeHost_PfSI: probeHost called from probing(); defined in MBITES-HostEncounter.R
# infect a human
probeHost_PfSI <- function(tBite, ixS, ixM, Pf, private, self){
  if(any(Pf$spz)){ # sample a clonal variant if multiple
    PfClonalVar = which(Pf$spz)
    PfIx = sample(x = PfClonalVar, size = 1)
    infectiousBite_PfSI(tBite = tBite, ixS = ixS, ixM = ixM, PfM = Pf$PfM[[PfIx]], private = private, self = self)
  }
}

# infectiousBite_PfSI
infectiousBite_PfSI <- function(tBite, ixS, ixM, PfM, private, self){
  if(runif(1) < private$Pathogens$Pf$b){
    tInfStart = tBite + ttInfectionPf() # progression from latent to infectious
    self$add2Q_startPfSI(tEvent = tInfStart, PAR = PfM)
  }
}


###################################################################
# Simulated Biting PfSI
###################################################################

# event_simbitePfSI: make a event package to insert into the eventQ
event_simbitePfSI <- function(tEvent, PAR = NULL){
  list(tEvent = tEvent, PAR = PAR, FUN = simbite_PfSI)
}

# simbite_PfSI: fire the event
simbite_PfSI <- function(tEvent, PAR, private, self){
  if(getNOISY){
    print(paste0("human ",private$myID," being simBitten at: ",tEvent))
  }
  probeHost_PfSI(tBite = tEvent, ixS = NULL, ixM = NULL, Pf = PAR, private = private, self = self)
}

###################################################################
# Start a PfSI Infection
###################################################################

# event_startPfSI: begin a PfSI infection
event_startPfSI <- function(tEvent, PAR = NULL){
  list(tEvent = tEvent, PAR = PAR, FUN = infectHuman_PfSI)
}

# infectHuman_PfSI
infectHuman_PfSI <- function(tEvent, PAR, private, self){
  if(!private$Pathogens$Pf$infected & !private$Pathogens$Pf$chemoprophylaxis){
    self$trackHist(tEvent = tEvent, event = "I")
    private$Pathogens$Pf$infected = TRUE
    private$Pathogens$Pf$t0 = tEvent
    private$Pathogens$Pf$pfid = PAR$pfid
    if(runif(1) < FeverPf){
      self$add2Q_feverPfSI(tEvent = tEvent)
    }
    self$add2Q_endPfSI(tEvent = tEvent)
  }
}


###################################################################
# End an infection
###################################################################

# event_endPfSI
event_endPfSI <- function(tEvent, PAR = NULL){
  tEnd = tEvent + ttClearPf()
  list(tEvent = tEnd, PAR = PAR, FUN = endPfSI)
}

# endPfSI
endPfSI <- function(tEvent, PAR = NULL, private, self){
  if(private$Pathogens$Pf$infected){
    self$trackHist(tEvent = tEvent, event = "S")
    private$Pathogens$Pf$infected = FALSE
  }
}


###################################################################
# Fever
###################################################################

# event_feverPfSI
event_feverPfSI <- function(tEvent, PAR = NULL){
  tFever = tEvent + ttFeverPf()
  list(tEvent = tFever, PAR = PAR, FUN = feverPfSI)
}

# feverPfSI
feverPfSI <- function(tEvent, PAR = NULL, private, self){
  self$trackHist(tEvent = tEvent, event = "F")
  if(runif(1) < TreatPf){
    self$add2Q_treatPfSI(tEvent = tEvent)
  }
}


###################################################################
# Treatment (Chemoprophylaxis Begin)
###################################################################

# event_treatPfSI
event_treatPfSI <- function(tEvent, PAR = NULL){
  tTreat = tEvent + ttTreatPf()
  list(tEvent = tTreat, PAR = PAR, FUN = treatPfSI)
}

# treatPfSI
treatPfSI <- function(tEvent, PAR = NULL, private, self){
  if(private$Pathogens$Pf$infected){
    private$Pathogens$Pf$infected = FALSE
    self$trackHist(tEvent = tEvent, event = "S")
  }
  private$Pathogens$Pf$chemoprophylaxis = TRUE
  self$trackHist(tEvent = tEvent, event = "P")
  self$add2Q_endprophylaxisPfSI(tEvent = tEvent)
}


###################################################################
# Chemoprophylaxis End
###################################################################

event_endprophylaxisPfSI <- function(tEvent, PAR = NULL){
  tEnd = tEvent + ttSusceptiblePf()
  list(tEvent = tEnd, PAR = PAR, FUN = endprophylaxisPfSI)
}

endprophylaxisPfSI <- function(tEvent, PAR = NULL, private, self){
  self$trackHist(tEvent = tEvent, event = "S")
  private$Pathogens$Pf$chemoprophylaxis = FALSE
}


###################################################################
# PE vaccination
###################################################################

event_pevaccinatePfSI <- function(tEvent, PAR = NULL){
  list(tEvent = tEvent, PAR = PAR, FUN = pevaccinatePfSI)
}

pevaccinatePfSI <- function(tEvent, PAR = NULL, private, self){
  if(runif(1) < PEProtectPf){
    self$trackHist(tEvent = tEvent, event = "PEvaxx")
    private$Pathogens$Pf$b = Pf_b * (1-peBlockPf)
    self$add2Q_pewanePfSI(tEvent = tEvent)
  }
}

event_pewanePfSI <- function(tEvent, PAR = NULL){
  tWane = tEvent + ttPEWanePf()
  list(tEvent = tWane, PAR = PAR, FUN = pewanePfSI)
}

pewanePfSI <- function(tEvent, PAR = NULL, private, self){
  self$trackHist(tEvent = tEvent, event = "PEwane")
  private$Pathogens$Pf$b = Pf_b
}


###################################################################
# HUMAN GS vaccination functions
###################################################################

event_gsvaccinatePfSI <- function(tEvent, PAR = NULL){
  list(tEvent = tEvent, PAR = PAR, FUN = gsvaccinatePfSI)
}

gsvaccinatePfSI <- function(tEvent, PAR = NULL, private, self){
  if(runif(1) < GSProtectPf){
    self$trackHist(tEvent = tEvent, event = "GSvaxx")
    private$Pathogens$Pf$c = Pf_c*(1-gsBlockPf)
    self$add2Q_gswanePfSI(tEvent = tEvent)
  }
}

event_gswanePfSI <- function(tEvent, PAR = NULL){
  tWane = tEvent + ttGSWane()
  list(tEvent = tWane, PAR = PAR, FUN = gswanePfSI)
}

gswanePfSI <- function(tEvent, PAR = NULL, private, self){
  self$trackHist(tEvent = tEvent, event = "GSwane")
  private$Pathogens$Pf$c = Pf_c
}


###################################################################
# Diagnostics & Testing
###################################################################

rdtTestPfSI <- function(tEvent, PAR = NULL, private, self){
  if(private$Pathogens$Pf$infected){
    rbinom(1,1,rdtSensPf)
  } else {
    rbinom(1,1,rdtSpecPf)
  }
}

lmTestPfSI <- function(tEvent, PAR = NULL, private, self){
  if(private$Pathogens$Pf$infected){
    rbinom(1,1,lmSensPf)
  } else {
    rbinom(1,1,lmSpecPf)
  }
}


###################################################################
# From bloodstream infection to infect the mosquito
###################################################################

# infectMosquito_PfSI <- function(tBite, ixH, ixS, ixM){
#   with(HUMANS[[ixH]]$Pathogens$Pf,{
#     if(infected==TRUE & rbinom(1,1,HUMANS[[ixH]]$Pathogens$Pf$c)){
#       infObj = makePfM(ixH, tBite, ixS)
#       return(infObj)
#     } else {
#       infObj = list(infected = FALSE)
#       return(infObj)
#     }
#   })
# }


infectMosquito_PfSI <- function(){
  print("sean needs to write this!")
}

##########################################
# Pathogens: PfSI Functions and Data
##########################################

# PfSI Pathogen Object
pathOBJ_PfSI <- function(b = 1, c = 1){
  list(
    infected = FALSE,
    chemoprophylaxis = FALSE,
    b = b,
    c = c,
    pfid = NULL,
    t0 = NULL
  )
}

# NULL Pf object passed from 'mosquito'
makePf0 <- function(ixH, tBite, ixS = NULL){
  PfM = list()
  PfM[[1]] = list(tm=tBite, ixS=ixS, ixH=ixH, damID=NULL, sireID=NULL, pfid=NULL)
  list(
    spz = TRUE,
    PfM = PfM
  )
}



###################################################################
# Add Methods to R6 'Human' and 'HumanPop' Class
###################################################################

#' Initialize PfSI-SimBite Module (Pathogen)
#'
#' Initialize methods in \code{\link{Human}} and \code{\link{HumanPop}} classes for simulated biting.
#' This should be run after calling \code{\link{init.PfSI}} if humans are being simulated as a stand-alone component.
#'
#' @param write me
#' @return write me
#' @examples
#' init.simbitePfSI()
#' @export
init.simbitePfSI <- function(){

  # add2Q_simbitePfSI
  Human$set(which = "public",name = "add2Q_simbitePfSI",
            value = function(tEvent, PAR = NULL){
              Pf0 = makePf0(ixH = private$myID,tBite = tEvent)
              self$addEvent2Q(event = event_simbitePfSI(tEvent = tEvent, PAR = Pf0))
            }
  )

  # queueInfections
  HumanPop$set(which = "public",name = "queueInfections_simbitePfSI",
               value = function(tMax, tBite = 1/20){
                 for(i in 1:self$nHum){
                   t = 0
                   if(self$verbose){
                     print(paste0("queueing bites for human: ",i))
                   }
                   while(t < tMax){
                     t = t + rexp(n = 1,rate = tBite)
                     private$pop[[i]]$add2Q_simbitePfSI(tEvent = t)
                   }
                   private$pop[[i]]$trackHist(tEvent = self$tStart,event = "S")
                 }
               }
  )

  # queueVaxx
  HumanPop$set(which = "public",name = "queueVaxx_simbitePfSI",
               value = function(tVaxx, tTreat, fracPop){
                 for(i in 1:floor(fracPop*self$nHum)){
                   if(self$verbose){
                     print(paste0("queueing vaccination for human: ",i))
                   }
                   private$pop[[i]]$add2Q_pevaccinatePfSI(tEvent = tVaxx)
                   private$pop[[i]]$add2Q_treatPfSI(tEvent = tTreat)
                 }
               }
  )

}

#' Initialize PfSI Module
#'
#' Initialize methods in \code{\link{Human}} and \code{\link{HumanPop}} classes for PfSI Pathogen module.
#'
#' @param write me
#' @return write me
#' @examples
#' init.PfSI()
#' @export
init.PfSI <- function(){

  # setPfSI
  HumanPop$set(which = "public",name = "init_PfSI",
               value = function(){

                 print(paste0("initializing PfSI PATHOGEN module"))

                 for(i in 1:self$nHum){
                   private$pop[[i]]$setPathogensObject(pathogen = "PfSI")
                 }
               })

  # add2Q_startPfSI
  Human$set(which = "public",name = "add2Q_startPfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = event_startPfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  # add2Q_endPfSI
  Human$set(which = "public",name = "add2Q_endPfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = event_endPfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  # add2Q_feverPfSI
  Human$set(which = "public",name = "add2Q_feverPfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = event_feverPfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  # add2Q_treatPfSI
  Human$set(which = "public",name = "add2Q_treatPfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = event_treatPfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  # add2Q_endprophylaxisPfSI
  Human$set(which = "public",name = "add2Q_endprophylaxisPfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = event_endprophylaxisPfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  # add2Q_pevaccinatePfSI
  Human$set(which = "public",name = "add2Q_pevaccinatePfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = event_pevaccinatePfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  # add2Q_pewanePfSI
  Human$set(which = "public",name = "add2Q_pewanePfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = event_pewanePfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  # add2Q_gsvaccinatePfSI
  Human$set(which = "public",name = "add2Q_gsvaccinatePfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = event_gsvaccinatePfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  # add2Q_gswanePfSI
  Human$set(which = "public",name = "add2Q_gswanePfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = event_gswanePfSI(tEvent = tEvent, PAR = PAR))
            }
  )

}

# #' Initialize PfSI Module (Pathogen)
# #'
# #' Initialize methods in \code{\link{Human}} and \code{\link{HumanPop}} classes for PfSI Pathogen module.
# #'
# #' @param write me
# #' @return write me
# #' @examples
# #' init.PfSI()
# #' @export
# init.PfSI <- function(){
#
#   # setPfSI
#   HumanPop$set(which = "public",name = "init_PfSI",
#                value = function(){
#
#                  print(paste0("initializing PfSI PATHOGEN module"))
#
#                  for(i in 1:self$nHum){
#                    private$pop[[i]]$setPathogensObject(pathogen = "PfSI")
#                  }
#                })
#
#   # add2Q_startPfSI
#   Human$set(which = "public",name = "add2Q_startPfSI",
#             value = function(tEvent, PAR = NULL){
#               self$addEvent2Q(event = event_startPfSI(tEvent = tEvent, PAR = PAR))
#             }
#   )
#
#   # add2Q_endPfSI
#   Human$set(which = "public",name = "add2Q_endPfSI",
#             value = function(tEvent, PAR = NULL){
#               self$addEvent2Q(event = event_endPfSI(tEvent = tEvent, PAR = PAR))
#             }
#   )
#
#   # add2Q_feverPfSI
#   Human$set(which = "public",name = "add2Q_feverPfSI",
#             value = function(tEvent, PAR = NULL){
#               self$addEvent2Q(event = event_feverPfSI(tEvent = tEvent, PAR = PAR))
#             }
#   )
#
#   # add2Q_treatPfSI
#   Human$set(which = "public",name = "add2Q_treatPfSI",
#             value = function(tEvent, PAR = NULL){
#               self$addEvent2Q(event = event_treatPfSI(tEvent = tEvent, PAR = PAR))
#             }
#   )
#
#   # add2Q_endprophylaxisPfSI
#   Human$set(which = "public",name = "add2Q_endprophylaxisPfSI",
#             value = function(tEvent, PAR = NULL){
#               self$addEvent2Q(event = event_endprophylaxisPfSI(tEvent = tEvent, PAR = PAR))
#             }
#   )
#
#   # add2Q_pevaccinatePfSI
#   Human$set(which = "public",name = "add2Q_pevaccinatePfSI",
#             value = function(tEvent, PAR = NULL){
#               self$addEvent2Q(event = event_pevaccinatePfSI(tEvent = tEvent, PAR = PAR))
#             }
#   )
#
#   # add2Q_pewanePfSI
#   Human$set(which = "public",name = "add2Q_pewanePfSI",
#             value = function(tEvent, PAR = NULL){
#               self$addEvent2Q(event = event_pewanePfSI(tEvent = tEvent, PAR = PAR))
#             }
#   )
#
#   # add2Q_gsvaccinatePfSI
#   Human$set(which = "public",name = "add2Q_gsvaccinatePfSI",
#             value = function(tEvent, PAR = NULL){
#               self$addEvent2Q(event = event_gsvaccinatePfSI(tEvent = tEvent, PAR = PAR))
#             }
#   )
#
#   # add2Q_gswanePfSI
#   Human$set(which = "public",name = "add2Q_gswanePfSI",
#             value = function(tEvent, PAR = NULL){
#               self$addEvent2Q(event = event_gswanePfSI(tEvent = tEvent, PAR = PAR))
#             }
#   )
#
# }


##########################################
# Plotting
##########################################

plotPfsiOneTrajectory <- function(ixH, oneHistory, tMax){

  times = oneHistory$eventT[-1]
  events = oneHistory$events[-1]

  # fever
  ixF = which(events == "F")
  if(length(ixF)>0){
    points(x = times[ixF],y = rep(x = ixH,times = length(ixF)),pch=17,cex=0.5,col="red")
  }

  # prophylaxis
  ixP = which(events == "P")
  if(length(ixP)>0){
    points(x = times[ixP],y = rep(x = ixH,times = length(ixP)),pch=16,cex=0.5,col="blue")
  }

  # vaccination
  ixV = grep(pattern = "vaxx$",x = events)
  ixW = grep(pattern = "wane$",x = events)
  if(length(ixV)>0){
    points(x = times[ixV],y = rep(x = ixH,times = length(ixV)),pch=18,cex=0.5,col="darkorange")
  }
  if(length(ixW)>0){
    points(x = times[ixW],y = rep(x = ixH,times = length(ixW)),pch=18,cex=0.5,col="darkorange4")
  }

  events = events[-c(ixF,ixV,ixW)]
  times = times[-c(ixF,ixV,ixW)]

  # state trajectory
  if(length(events) > 0){

    for(i in 1:(length(events))){

      if(i == length(events)){ # final trajectory
        if(events[i] == "I"){
          segments(x0 = times[i],y0 = ixH,x1 = tMax,y1 = ixH,col = "red",lwd = 2)
        }
        if(events[i] == "S"){
          segments(x0 = times[i],y0 = ixH,x1 = tMax,y1 = ixH,col = "grey70",lwd = 2)
        }
        if(events[i] == "P"){
          segments(x0 = times[i],y0 = ixH,x1 = tMax,y1 = ixH,col = "blue",lwd = 2)
        }

      } else { # interior trajectory
        if(events[i] == "I"){
          segments(x0 = times[i],y0 = ixH,x1 = times[i+1],y1 = ixH,col = "red",lwd = 2)
        }
        if(events[i] == "S"){
          segments(x0 = times[i],y0 = ixH,x1 = times[i+1],y1 = ixH,col = "grey70",lwd = 2)
        }
        if(events[i] == "P"){
          segments(x0 = times[i],y0 = ixH,x1 = times[i+1],y1 = ixH,col = "blue",lwd = 2)
        }
      }

    } # end for
  } # end state trajectory

}

plotPfsiTrajectory <- function(history){

  tMax = max(sapply(X = history,FUN = function(x){max(x$eventT)}))

  plot(1,type="n",xaxt="n",yaxt="n",ylab="Humans",xlab="Time (Years)",xlim=c(0,tMax),ylim=c(0,length(history)))
  ttMax = tMax/365
  axis(side = 1,at = c(0:ttMax)*365,labels = c(0:ttMax))

  for(ixH in 1:length(history)){
    plotPfsiOneTrajectory(ixH = ixH,oneHistory = history[[ixH]],tMax = tMax)
  }

}
