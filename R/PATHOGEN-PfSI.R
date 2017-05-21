########################################
#  PFSI Setup
#  Sean Wu
#  April 5, 2017
########################################


#' Initialize PfSI Module Parameters (Pathogen)
#'
#' Generate a list of parameters PfSI_PAR in \code{\link{Human}} and public methods in \code{\link{Human}}; also defines public methods
#' in \code{\link{MosquitoFemale}}.
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
#' @return Defines a field (list) PfSI_PAR in \code{\link{HumanPop}} and public methods in \code{\link{Human}}
#' @examples
#' PfSI.Setup()
#' @export
PfSI.Setup <- function(

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

  print(paste0("initializing PfSI PATHOGEN module"))

  ###################################################################
  # Add PfSI Parameters to 'Human' Class
  ###################################################################

  # PfSI_PAR: list of PfSI parameters added to private field of 'Human' class
  Human$set(which = "private",name = "PfSI_PAR",
            value = list(
              Pf_c   = Pf_c,
              Pf_b   = Pf_b,
              DurationPf = DurationPf,
              LatentPf = LatentPf,
              FeverPf = FeverPf,
              mnFeverPf = mnFeverPf,
              vrFeverPf = vrFeverPf,
              TreatPf = TreatPf,
              mnTreatPf = mnTreatPf,
              mnChemoprophylaxisPf = mnChemoprophylaxisPf,
              PEProtectPf = PEProtectPf,
              peBlockPf = peBlockPf,
              mnPEPf = mnPEPf,
              vrPEPf = vrPEPf,
              GSProtectPf = GSProtectPf,
              gsBlockPf = gsBlockPf,
              mnGSPf = mnGSPf,
              vrGSPf = vrGSPf,
              rdtSensPf = rdtSensPf,
              rdtSpecPf = rdtSpecPf,
              lmSensPf = lmSensPf,
              lmSpecPf = lmSpecPf
            )
  )

  # getter for PfSI_PAR
  Human$set(which = "public",name = "get_PfSI_PAR",
            value = function(){
              return(private$PfSI_PAR)
            }
  )

  # setter for PfSI_PAR
  Human$set(which = "public",name = "set_PfSI_PAR",
            value = function(PfSI_PAR){
              private$PfSI_PAR = PfSI_PAR
            }
  )

  ###################################################################
  # Add PfSI Pathogen Object to 'Human' Class
  ###################################################################

  # Human$set(which = "private",name = "Pathogens",
  #           value = list(
  #             Pf = list(
  #               infected = FALSE,
  #               chemoprophylaxis = FALSE,
  #               PfID = NULL
  #             )
  #           ),
  #           overwrite = TRUE
  # )

  ###################################################################
  # Add PfSI Timing Functions to 'Human' Class
  ###################################################################

  # Duration of Infection
  # (How many days does the infection last?)
  Human$set(which = "public",name = "ttClearPf",
            value = function(){
              return(rexp(n = 1, rate = 1/private$PfSI_PAR$DurationPf))
            }
  )

  # Latency:
  # (How many days after the infectious
  #  bite does the infection start?)
  Human$set(which = "public",name = "ttInfectionPf",
            value = function(){
              return(private$PfSI_PAR$LatentPf)
            }
  )

  # Timing of Fever Incident
  # (relative to start of the infection)
  Human$set(which = "public",name = "ttFeverPf",
            value = function(){
              return(private$PfSI_PAR$mnFeverPf)
            }
  )

  # Timing of Treatment
  # (relative to start of the fever)
  Human$set(which = "public",name = "ttTreatPf",
            value = function(){
              return(private$PfSI_PAR$mnTreatPf)
            }
  )

  # Prophylaxis, time to susceptibility
  Human$set(which = "public",name = "ttSusceptiblePf",
            value = function(){
              return(private$PfSI_PAR$mnChemoprophylaxisPf)
            }
  )

  # Duration of protection by PE Vaccination
  Human$set(which = "public",name = "ttPEWanePf",
            value = function(){
              return(rnorm(n = 1, mean = private$PfSI_PAR$mnPEPf, sd = private$PfSI_PAR$vrPEPf))
            }
  )

  # Duration of protection Blocked by GS Vaccination
  Human$set(which = "public",name = "ttGSWanePf",
            value = function(){
              return(rnorm(n = 1, mean = private$PfSI_PAR$mnGSPf, sd = private$PfSI_PAR$vrGSPf))
            }
  )

  ###################################################################
  # PfSI: Mosquito to Human infectious bite
  # Add methods to 'Human' and 'MosquitoFemale' Classes
  ###################################################################

  # probeHost_PfSI ADD METHOD TO MosquitoFemale CLASS

  # infectiousBite_PfSI
  Human$set(which = "public",name = "infectiousBite_PfSI",
            value = function(tBite, PAR){
              if(runif(1) < private$Pathogens$PfSI_PAR$Pf_b){
                tInfStart = tBite + self$ttInfectionPf()
                # track transmission?
                self$add2Q_infectHumanPfSI(tEvent = tInfStart, PAR = PAR)
              }
            }
  )


  ###################################################################
  # Add PfSI Events to 'Human' Class
  # 'XX' family of functions for human event queues
  ###################################################################

  ###################################################################
  # Start a PfSI Infection
  ###################################################################

  # add2Q_infectHumanPfSI
  Human$set(which = "public",name = "add2Q_infectHumanPfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = self$event_infectHumanPfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  # event_infectHumanPfSI: begin a PfSI infection
  Human$set(which = "public",name = "event_infectHumanPfSI",
            value = function(tEvent, PAR = NULL){
              list(tEvent = tEvent, PAR = PAR, tag = "infectHumanPfSI")
            }
  )

  # infectHumanPfSI
  Human$set(which = "public",name = "infectHumanPfSI",
            value = function(tEvent, PAR){
              if(!private$Pathogens$Pf$infected & !private$Pathogens$Pf$chemoprophylaxis){
                self$trackHist(tEvent = tEvent, event = "I") # track history
                private$Pathogens$Pf$infected = TRUE
                private$Pathogens$Pf$PfID = PAR$get_PfID()
                if(runif(1) < private$PfSI_PAR$FeverPf){
                    self$add2Q_feverPfSI(tEvent = tEvent)
                }
                self$add2Q_endPfSI(tEvent = tEvent)
              }
            }
  )

  ###################################################################
  # End an PfSI Infection
  ###################################################################

  # add2Q_endPfSI
  Human$set(which = "public",name = "add2Q_endPfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = self$event_endPfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  # event_endPfSI: end a PfSI infection
  Human$set(which = "public",name = "event_endPfSI",
            value = function(tEvent, PAR = NULL){
              tE = tEvent + self$ttClearPf()
              list(tEvent = tFever, PAR = PAR, tag = "endPfSI")
            }
  )

  # endPfSI
  Human$set(which = "public",name = "endPfSI",
            value = function(tEvent, PAR){
              if(private$Pathogens$Pf$infected){
                self$trackHist(tEvent = tEvent, event = "S") # track history
                private$Pathogens$Pf$infected = FALSE
              }
            }
  )

  ###################################################################
  # Fever
  ###################################################################

  Human$set(which = "public",name = "add2Q_feverPfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = self$event_feverPfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  Human$set(which = "public",name = "event_feverPfSI",
            value = function(tEvent, PAR = NULL){
              tFever = tEvent + self$ttFeverPf()
              list(tEvent = tFever, PAR = PAR, tag = "feverPfSI")
            }
  )

  Human$set(which = "public",name = "feverPfSI",
            value = function(tEvent, PAR){
              self$trackHist(tEvent = tEvent, event = "F")
              if(runif(1) < private$PfSI_PAR$TreatPf){
                self$add2Q_treatPfSI(tEvent = tEvent)
              }
            }
  )

  ###################################################################
  # Treatment
  ###################################################################

  Human$set(which = "public",name = "add2Q_treatPfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = self$event_treatPfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  Human$set(which = "public",name = "event_treatPfSI",
            value = function(tEvent, PAR = NULL){
              tTreat = tEvent + self$ttTreatPf()
              list(tEvent = tTreat, PAR = PAR, tag = "treatPfSI")
            }
  )

  Human$set(which = "public",name = "treatPfSI",
            value = function(tEvent, PAR){

              # treat
              if(private$Pathogens$Pf$infected){
                private$Pathogens$Pf$infected = FALSE
                self$trackHist(tEvent = tEvent, event = "S")
              }
              private$Pathogens$Pf$chemoprophylaxis = TRUE
              self$trackHist(tEvent = tEvent, event = "P")
              # Initiate a period of protection from chemoprophlaxis
              self$add2Q_endprophylaxisPfSI(tEvent = tEvent)

            }
  )

  ###################################################################
  # End of Chemoprophylaxis
  ###################################################################

  Human$set(which = "public",name = "add2Q_endprophylaxisPfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = self$event_endprophylaxisPfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  Human$set(which = "public",name = "event_endprophylaxisPfSI",
            value = function(tEvent, PAR = NULL){
              tSusceptible = tEvent + self$ttSusceptiblePf()
              list(tEvent = tSusceptible, PAR = PAR, tag = "endprophylaxisPfSI")
            }
  )

  Human$set(which = "public",name = "endprophylaxisPfSI",
            value = function(tEvent, PAR){

              # End Prophylaxis
              self$trackHist(tEvent = tEvent, event = "S")
              private$Pathogens$Pf$chemoprophylaxis = FALSE

            }
  )

  ###################################################################
  # HUMAN PE vaccination functions
  ###################################################################

  # vaccination
  Human$set(which = "public",name = "add2Q_pevaccinatePfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = self$event_pevaccinatePfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  Human$set(which = "public",name = "event_pevaccinatePfSI",
            value = function(tEvent, PAR = NULL){
              list(tEvent = tSusceptible, PAR = PAR, tag = "pevaccinatePfSI")
            }
  )

  Human$set(which = "public",name = "pevaccinatePfSI",
            value = function(tEvent, PAR){
              # PATHOGENS$Pf IS NOW A CLASS AGAIN...NEED TO ADJUST STUFF ABOVE
              if(rbinom(1,1,PEProtectPf)){
                HUMANS[[ixH]]$Pathogens$Pf$b <<- Pf_b * (1-peBlockPf)
                add2Q_pewanePfSI(ixH, t)
              }

            }
  )



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

  # waning protection
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

  # vaccination
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

  # waning protection
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







  # HumanPop$set(which = "private",name = "init.PfSI",
  #
  #           value = function(PfPR){
  #
  #             PfID = 1L
  #             for(ixH in 1:self$nHum){
  #
  #               if(runif(1) < PfPR){
  #
  #                 PfID = PfID + 1L
  #               } else {
  #                 private$pop[[ix]]$trackHist(tEvent = 0, event = "S")
  #               }
  #
  #             }
  #
  #           }
  # )





  ###################################################################
  # PfSI Diagnostics
  ###################################################################

  Human$set(which = "public",name = "rdtTest_PfSI",
            value = function(tEvent, PAR){
              if(private$Pathogens$Pf$infected){
                runif(1) < private$PfSI_PAR$rdtSensPf
              } else {
                runif(1) < private$PfSI_PAR$rdtSpecPf
              }
            }
  )

  Human$set(which = "public",name = "lmTest_PfSI",
            value = function(tEvent, PAR){
              if(private$Pathogens$Pf$infected){
                runif(1) < private$PfSI_PAR$lmSensPf
              } else {
                runif(1) < private$PfSI_PAR$lmSpecPf
              }
            }
  )


}













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
