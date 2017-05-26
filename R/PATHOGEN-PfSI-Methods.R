#################################################################
#
#   MASH
#   R6-ified
#   PfSI Setup
#   David Smith, Hector Sanchez, Sean Wu
#   May 21, 2017
#
#################################################################


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
  # Add PfSI Utilities to 'HumanPop' Class
  ###################################################################

  # PfID counter
  HumanPop$set(which = "private",name = "PfID",
            value = 0L
  )

  # whenever a new liver-stage infection manifests in a human we increment the PfID counter and return the new PfID
  HumanPop$set(which = "public",name = "increment_PfID",
            value = function(){
              private$PfID = private$PfID + 1L
              return(private$PfID)
            }
  )

  # initialize PfSI infections with parasite prevalence PfPR
  HumanPop$set(which = "public",name = "PfSI.Init",

            value = function(PfPR, tStart = 0, b = NULL, c = NULL){

              private$PfID = 1L
              self$set_humanPfSI(b,c)

              for(ixH in 1:self$nHumans){

                if(runif(1) < PfPR){
                  private$pop[[ixH]]$infectHumanPfSI(tEvent = tStart, PAR = list(damID=-1L,sireID=-1L))
                } else {
                  private$pop[[ixH]]$trackHist(tEvent = tStart, event = "S")
                }

              }

            }
  )


  ###################################################################
  # Add PfSI Pathogen Object to 'Human' & 'HumanPop' Class
  ###################################################################

  Human$set(which = "public",name = "set_humanPfSI",
            value = function(PfID, tInf = NULL, b = 0.55, c = 0.15, damID = NULL, sireID = NULL, infected = FALSE, chemoprophylaxis = FALSE){
              private$Pathogens$Pf = humanPfSI$new(PfID=PfID,tInf=tInf,b=b,c=c,damID=damID,sireID=sireID,infected=infected,chemoprophylaxis=chemoprophylaxis)
            },
            overwrite = TRUE
  )

  Human$set(which = "public",name = "get_humanPfSI",
            value = function(){
              return(private$Pathogens$Pf)
            },
            overwrite = TRUE
  )

  HumanPop$set(which = "public",name = "set_humanPfSI",
            value = function(b = NULL, c = NULL){

              # sanity checks
              if(is.null(b)){
                b = rep(x = 0.55,times = self$nHumans)
              } else {
                if(length(b)!=self$nHumans){
                  stop(paste0("length of b: ",length(b)," must be equal to size of human population: ",self$nHumans))
                }
              }
              if(is.null(c)){
                c = rep(x = 0.15,times = self$nHumans)
              } else {
                if(length(c)!=self$nHumans){
                  stop(paste0("length of c: ",length(c)," must be equal to size of human population: ",self$nHumans))
                }
              }

              # set pathogens
              for(ixH in 1:self$nHumans){
                private$pop[[ixH]]$set_humanPfSI(PfID = NULL, b = b[ixH], c = c[ixH])
              }

            },
            overwrite = TRUE
  )

  ###################################################################
  # Add PfSI Pathogen Object to 'MosquitoFemale' & 'MosquitoPopFemale' Class
  ###################################################################

  # DO THIS WHEN MOSQUITOES EXIST


  ###################################################################
  # PfSI: Mosquito to Human infectious bite
  # Add methods to 'Human' Class
  ###################################################################

  # probeHost_PfSI:
  # arguments are tBite (time of bite)
  # mosquitoPfSI; the mosquitoPfSI R6 object passed from the biting mosquito
  Human$set(which = "public",name = "probeHost_PfSI",

            value = function(tBite, mosquitoPfSI){
              if(any(mosquitoPfSI$get_spz()>0)){
                PAR = list(mosquitoPfSI = mosquitoPfSI)
                self$infectiousBite_PfSI(tBite, PAR)
              }

            }
  )

  # infectiousBite_PfSI
  Human$set(which = "public",name = "infectiousBite_PfSI",
            value = function(tBite, PAR){
              if(runif(1) < private$Pathogens$Pf$get_b()){

                PfIx = sample(x = which(PAR$mosquitoPfSI$get_spz()>0), size = 1) # sample a clonal variant if multiple
                PAR = list(damID = PfIx, sireID = PfIx)

                tInfStart = tBite + self$ttInfectionPf()
                self$add2Q_infectHumanPfSI(tEvent = tInfStart, PAR = PAR)
              }
            }
  )


  ###################################################################
  # PfSI: Human to Mosquito infectious bite
  # Add methods to 'MosquitoFemale' Classe
  ###################################################################

  # its okay if this function takes as a direct argument humanPfSI;
  # like probeHost_PfSI, the functions that directly interact between classes are ok to pass these things by name rather than generic PAR
  # try to reserve generic argument names like PAR when all modification is within-class; ie, when there will be no ambiguity.
  # do not use when objects are passed between classes

  # infectMosquito_PfSI <- function(tBite, ixH, ixS, ixM){
  #   with(HUMANS[[ixH]]$Pathogens$Pf,{
  #     if(infected==TRUE & rbinom(1,1,HUMANS[[ixH]]$Pathogens$Pf$c)){
  #       infObj = makePfM(ixH, tBite, ixS)
  #       if(PfTransmission_TRACK){
  #         trackPfTransmission(M2H = FALSE, tBite = tBite, ixH = ixH, ixS = ixS, ixM = ixM, PfM = infObj$PfM)
  #       }
  #       return(infObj)
  #     } else {
  #       infObj = list(infected = FALSE)
  #       return(infObj)
  #     }
  #   })
  # }


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
              if(!private$Pathogens$Pf$get_infected() & !private$Pathogens$Pf$get_chemoprophylaxis()){
                self$trackHist(tEvent = tEvent, event = "I") # track history
                private$Pathogens$Pf$set_infected(TRUE)

                # newID = self$get_HumansPointer()$increment_PfID()
                # private$Pathogens$Pf$push_PfID(newID)

                private$Pathogens$Pf$push_PfID(self$get_HumansPointer()$increment_PfID())

                private$Pathogens$Pf$push_damID(PAR$damID)
                private$Pathogens$Pf$push_sireID(PAR$sireID)
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
              tEnd = tEvent + self$ttClearPf()
              list(tEvent = tEnd, PAR = PAR, tag = "endPfSI")
            }
  )

  # endPfSI
  Human$set(which = "public",name = "endPfSI",
            value = function(tEvent, PAR){
              if(private$Pathogens$Pf$get_infected()){
                self$trackHist(tEvent = tEvent, event = "S") # track history
                private$Pathogens$Pf$set_infected(FALSE)
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
              if(private$Pathogens$Pf$get_infected()){
                private$Pathogens$Pf$set_infected(FALSE)
                self$trackHist(tEvent = tEvent, event = "S")
              }
              private$Pathogens$Pf$set_chemoprophylaxis(TRUE)
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
              private$Pathogens$Pf$set_chemoprophylaxis(FALSE)

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
              list(tEvent = tEvent, PAR = PAR, tag = "pevaccinatePfSI")
            }
  )

  Human$set(which = "public",name = "pevaccinatePfSI",
            value = function(tEvent, PAR){
              if(runif(1) < private$PfSI_PAR$PEProtectPf){
                self$trackHist(tEvent = tEvent, event = "PEvaxx")
                private$Pathogens$Pf$set_b(private$PfSI_PAR$Pf_b * (1-private$PfSI_PAR$peBlockPf))
                self$add2Q_pewanePfSI(tEvent = tEvent)
              }
            }
  )

  # waning protection
  Human$set(which = "public",name = "add2Q_pewanePfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = self$event_pewanePfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  Human$set(which = "public",name = "event_pewanePfSI",
            value = function(tEvent, PAR = NULL){
              tWane = tEvent + self$ttPEWanePf()
              list(tEvent = tWane, PAR = PAR, tag = "pewanePfSI")
            }
  )

  Human$set(which = "public",name = "pewanePfSI",
            value = function(tEvent, PAR){
              self$trackHist(tEvent = tEvent, event = "PEwane")
              private$Pathogens$Pf$set_b(private$PfSI_PAR$Pf_b)
            }
  )

  ###################################################################
  # HUMAN GS vaccination functions
  ###################################################################

  # vaccination
  Human$set(which = "public",name = "add2Q_gsvaccinatePfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = self$event_gsvaccinatePfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  Human$set(which = "public",name = "event_gsvaccinatePfSI",
            value = function(tEvent, PAR = NULL){
              list(tEvent = tEvent, PAR = PAR, tag = "gsvaccinatePfSI")
            }
  )

  Human$set(which = "public",name = "gsvaccinatePfSI",
            value = function(tEvent, PAR){
              if(runif(1) < private$PfSI_PAR$GSProtectPf){
                self$trackHist(tEvent = tEvent, event = "GSvaxx")
                private$Pathogens$Pf$set_c(private$PfSI_PAR$Pf_c * (1-private$PfSI_PAR$gsBlockPf))
                self$add2Q_gswanePfSI(tEvent = tEvent)
              }
            }
  )

  # waning protection
  Human$set(which = "public",name = "add2Q_gswanePfSI",
            value = function(tEvent, PAR = NULL){
              self$addEvent2Q(event = self$event_gswanePfSI(tEvent = tEvent, PAR = PAR))
            }
  )

  Human$set(which = "public",name = "event_gswanePfSI",
            value = function(tEvent, PAR = NULL){
              tWane = tEvent + self$ttGSWane()
              list(tEvent = tWane, PAR = PAR, tag = "gswanePfSI")
            }
  )

  Human$set(which = "public",name = "gswanePfSI",
            value = function(tEvent, PAR){
              self$trackHist(tEvent = tEvent, event = "GSwane")
              private$Pathogens$Pf$set_c(private$PfSI_PAR$Pf_c)
            }
  )

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

  ###################################################################
  # PfSI Auxiliary Definitions
  ###################################################################

  PfSI.Auxiliary.Setup()

}
