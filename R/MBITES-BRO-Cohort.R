#################################################################
#
#   MASH
#   R6-ified
#   MBITES-BRO (Blood Feeding, Resting, Oviposition)
#   Simplified Cohort Simulation
#   David Smith, Hector Sanchez, Sean Wu
#   August 9, 2017
#
#################################################################

#################################################################
# MBITES-BRO-Cohort: Host Encounter
#################################################################

#################################################################
#   Human Host Encounter
#################################################################

#' MBITES-BRO-Cohort: Human Host Encounter for \code{\link{MicroMosquitoFemale}}
#'
#' After calling \code{\link{mbitesGeneric_chooseHost}}, the mosquito encounters a human host and attempts to feed.
#'  * This method is bound to \code{MicroMosquitoFemale$humanEncounter()}.
#' @md
mbitesBRO_cohort_humanEncounter <- function(){
  if(runif(1) < 1-private$FemalePopPointer$get_MBITES_PAR("surviveH")){ # does not survive to probe
    private$stateNew = "D"
  } else { # survives to probe

    if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("probeH")){ # undeterred
      if(runif(1) < 1-private$FemalePopPointer$get_MBITES_PAR("surviveprobeH")){ # does not survive probing
        private$stateNew = "D"
      } else { # survives probing

        if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("feedH")){ # successfully begins feeding
          self$BloodMeal() # MBITES-BRO-Energetics.R
          private$history$historyFeed(privateEnv = private) # MOSQUITO-History.hpp
          private$stateNew = "R"
        }

      }

    }

  }

}


#################################################################
#   Simulation
#################################################################

#' MBITES-BRO-Cohort: Run Simulation for \code{\link{MicroMosquitoFemale}}
#'
#' Run the M-BITES life cycle simulation algorithm until mosquito is dead.
#' This method calls \code{\link{mbitesBRO_oneBout}} to simulate each life stage.
#' For differences between cohort and microsimulation versions see \code{\link{MBITES.BRO.Cohort.Setup}}
#'  * This method is bound to \code{MicroMosquitoFemale$MBITES_Cohort()}.
#'
#' @md
mbitesBRO_cohort_oneMosquito_MBITES <- function(){

  # run algorithm until dead
  while(private$stateNew != "D"){
    self$oneBout()
  }

}


#################################################################
# MBITES-BRO-Cohort: Run Cohort Simulation
#################################################################

#' MBITES-BRO-Cohort: Human Host Encounter for \code{\link{MicroMosquitoFemale}}
#'
#' After calling \code{\link{mbitesGeneric_chooseHost}}, the mosquito encounters a human host and attempts to feed.
#'  * This method is bound to \code{MicroMosquitoPopFemale$simCohort()}.
#' @md
mbitesBRO_cohort_simCohort <- function(N){

  # allocate space for cohort
  private$cohortPop = vector(mode="list",length=N)

  # randomly allocate to initial positions
  aquaN = private$LandscapePointer$AquaSitesN
  aquaIx = sample(x = aquaN,size = N,replace = TRUE)

  # assign cohort
  for(i in 1:N){
    private$cohortPop[[i]] = MicroMosquitoFemale$new(id = as.character(ix), time = 0, ix = aquaIx[ix], genotype = 0L, state = private$initState)
    private$pop[[ix]]$set_FemalePopPointer(self)
    # private$pop[[ix]]$set_MalePopPointer(private$MalePopPointer)
    private$pop[[ix]]$set_TilePointer(private$TilePointer)
    private$pop[[ix]]$set_LandscapePointer(private$LandscapePointer)
    # private$pop[[ix]]$set_HumansPointer(private$HumansPointer)
  }

  # do the sim.

  # write to directory.

  # remove the temporary cohort and manually garbage collect to clear up memory
  rm(private$cohortPop)
  gc()
}
