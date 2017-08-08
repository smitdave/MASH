#################################################################
#
#   MASH
#   MICRO Tile
#   Microsimulation 'MicroTile' MICRO Simulation Method
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 1, 2017
#
#################################################################

#################################################################
# MICRO MicroTile Simulation
#################################################################

#' MICRO \code{\link{MicroTile}}: Run Simulation one Time Step
#'
#' Run MICRO simulation for one time step, the length of which defines the temporal window for indifference to contingent events.
#'
#' @param timeStep the time step of the model
#' @param print print current iteration (disable for running in batch mode)
#' @param logInterval interval to clear population and track mosquito history (see \code{\link{clear_pop}})
#' @md
simMICRO_oneStep <- function(timeStep = 1, print = FALSE, logInterval = 10){

  if(print){
    print(paste0("Current tNow: ",private$tNow))
  }

  # human activity space simulation
  private$HumanPop$sim_ActivitySpace()

  # Aquatic Ecology
  private$Landscape$oneStep_AquaticEcology() # manage lambda -> ImagoQ (emerge) or EggQ -> ImagoQ (EL4P)
  private$Landscape$addCohort() # emerging adults from ImagoQ to MicroMosquitoPopFemale

  # M-BITES
  private$FemalePop$MBITES()
  if(!is.null(private$MalePop)){
    private$MalePop$MBITES()
  }

  # human event queue simulation
  private$HumanPop$simHumans(tPause = private$tNow)

  # clear mosquito pop and track output
  if(private$tNow %% logInterval == 0){
    private$FemalePop$clear_pop(historyTrack = TRUE)
  }

  # update tNow
  private$tNow = private$tNow + timeStep


}

#################################################################
# Set Methods
#################################################################

MicroTile$set(which = "public",name = "simMICRO_oneStep",
          value = simMICRO_oneStep,
          overwrite = TRUE
)
