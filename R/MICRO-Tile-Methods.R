#################################################################
#
#   MASH
#   MICRO Tile
#   Microsimulation 'MicroTile' MICRO Simulation Method
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 1, 2017
#
#################################################################

#' MICRO \code{\link{MicroTile}}: Run Simulation one Time Step
#'
#' Run MICRO simulation for one time step, the length of which defines the temporal window for indifference to contingent events.
#'
#' @param timeStep the time step of the model
#' @md
simMICRO_oneStep <- function(timeStep = 1, print = FALSE){

  if(print){
    print(paste0("Current tMax: ",private$tMax))
  }

  # human activity space simulation
  private$HumanPop$sim_ActivitySpace()

  # # human activity space simulation
  # activitySpace()
  #
  # # "EL4P" Aquatic Ecology
  # oneDay_EL4P(tNow = tMax)
  # addCohort(tNow = tMax)
  #
  # # male MBITES
  # MBITESmale(P)
  #
  # # female MBITES
  # MBITES(P)
  #
  # # human events
  # runHumanEventQ(tPause = tMax)
  #
  # # track all output
  # trackEL4P(con = el4pCon)
  # trackAdults(con = adultCon)
  # # log mosquito data and clear out vector every 10 days
  # if((tMax > tStart) & (tMax %% 10 == 0)){
  #   print(paste0("logging mosquito bionomics and histories"))
  #   trackBionomics(directory = out,fileName = paste0("bionomics",tMax,".json"))
  #   trackHistory(directory = out,fileName = paste0("historyF",tMax,".json"))
  #   trackHistoryM(directory = out,fileName = paste0("historyM",tMax,".json"))
  #   resetMosyPop(female = TRUE)
  #   resetMosyPop(female = FALSE)
  # }
  # # log human event data when simulation completes
  # if(tMax == tEnd){
  #   writeHumanEvent_PfSI(directory = out,fileName = "humanPfSI.json")
  # }


  if(!is.null(private$MalePop)){
    # run male dynamics
  }

  # update tNow
  private$tNow = private$tNow + timeStep

}
