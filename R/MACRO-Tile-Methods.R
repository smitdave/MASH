#################################################################
#
#   MASH
#   R6-ified
#   MACRO MacroTile Methods Definition
#   David Smith, Hector Sanchez, Sean Wu
#   May 11, 2016
#
#################################################################


# simMacroPfSI = function(mashtask){with(mashtask,{
#   TIME = t0
#   while (TIME < tMAX){
#     TIME = TIME+1
#     tPause = TIME+1
#
#     LANDSCAPE$ImagoQ = oneDay_emerge_MACRO(tNow = tMax)
#     LANDSCAPE$MPop$M = MPop$M + recruitRM(LANDSCAPE)
#     LANDSCAPE$MPop <- deltaM(0, LANDSCAPE$MPop, LANDSCAPE$kappa)
#     LANDSCAPE$EggQ = layEggs(t,LANDSCAPE)
#     runHumanEventQ(tPause = tMax)
#     updateKappa()
#     queueInfectiousBites()
#   }
# })}

#' MACRO \code{MacroTile} Simulate MACRO Dynamics
#'
#' write more stuff!!!!!!!!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
simMacro <- function(tMax){
  while(private$tNow < tMax){
    private$tNow = private$tNow + 1

    private$Patches$oneDay_MacroEmerge()
    private$Patches$addCohort_MacroEmerge()

    private$MosquitoPop$oneDay_RM()
    private$MosquitoPop$layEggs() # doesnt actually do anything in Emerge

    private$HumanPop$simHumans(tPause = private$tNow)

    private$Patches$updateKappa()
    private$HumanPop$queueInfectiousBites()
  }
}

# assign to public method
MacroTile$set(which = "public",name = "simMacro",
          value = simMacro,
          overwrite = TRUE
)
