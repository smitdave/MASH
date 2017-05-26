#################################################################
#
#   MASH
#   R6-ified
#   MACRO MacroTile Methods Definition
#   David Smith, Hector Sanchez, Sean Wu
#   May 11, 2016
#
#################################################################


simMacroPfSI = function(mashtask){with(mashtask,{
  TIME = t0
  while (TIME < tMAX){
    TIME = TIME+1
    tPause = TIME+1

    LANDSCAPE$ImagoQ = oneDay_emerge_MACRO(tNow = tMax)
    LANDSCAPE$MPop$M = MPop$M + recruitRM(LANDSCAPE)
    LANDSCAPE$MPop <- deltaM(0, LANDSCAPE$MPop, LANDSCAPE$kappa)
    LANDSCAPE$EggQ = layEggs(t,LANDSCAPE)
    runHumanEventQ(tPause = tMax)
    updateKappa()
    queueInfectiousBites()
  }
})}
