#################################################################
#
#   MASH
#   R6-ified
#   MBITES Module Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   July 19, 2017
#
#################################################################

#################################################################
# M-BITES FULL Parameters
#################################################################

#' Generate Parameters for M-BITES 'Full' Module
#'
#' Generate named list of parameters used throughout M-BITES 'Full' Module. All arguments have default values which are listed below before the definition.
#'
#' @param enterhouse.a = 9, # beta house entry parameters
#' @param enterhouse.b = 1, # beta house entry parameters
#' @param feedHaz.a = 99, # beta feeding site hazard weights
#' @param feedHaz.b = 1, # beta feeding site hazard weights
#' @param aquaHaz.a = 99, # beta aqua site hazard weights
#' @param aquaHaz.b = 1, # beta aqua site hazard weights
#' @param sugarHaz.a = 99, # beta sugar site hazard weights
#' @param sugarHaz.b = 1, # beta sugar site hazard weights
#' @param swarmHaz.a = 99, # beta swarm site hazard weights
#' @param swarmHaz.b = 1, # beta swarm site hazard weights
#' @param F.t = 1, #blood feed search bout timing
#' @param B.t = 0.75, #blood feed attempt bout timing
#' @param R.t = 1.5, #resting bout timing
#' @param L.t = 0.75, #egg laying search bout timing
#' @param O.t = 1, #egg laying attempt bout timing
#' @param M.t = 1.5, #mating bout timing
#' @param S.t = 0.5, #sugar feeding bout timing
#' @param F.s = 0.99,
#' @param B.s = 0.99,
#' @param L.s = 0.99,
#' @param O.s = 0.99,
#' @param M.s = 0.95,
#' @param S.s = 0.95,
#' @param F.p = 0.95,
#' @param B.p = 0.98,
#' @param R.p = 0.98,
#' @param L.p = 0.80,
#' @param O.p = 0.98,
#' @param M.p = 0.98,
#' @param S.p = 0.98,
#' @param maleM.s = .99,
#' @param maleM.p = .95,
#' @param Q = 0.9, #human blood index
#' @param reFeed = 0.01, #probability to refeed post resting bout
#' @param bm.a = 7.5, #shape param for bloodmeal size
#' @param bm.b = 2.5, #shape param for bloodmeal size
#' @param rf.a = 60, #exp param for refeeding as function of bmSize
#' @param rf.b = 5e3, #exp param for refeeding as function of bmSize
#' @param of.a = 5, #exp param for overfeeding as function of bmSize
#' @param of.b = 5e3, #exp param for overfeeding as function of bmSize
#' @param S.a = 20, #exp param for death as function of energy
#' @param S.b = 10, #exp param for death as function of energy
#' @param S.u = 1/7, #energy expenditure for each bout
#' @param S.sa = 15, #exp param for sugar bout as function of energy
#' @param S.sb = 5, #exp param for sugar bout as function of energy
#' @param sf.a = 7.5, #shape param for sugar feed size
#' @param sf.b = 4.5, #shape param for sugar feed size
#' @param B.energy = 1/10, #scaling factor energy from a bloodmeal
#' @param energyPreG = 0, #pre-gonotrophic energy requirement
#' @param preGsugar = 0,
#' @param preGblood = 0,
#' @param sns.a = 0.085, #exp param for senescence
#' @param sns.b = 100, #exp param for senescence
#' @param ttsz.p = 0.5, #zero-inflation for tattering damage
#' @param ttsz.a = 5, #shape param for tattering damage
#' @param ttsz.b = 95, #shape param for tattering damage
#' @param ttr.a = 15, #exp param for tattering survival
#' @param ttr.b = 500, #exp param for tattering survival
#' @param maxBatch = 30, #maximum batch size
#' @param E.p = 0.5, # probability of surviving estivation
#' @param Emax = 90, # onset of the dry season; a day of the calendar year
#' @param Eb = 0.9,
#' @param eEndm = 180, # end of estivation; a day of the calendar year
#' @param eEndV = 30,
#' @param surviveH = 1,
#' @param probeH = 1,
#' @param surviveprobeH = 1,
#' @param feedH = 1,
#' @param surviveL = 1,
#' @param feedL = 1,
#' @param SUGAR = TRUE,
#' @param ESTIVATION = FALSE,
#' @param MATE = TRUE,
#' @param SENESCE = TRUE, #senesce
#' @param TATTER = TRUE, #tattering
#' @param REFEED = TRUE,
#' @param OVERFEED TRUE; simulate overfeeding? See \code{\link{overFeed}}
#' @param HISTORY TRUE; record history? Needed for \code{\link{cohortBionomics}}, see \code{\link{historyTrack}}
#' @param batchSize "bms"; switches for how egg batch size is calculated, should be either "bms" or "norm", see \code{\link{BatchSize.bms}} or \code{\link{BatchSize.norm}}
#' @param eggMatT "off"; switches for how egg maturation is calculated, should be either "off" or "norm", see \code{\link{eggMaturationTime.off}} or \code{\link{eggMaturationTime.norm}}
#' @param InAndOut matrix of weights for landing spot, see \code{\link{newSpot}}
#' @param Fwts rep(1,5); blood feeding search bout landing spot weights, see \code{\link{newSpot}}
#' @param Rwts rep(1,5); resting bout landing spot weights, see \code{\link{newSpot}}
#' @param Lwts rep(1,5); egg laying search bout landing spot weights, see \code{\link{newSpot}}
#' @param Owts rep(1,5); egg laying attempt bout landing spot weights, see \code{\link{newSpot}}
#' @param Mwts rep(1,5); mating bout landing spot weights, see \code{\link{newSpot}}
#' @param Swts rep(1,5); sugar bout landing spot weights, see \code{\link{newSpot}}
#' @param eggT the minimum time before eggs are mature
#' @param eggP the minimum provision before eggs are mature
#' @param Pathogen name of the PATHOGEN module to use, can be in "PfSI", "PfMOI"
#' @return a named list of parameters
#' @examples
#' P = MBITES.FULL.Parameters()
#' @export
MBITES.FULL.Parameters <- function(

  # landscape creation parameters
  enterhouse.a = 9, # beta house entry parameters
  enterhouse.b = 1, # beta house entry parameters
  feedHaz.a = 99, # beta feeding site hazard weights
  feedHaz.b = 1, # beta feeding site hazard weights
  aquaHaz.a = 99, # beta aqua site hazard weights
  aquaHaz.b = 1, # beta aqua site hazard weights
  sugarHaz.a = 99, # beta sugar site hazard weights
  sugarHaz.b = 1, # beta sugar site hazard weights
  swarmHaz.a = 99, # beta swarm site hazard weights
  swarmHaz.b = 1, # beta swarm site hazard weights

  #rate parameters for exponential timing
  F.t = 1, #blood feed search bout timing
  B.t = 0.75, #blood feed attempt bout timing
  R.t = 1.5, #resting bout timing
  L.t = 0.75, #egg laying search bout timing
  O.t = 1, #egg laying attempt bout timing
  M.t = 1.5, #mating bout timing
  S.t = 0.5, #sugar feeding bout timing

  #success parameters
  F.s = 0.99,
  B.s = 0.99,
  L.s = 0.99,
  O.s = 0.99,
  M.s = 0.95,
  S.s = 0.95,

  #survival parameters
  F.p = 0.95,
  B.p = 0.98,
  R.p = 0.98,
  L.p = 0.80,
  O.p = 0.98,
  M.p = 0.98,
  S.p = 0.98,

  #male specific parameters
  maleM.s = .99,
  maleM.p = .95,

  #energetics and feeding
  Q = 0.9, #human blood index
  reFeed = 0.01, #probability to refeed post resting bout
  bm.a = 7.5, #shape param for bloodmeal size
  bm.b = 2.5, #shape param for bloodmeal size
  rf.a = 60, #exp param for refeeding as function of bmSize
  rf.b = 5e3, #exp param for refeeding as function of bmSize
  of.a = 5, #exp param for overfeeding as function of bmSize
  of.b = 5e3, #exp param for overfeeding as function of bmSize
  S.a = 20, #exp param for death as function of energy
  S.b = 10, #exp param for death as function of energy
  S.u = 1/7, #energy expenditure for each bout
  S.sa = 15, #exp param for sugar bout as function of energy
  S.sb = 5, #exp param for sugar bout as function of energy
  sf.a = 7.5, #shape param for sugar feed size
  sf.b = 4.5, #shape param for sugar feed size

  B.energy = 1/10, #scaling factor energy from a bloodmeal
  energyPreG = 0, #pre-gonotrophic energy requirement
  preGsugar = 0,
  preGblood = 0,
  eggP.min = 0.5, # minimum energy provision for eggs to mature

  #senescence and tattering
  sns.a = 0.085, #exp param for senescence
  sns.b = 100, #exp param for senescence
  ttsz.p = 0.5, #zero-inflation for tattering damage
  ttsz.a = 5, #shape param for tattering damage
  ttsz.b = 95, #shape param for tattering damage
  ttr.a = 15, #exp param for tattering survival
  ttr.b = 500, #exp param for tattering survival

  #egg production
  maxBatch = 30, #maximum batch size

  # estivation
  E.p = 0.5, # probability of surviving estivation
  Emax = 90, # onset of the dry season; a day of the calendar year
  Eb = 0.9,
  eEndm = 180, # end of estivation; a day of the calendar year
  eEndV = 30,

  #human host
  surviveH = 1,
  probeH = 1,
  surviveprobeH = 1,
  feedH = 1,

  #animal host
  surviveL = 1,
  feedL = 1,

  #control parameters
  SUGAR = TRUE,
  ESTIVATION = FALSE,
  MATE = TRUE,
  SENESCE = TRUE, #senesce
  TATTER = TRUE, #tattering
  REFEED = TRUE,
  OVERFEED = TRUE,
  HISTORY = TRUE,

  # switches
  batchSize = "bms", # should be {"bms","norm"}
  eggMatT = "off", # should be {"off","norm"}

  #landing and movement
  InAndOut = matrix(
    c(
      c(4,2,1,0,1),
      c(2,1,1,0,1),
      c(2,1,1,0,2),
      c(1,1,1,0,1),
      c(6,4,2,1,0)
    ), byrow=FALSE, ncol=5),

  Fwts = rep(1,5),
  Rwts = rep(1,5),
  Lwts = rep(1,5),
  Owts = rep(1,5),
  Mwts = rep(1,5),
  Swts = rep(1,5),

  eggT = 0,
  eggP = 0,
  Pathogen = "PfSI"


){

  return(list(

    # landscape creation parameters
    enterhouse.a = enterhouse.a, # beta house entry weights
    enterhouse.b = enterhouse.b, # beta house entry weights
    enterhouse.mean = enterhouse.a / (enterhouse.a + enterhouse.b), # mean of beta distributed house entry probabilities
    feedHaz.a = feedHaz.a, # beta feeding site hazard weights
    feedHaz.b = feedHaz.b, # beta feeding site hazard weights
    feedHaz.mean = feedHaz.a / (feedHaz.a + feedHaz.b), # mean
    aquaHaz.a = aquaHaz.a, # beta aqua site hazard weights
    aquaHaz.b = aquaHaz.b, # beta aqua site hazard weights
    aquaHaz.mean = aquaHaz.a / (aquaHaz.a + aquaHaz.b), # mean
    sugarHaz.a = sugarHaz.a, # beta sugar site hazard weights
    sugarHaz.b = sugarHaz.b, # beta sugar site hazard weights
    sugarHaz.mean = sugarHaz.a / (sugarHaz.a + sugarHaz.b), # mean
    swarmHaz.a = swarmHaz.a, # beta swarm site hazard weights
    swarmHaz.b = swarmHaz.b, # beta swarm site hazard weights
    swarmHaz.mean = swarmHaz.a / (swarmHaz.a + swarmHaz.b), # mean

    #timing parameters
    F.t=F.t, B.t=B.t, R.t=R.t, L.t=L.t, O.t=O.t, M.t=M.t, S.t=S.t,

    #success parameters
    F.s=F.s, B.s=B.s, L.s=L.s, O.s=O.s, M.s=M.s, S.s=S.s,

    #survival parameters
    F.p=F.p, B.p=B.p, R.p=R.p, L.p=L.p, O.p=O.p, M.p=M.p, S.p=S.p,

    #male specific parameters
    maleM.s=maleM.s,
    maleM.p=maleM.p,

    #energetics & feeding
    Q=Q, reFeed=reFeed, bm.a=bm.a, bm.b=bm.b, rf.a=rf.a, rf.b=rf.b, of.a=of.a, of.b=of.b, S.a=S.a, S.b=S.b, S.u=S.u, S.sa=S.sa, S.sb=S.sb, sf.a=sf.a, sf.b=sf.b,
    B.energy = B.energy,
    energyPreG = energyPreG,
    preGsugar = preGsugar,
    preGblood = preGblood,

    #senescence & tattering
    sns.a=sns.a, sns.b=sns.b, ttsz.p=ttsz.p, ttsz.a=ttsz.a, ttsz.b=ttsz.b, ttr.a=ttr.a, ttr.b=ttr.b,

    #egg production
    maxBatch=maxBatch,

    # estivation
    E.p = E.p,
    Emax = Emax,
    Eb = Eb,
    eEndm = eEndm,
    eEndV = eEndV,

    #human host
    surviveH=surviveH, probeH=probeH, surviveprobeH=surviveprobeH, feedH=feedH,

    #animal host
    surviveL=surviveL, feedL=feedL,

    # switches
    batchSize = batchSize,
    eggMatT = eggMatT,

    #house entering & resting parameters
    InAndOut=InAndOut, Fwts=Fwts, Rwts=Rwts, Lwts=Lwts, Owts=Owts, Mwts=Mwts, Swts=Swts,

    #control parameters
    SUGAR=SUGAR, ESTIVATION=ESTIVATION, MATE=MATE, SENESCE=SENESCE, TATTER=TATTER, REFEED=REFEED, OVERFEED=OVERFEED, HISTORY=HISTORY,

    eggT = eggT,
    eggP = eggP,
    energyPreG = energyPreG,
    Pathogen = Pathogen
  ))

}


#################################################################
# M-BITES BRO Parameters
#################################################################

#' Generate Parameters for M-BITES 'BRO' Module
#'
#' Generate named list of parameters used throughout MICRO/M-BITES. All arguments have default values which are listed below before the definition.
#'
#' @param B_wts landing spot weights
#' @param R_wts landing spot weights
#' @param O_wts landing spot weights
#' @param InAndOut matrix of landing probabilities
#' @param bfa.p Blood Feed Attempt: Prob Survives (Base)
#' @param bfa.s Blood Feed Attempt: Prob Succeeds
#' @param B_time Blood Feed Attempt: Mean Time Elapsed (in Days)
#' @param Q Proportion of Bites on Humans
#' @param bfs.L Blood Feed Search: Prob Leaves Location
#' @param bfs.E Blood Feed Search: Prob Enters New Location
#' @param bfs.p Blood Feed Search: Prob Survives
#' @param F_time Blood Feed Search: Mean Time Elapsed (in Days)
#' @param bms.a Blood Meal Size, a
#' @param bms.b Blood Meal Size, b
#' @param bms.z Blood Feeding Hazard
#' @param rf.a ReFeed WRITE ME
#' @param rf.b ReFeed WRITE ME
#' @param of.a Overfeed WRITE ME
#' @param of.b Overfeed WRITE ME
#' @param ppr.p Resting: Prob Survives (Base)
#' @param R_time Resting: Mean Time Elapsed (in Days)
#' @param reFeed WRITE ME
#' @param ela.p Egg Laying Attempt: Prob Survives (Base)
#' @param ela.s Egg Laying Attempt: Prob Success
#' @param O_time Egg Laying Attempt: Mean Time Elapsed
#' @param els.p Egg Laying Search: Prob Survives (Base)
#' @param L_time Egg Laying Search: Mean Time Elapsed
#' @param els.a Egg Laying Search: Prob Accepts
#' @param sns.a WRITE ME
#' @param sns.b WRITE ME
#' @param sgr.d WRITE ME
#' @param sgr.p WRITE ME
#' @param sgr.t WRITE ME
#' @param sgr.a WRITE ME
#' @param sgr.b WRITE ME
#' @param sgr.sa WRITE ME
#' @param sgr.sb WRITE ME
#' @param sgr.sz WRITE ME
#' @param sgr.cvr WRITE ME
#' @param ttsz.p WRITE ME
#' @param ttsz.a WRITE ME
#' @param ttsz.b WRITE ME
#' @param ttr.a WRITE ME
#' @param ttr.b WRITE ME
#' @param rpl.cvr WRITE ME
#' @param rpl.rpl WRITE ME
#' @param rpl.die WRITE ME
#' @param eggT the minimum time before eggs are mature
#' @param eggP the minimum provision before eggs are mature
#' @param energyPreG pre-gonotrophic energy requirement
#' @param Pathogen name of the PATHOGEN module to use, can be in "PfSI", "PfMOI"
#' @return a named list of parameters
#' @examples
#' P = MBITES.BRO.Parameters()
#' @export
MBITES.BRO.Parameters <- function(

  ##########################################
  # Landing Spot
  ##########################################

  B_wts = rep(1,6),
  R_wts = rep(1,6),
  O_wts = rep(1,6),
  InAndOut = matrix(
    c(c(4,2,1,0,1),
      c(2,1,1,0,1),
      c(2,1,1,0,2),
      c(1,1,1,0,1),
      c(6,4,2,1,0)
    ), byrow=FALSE, ncol=5),

  ##########################################
  # Blood Feeding Attempt
  ##########################################
  bfa.p = .98,  # Blood Feed Attempt . Prob Survives (Base)
  bfa.s = .3,   # Blood Feed Attempt . Prob Succeeds
  B_time = 2/3,  # Blood Feed Attempt . Mean Time Elapsed (in Days)
  Q = 0.9,      # Proportion of Bites on Humans

  ##########################################
  # Blood Feeding Search
  ##########################################
  bfs.L = 0.1,  # Blood Feed Search . Prob Leaves Location
  bfs.E = .95,  # Blood Feed Search . Prob Enters New Location
  bfs.p = 0.9,  # Blood Feed Search . Prob Survives
  F_time = 2/24, # Blood Feed Search . Mean Time Elapsed (in Days)

  ##########################################
  # Partial Blood Feeding
  ##########################################
  bms.a = 7.5,  # Blood Meal Size, a
  bms.b = 2.5,  # Blood Meal Size, b
  bms.z = .01,  # Blood Feeding Hazard

  ##########################################
  # ReFeed
  ##########################################
  rf.a=18,
  rf.b=10000,

  ##########################################
  # Overfeed
  ##########################################
  of.a=5,
  of.b=500,

  ##########################################
  # Post Prandial Resting
  ##########################################
  ppr.p = .9,   # Resting . Prob Survives (Base)
  R_time = 0.8,  # Resting . Mean Time Elapsed (in Days)
  reFeed = 0.2,

  ##########################################
  # Egg Laying Attempt
  ##########################################
  ela.p = .9,   # Egg Laying Attempt . Prob Survives (Base)
  ela.s = .7,   # Egg Laying Attempt . Prob Success
  O_time = 1,    # Egg Laying Attempt . Mean Time Elapsed

  ##########################################
  # Egg Laying Search
  ##########################################
  els.p = .9,   # Egg Laying Search . Prob Survives (Base)
  L_time = .3,   # Egg Laying Search . Mean Time Elapsed
  els.a = .95,  # Egg Laying Search . Prob Accepts

  ##########################################
  # SENESCE
  ##########################################
  sns.a   =   0.1,
  sns.b   = 100,

  ##########################################
  # SUGAR
  ##########################################
  sgr.d     =   1/10, # 0 = off
  sgr.p   =   0.95,
  sgr.t   =   1/24,
  sgr.a     =  50,
  sgr.b     =  10,
  sgr.sa    =  50,
  sgr.sb    = 200,
  sgr.sz    =   1,
  sgr.cvr   =  0.2,

  ##########################################
  # TATTER
  ##########################################
  ttsz.p =   0.5,
  ttsz.a =   5,
  ttsz.b =  95,
  ttr.a  =  10,
  ttr.b  = 500,

  ##########################################
  # REPEL
  ##########################################
  rpl.cvr = 0,
  rpl.rpl = 0,
  rpl.die = 0,

  eggT = 0,
  eggP = 0,
  energyPreG = 0,
  Pathogen = "PfSI"

){

  return(list(

    B_wts = B_wts, R_wts = R_wts, O_wts = O_wts,

    ##########################################
    # Blood Feeding Attempt
    ##########################################
    bfa.s=bfa.s, bfa.p=bfa.p, B_time=B_time, Q=Q,

    ##########################################
    # Blood Feeding Search
    ##########################################
    bfs.L=bfs.L, bfs.E=bfs.E, bfs.p=bfs.p, F_time=F_time,

    ##########################################
    # Blood Meal Size
    ##########################################
    bms.a=bms.a, bms.b=bms.b, bms.z=bms.z,

    ##########################################
    # Refeed
    ##########################################
    rf.a=rf.a, rf.b=rf.b,

    ##########################################
    # Overfeeding
    ##########################################
    of.a=of.a, of.b=of.b,

    ##########################################
    # Post Prandial Resting
    ##########################################
    R_time=R_time, ppr.p=ppr.p, reFeed=reFeed,

    ##########################################
    # Egg Laying Attempt
    ##########################################
    ela.p=ela.p, ela.s=ela.s, O_time=O_time,

    ##########################################
    # Egg Laying Search
    ##########################################
    els.p=els.p, L_time=L_time, els.a=els.a,

    ##########################################
    # Sugar Feeding
    ##########################################
    sgr.d=sgr.d, sgr.p=sgr.p, sgr.a=sgr.a, sgr.b=sgr.b,
    sgr.sa = sgr.sa, sgr.sb = sgr.sb, sgr.sz=sgr.sz,
    sgr.cvr=sgr.cvr, sgr.t=sgr.t,

    ##########################################
    #REPELLANT
    ##########################################
    rpl.cvr=rpl.cvr, rpl.rpl=rpl.rpl, rpl.die=rpl.die,

    eggT = eggT,
    eggP = eggP,
    energyPreG = energyPreG,
    Pathogen = Pathogen

  ))

}
