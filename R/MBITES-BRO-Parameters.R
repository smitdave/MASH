#################################################################
#
#   MASH
#   R6-ified
#   M-BITES BRO
#   Setup Module
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   July 19, 2017
#
#################################################################

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
  # Blood Feeding Bout
  ##########################################
  B_surv = (0.98 * 0.9),
  # bfa.p = .98,  # Blood Feed Attempt . Prob Survives (Base)
  bfa.s = .3,   # Blood Feed Attempt . Prob Succeeds
  B_time = 2/3,  # Blood Feed Attempt . Mean Time Elapsed (in Days)
  Q = 0.9,      # Proportion of Bites on Humans

  ##########################################
  # Blood Feeding Search
  ##########################################
  bfs.L = 0.1,  # Blood Feed Search . Prob Leaves Location
  bfs.E = .95,  # Blood Feed Search . Prob Enters New Location
  # bfs.p = 0.9,  # Blood Feed Search . Prob Survives
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
  R_surv = 0.9,
  # ppr.p = .9,   # Resting . Prob Survives (Base)
  R_time = 0.8,  # Resting . Mean Time Elapsed (in Days)
  reFeed = 0.2,

  ##########################################
  # Egg Laying Attempt
  ##########################################
  O_surv = 0.80,
  # ela.p = .9,   # Egg Laying Attempt . Prob Survives (Base)
  ela.s = .7,   # Egg Laying Attempt . Prob Success
  O_time = 1,    # Egg Laying Attempt . Mean Time Elapsed

  ##########################################
  # Egg Laying Search
  ##########################################
  # els.p = .9,   # Egg Laying Search . Prob Survives (Base)
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
