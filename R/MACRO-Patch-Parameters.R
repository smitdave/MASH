#################################################################
#
#   MASH
#   R6-ified
#   MACRO MacroPatch Class Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   May 22, 2016
#
#################################################################


#' Initialize MACRO Patch Parameters for \code{MacroPatch}
#'
#' This is used to generate a list of parameters for \code{\link{MacroPatch}} and should be used during its initialization.
#'
#' @param N number of patches
#' @param bWeightZoo1 shape parameter of gamma zoophilic biting weights
#' @param bWeightZoo2 rate parameter of gamma zoophilic biting weights
#' @return return a list
#' @examples
#' MACRO.Patch.Parameters()
#' @export
MACRO.Patch.Parameters <- function(

    ########################################
    #  Parameters
    ########################################

    # number of Patches
    N,

    # houses
    hhID,

    # Biting weights
    bWeightZoo1 = 1,
    bWeightZoo2 = 1,

    humanIDs,

    aquaModel = "emerge",
    ... # named parameters to be passed to specific aquaModel generating function

  ){

    MacroPatch_PAR = list(
        N   = N,
        hhID = hhID,

        bWeightHuman = rep(0,N),
        bWeightZoo   = rgamma(n = N,shape = bWeightZoo1, rate = bWeightZoo1),
        bWeightZootox = rep(0,N),

        Q         = rep(0,N),
        kappa     = rep(0,N),
        humanIDs  = humanIDs,

        #Egg laying
        aquaID        = 1L:N,
        aquaP         = rep(1,N),
        aquaNewM      = rep(0,N),
        weightAqua    = rep(0,N),   # For modeling movement
        weightOvitrap = rep(0,N),

        weightSugar   = rep(0,N),
        weightBait    = rep(0,N),

        weightMate    = rep(0,N)
      )

  if(aquaModel == "emerge"){

    emergeArgs = names(sapply(match.call(), deparse))[-1]
    if(!"lambda" %in% emergeArgs){
      stop("please specify the vector 'lambda' when using the 'Emerge' module of Aquatic Ecology")
    }
    PAR$season = aquaEmerge_makeLambda(...)
    PAR$ImagoQ = rep(0,N)
    PAR$EggQ = rep(0,N)

  } else if(aquaModel == "EL4P"){

    # PAR$something = aquaEL4P_makeMACRO(...)
    stop("sean hasn't written the routines for MACRO EL4P Aquatic Ecology")

  } else {
    stop("aquaModel must be a value in 'emerge' or 'EL4P'")
  }

  return(MacroPatch_PAR)
}
