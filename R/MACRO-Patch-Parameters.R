#################################################################
#
#   MASH
#   R6-ified
#   MACRO patch initialization parameters
#   Sean Wu
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

    aquaModel = "emerge"

  ){

    PAR = list(
        N   = N,
        hhID = hhID,

        bWeightHuman = rep(0,N),
        bWeightZoo   = rgamma(n = N,shape = bWeightZoo1, rate = bWeightZoo1),
        bWeightZootox = rep(0,N),

        Q         = rep(0,N),
        kappa     = rep(0,N),
        humanIDs  = humanIDs,

        #Egg laying
        aquaID        = NULL,
        aquaP         = NULL,
        aquaNewM      = NULL,
        weightAqua    = NULL,   # For modeling movement
        weightOvitrap = NULL

      )

  if(aquaModel == "emerge"){


  } else if(aquaModel == "EL4P"){

  } else {
    stop("aquaModel must be a value in 'emerge' or 'EL4P'!")
  }

}



# hhID      = list(),
#
# # How are infectious bites divided up?
# bWeightHuman   = NULL,
# bWeightZoo     = NULL,
# bWeightZootox  = NULL,
#
# # Net infectiousness
# Q         = NULL,
# kappa     = NULL,
# humanIDs  = list(),
#
# #Egg laying
# aquaID        = NULL,
# aquaP         = NULL,
# aquaNewM      = NULL,
# weightAqua    = NULL,   # For modeling movement
# weightOvitrap = NULL,
#
# #Sugar feeding
# weightSugar   = NULL,
# weightBait    = NULL,
#
# #Mating
# weightMate    = NULL,
#
# # Parasite
# PfTypes = list()
