#################################################################
#
#   MASH
#   MICRO Mosquito Populations
#   Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   July 19, 2017
#
#################################################################

#' Initialize MICRO Mosquito Population Parameters
#'
#' Generate a list of parameters to initialize \code{\link{MicroMosquitoPopFemale}} and (optionally) \code{\link{MicroMosquitoPopMale}} for initialization of
#' mosquito populations in a microsimulation tile \code{\link{MicroTile}}.
#'
#' @param module which M-BITES module to use (must be a character in "BRO","BROM","BROS","BROMS","FULL")
#' @param N_female number of female mosquitoes
#' @param N_male number of male mosquitoes
#' @param time initial time to start populations
#' @param ix_female vector of starting location indices for females (indices of \code{\link{AquaticSite}} they emerge from)
#' @param ix_male vector of starting location indices for males (indices of \code{\link{AquaticSite}} they emerge from)
#' @param genotype_female vector of genotpes for females
#' @param genotype_male vector of genotpes for males
#' @param ... additional named parameters to be passed to the specific M-BITES parameters function
#'  * MBITES-BRO: pass to \code{\link{MBITES.BRO.Parameters}}
#' @return return a list
#' @examples
#' MicroMosquitoPop.Parameters()
#' @md
#' @export
MicroMosquitoPop.Parameters <- function(
    module,
    N_female,
    N_male = NULL,
    time = 0,
    ix_female,
    ix_male = NULL,
    genotype_female,
    genotype_male = NULL,
    ...
  ){

    MosquitoPop_PAR = list()

    MosquitoPop_PAR$module = module
    MosquitoPop_PAR$N_female = N_female
    MosquitoPop_PAR$N_male = N_male
    MosquitoPop_PAR$time = time
    MosquitoPop_PAR$ix_female = ix_female
    MosquitoPop_PAR$ix_male = ix_male
    MosquitoPop_PAR$genotype_female = genotype_female
    MosquitoPop_PAR$genotype_male = genotype_male

    # make MBITES parameters
    MosquitoPop_PAR$MBITES_PAR = switch(module,
        BRO = {MBITES.BRO.Parameters(...)},
        BROS = {print("havent written")},
        BROM = {print("havent written")},
        BROMS = {print("havent written")},
        FULL = {print("havent written")},
        {stop("unrecognized M-BITES module")}
      )

      return(MosquitoPop_PAR)
}
