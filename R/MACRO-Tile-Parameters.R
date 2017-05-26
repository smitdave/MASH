#################################################################
#
#   MASH
#   R6-ified
#   MACRO MacroTile Class Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   May 26, 2017
#
#################################################################

#' Initialize MACRO Tile Parameters for \code{MacroTile}
#'
#' This is used to generate a list of parameters for \code{\link{MacroTile}} and should be used during its initialization.
#' \code{MACRO.Tile.Parameters} will generate parameters that are passed to the \code{\link[R6]{initialize}} method of
#' the component objects in a tile:
#' * \code{\link{HumanPop}}: creates parameters by calling \code{\{link{HumanPop.Parameters}}
#' * \code{\link{MacroPatch}}: creates parameters by calling \code{\link{MACRO.Patch.Parameters}}
#' * \code{\link{MacroMosquitoPop}}: directly initializes parameters
#' @md
#'
#' @param N number of patches
#' @param patchSize passed to \code{\link{sitePops}}
#' @param patchMin passed to \code{\link{sitePops}}
#' @param more eventually want to pass more params to \code{\link{MACRO.Patch.Parameters}}
#' @return return a list
#' @examples
#' MACRO.Tile.Parameters()
#' @export
MACRO.Tile.Parameters <- function(

  N,
  patchSize = 20,
  patchMin = 10

  ){

    demographics = sitePops(N=N,siteSize=patchSize,siteMin=patchMin)
    HumanPop_PAR = HumanPop.Parameters(nSite = N, demographics = demographics)

    patch_hhID_helper = rle(x = demographics$homeHumanID)
    patch_hhID = mapply(FUN = function(x,y){
        rep(x = val,times=y)
      },x=patch_hhID_helper$values,y=patch_hhID_helper$lengths)
    MacroPatch_PAR = MACRO.Patch.Parameters(N=N, hhID=patch_hhID, humanIDs=demographics$siteHumanID)

    MacroMosquitoPop_PAR = MACRO.MosquitoPop.Parameters(N=N)

  return(
    list(
      N = N,
      HumanPop_PAR = HumanPop_PAR,
      MacroPatch_PAR = MacroPatch_PAR,
      MacroMosquitoPop_PAR = MacroMosquitoPop_PAR
    )
  )
}
