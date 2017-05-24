#################################################################
#
#   MASH
#   R6-ified
#   Human Biting-related Methods for MACRO
#   Sean Wu
#   May 23, 2017
#
#################################################################


#################################################################
# expectedBites: how much do I expect to be bitten at a patch?
#################################################################

#' Get \code{Human} myEIR
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
get_myEIR <- function(){
  return(private$myEIR)
}

#' Set \code{Human} myEIR
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
set_myEIR <- function(myEIR){
  private$myEIR = myEIR
}

#' MACRO \code{Human} Method: expectedBites
#'
#' Write me! a method for \code{\link{Human}}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
expectedBites <- function(){
  here = self$get_location() # where am i now?
  newEIR = self$get_biteWeight() * (self$get_MosquitoPointer()$get_f()*self$get_MosquitoPointer()$get_Z(here)) / (self$get_PatchesPointer()$get_bWeightHuman(here) + self$get_PatchesPointer()$get_bWeightZoo(here) + self$get_PatchesPointer()$get_bWeightZootox(here))
  self$set_myEIR(newEIR)
}


# expectedBites = function(ixH){
#   here = HUMANS[[ixH]]$loc
#   myEIR =
# HUMANS[[ixH]]$w*with(LANDSCAPE$MPop,{f*Z[here]})/with(LANDSCAPE, w.human[here]+w.zoo[here]+w.zootox[here])
# }
#
#
# sumKappa = function(ixH){
#   here = HUMANS[[ixH]]$loc
#   LANDSCAPE$kappa[here] <<- LANDSCAPE$kappa[here] + HUMANS[[ixH]]$Pathogens$Pf$c*HUMANS[[ixH]]$w
# }
#
# addBites2Q = function(ixH, time, N){
#   if(N>0) add2Q_simbitePfSI(ixH, TIME, PAR=Pf0)
# }
#
# updateKappa = function(){
#   for(ixH in 1:nHumans) sumKappa(ixH)
#   LANDSCAPE$kappa <<- with(LANDSCAPE,kappa/(w.human+w.zoo+w.zootox))
# }
#
# queueInfectiousBites= function(){
#   for(ixH in 1:nHumans){
#     nBites = rnbinom(1, mu=expectedBites(ixH), size = .1)
#     addBites2Q(ixH, TIME, nBites)
#   }
# }
