#################################################################
#
#   MASH
#   R6-ified
#   Human Biting-related Methods for MACRO
#   David Smith, Hector Sanchez, Sean Wu
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


#' MACRO \code{Human} Method: addBites2Q
#'
#' Write me! a method for \code{\link{Human}}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
add2Q_Bites <- function(tEvent, PAR){
  if(PAR$nBites > 0){
    self$add2Q_SimBitePfSI(tEvent = tEvent)
  }
}

#' MACRO \code{HumanPop} Method: queueInfectiousBites
#'
#' Write me! a method for \code{\link{HumanPop}}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
queueInfectiousBites <- function(){
  for(ixH in 1:self$nHumans){
    private$pop[[ixH]]$expectedBites() # update expectedBites (EIR)
    mu = private$pop[[ixH]]$get_myEIR() # my expected EIR
    nBites = rnbinom(n = 1,mu = mu, size = 0.1) # number of bites
    private$pop[[ixH]]$add2Q_Bites(tEvent = self$get_TilePointer()$get_tNow(), PAR = list(nBites = nBites)) # add bites to queue
  }
}
