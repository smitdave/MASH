#################################################################
#
#   MASH
#   R6-ified
#   MICRO: Human ActivitySpace Simulation and Methods
#   David Smith, Hector Sanchez, Sean Wu
#   June 1, 2017
#
#################################################################

# NOTE: ALL OF THIS STUFF NEEDS TO GET INITIALIZED TO THE PROPER CLASSES IN
# MICRO.Humans.Setup() akin to MACRO.Humans.Setup()


#' MICRO: Get \code{\link{Human}} ActivitySpace
#'
#' This function is bound to \code{Human$get_ActivitySpace()}
#'
get_MicroHuman_ActivitySpace <- function(){
  return(private$ActivitySpace)
}

#' MICRO: Set \code{\link{Human}} ActivitySpace
#'
#' This function is bound to \code{HumanPop$set_ActivitySpace()}
#'
#' @param nDaily average daily number of other sites visited
#' @param Nplaces number of places visited
#' @param p weight on proportion of time spent at home
#' @param loc vector of other sites visited
#'
set_MicroHuman_ActivitySpace <- function(nDaily, Nplaces, p, loc){
  private$ActivitySpace$nDaily = nDaily
  private$ActivitySpace$Nplaces = Nplaces
  private$ActivitySpace$p = p
  private$ActivitySpace$loc = loc
}


#' MICRO \code{\link{HumanPop}} Method: Initialize Activity Space
#'
#' This function is bound to \code{HumanPop$init_ActivitySpace()}
#'
#' @param nDaily average daily number of other sites visited
#'
init_MicroHumanPop_ActivitySpace <- function(nDaily = 1.4){

}




#' MICRO \code{\link{Human}} Method: Initialize Activity Space
#'
#' This function is bound to \code{Human$set_LandscapePointer()}
#'
#' @param LandscapePointer the R6 \code{\link{Landscape}} object to point to
#'
init_MicroHuman_ActivitySpace <- function(){

}
