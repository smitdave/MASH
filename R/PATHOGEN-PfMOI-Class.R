#################################################################
#
#   MASH-R6
#   PATHOGEN component
#   PfMOI module R6 class definitions
#   David Smith, Hector Sanchez, Sean Wu
#   June 9, 2017
#
#################################################################


#################################################################
# Mosquito Stage
#################################################################

#' PfMOI Mosquito-stage Pathogen Class Definition
#'
#' This is the mosquito-stage PfMOI module pathogen object defined in \code{\link{MicroMosquitoFemale}}.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Methods:
#'  * **Constructor**
#'    * new: initialize a new \code{Landscape} object
#'      * Arguments:
#'        * \code{Landscape_PAR}: see \code{\link{Landscape.Parameters}} for parameter generation function.
#'  * **Getters & Setters**
#'    * get_XX: get \code{private$XX}
#'      * Arguments:
#'        * \code{ixS}: if \code{NULL} return all sites, otherwise return site indexed by \code{ixS}
#'    * set_FeedingSites: set \code{private$FeedingSites}
#'      * Arguments:
#'        * \code{FeedingSites}: replacement object (see below)
#'        * \code{ixS}: if \code{NULL} replace all sites, otherwise replace site indexed by \code{ixS}
#'  * **Pointers**
#'    * get_TilePointer: get \code{\link{MicroTile}} pointer
#'    * set_TilePointer: set \code{\link{MicroTile}} pointer
#'
#'
#'
#'
#'
#' @md
#' @export
mosquitoPfMOI <- R6::R6Class(classname="mosquitoPfMOI",
                     portable = TRUE,
                     cloneable = FALSE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #initialize
                       initialize = function(PfID = NULL, tInf = NULL, spz = 0L, damID = NULL, sireID = NULL){
                         private$PfID = PfID
                         private$tInf = tInf
                         private$spz = spz
                         private$damID = damID
                         private$sireID = sireID
                       },

                       ########################################
                       #  Accessors, Pointers, and Setters
                       ########################################

                       # MOI: Multiplicity of Infection
                       get_MOI = function(){
                         return(private$MOI)
                       },
                       set_MOI = function(MOI){
                         private$MOI = MOI
                       },
                       increment_MOI = function(){
                         private$MOI = private$MOI + 1
                       },

                       get_clone = function(m){
                         print("get the mth clonal variant as a list")
                         list(

                          )
                       }




                     ),

                     #private members
                     private = list(

                       MOI = NULL


                     )

) #end class definition


#################################################################
# Human Stage
#################################################################

#' PfMOI Human-stage Pathogen Class Definition
#'
#' This is the human-stage PfMOI module pathogen object defined in \code{\link{Human}}.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Methods:
#'  * **Constructor**
#'    * new: initialize a new \code{Landscape} object
#'      * Arguments:
#'        * \code{Landscape_PAR}: see \code{\link{Landscape.Parameters}} for parameter generation function.
#'  * **Getters & Setters**
#'    * get_XX: get \code{private$XX}
#'      * Arguments:
#'        * \code{ixS}: if \code{NULL} return all sites, otherwise return site indexed by \code{ixS}
#'    * set_FeedingSites: set \code{private$FeedingSites}
#'      * Arguments:
#'        * \code{FeedingSites}: replacement object (see below)
#'        * \code{ixS}: if \code{NULL} replace all sites, otherwise replace site indexed by \code{ixS}
#'  * **Pointers**
#'    * get_TilePointer: get \code{\link{MicroTile}} pointer
#'    * set_TilePointer: set \code{\link{MicroTile}} pointer
#'
#'
#'
#'
#'
#' @md
#' @export
humanPfMOI <- R6::R6Class(classname="mosquitoPfMOI",
                     portable = TRUE,
                     cloneable = FALSE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #initialize
                       initialize = function(PfID = NULL, tInf = NULL, spz = 0L, damID = NULL, sireID = NULL){
                         private$PfID = PfID
                         private$tInf = tInf
                         private$spz = spz
                         private$damID = damID
                         private$sireID = sireID
                       },

                       ########################################
                       #  Accessors, Pointers, and Setters
                       ########################################

                       # MOI: Multiplicity of Infection
                       get_MOI = function(){
                         return(private$MOI)
                       },
                       set_MOI = function(MOI){
                         private$MOI = MOI
                       },
                       increment_MOI = function(){
                         private$MOI = private$MOI + 1
                       },

                       get_clone = function(m){
                         print("get the mth clonal variant as a list")
                         list(

                          )
                       }




                     ),

                     #private members
                     private = list(

                       MOI = NULL, # my multiplicity of infection
                       PfID = NULL, # vector of PfID
                       b = NULL, # infected mosquito to human transmission efficiency
                       c = NULL, # infected human to mosquito transmission efficiency
                       chemoprophylaxis = NULL,
                       history = NULL

                     )

) #end class definition
