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
                       initialize = function(PfID = NULL, tInf = NULL, MOI = 0L, damID = NULL, sireID = NULL){
                         private$PfID = PfID
                         private$tInf = tInf
                         private$MOI = MOI
                         private$damID = damID
                         private$sireID = sireID
                       },

                       ########################################
                       #  Getters & Setters
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

                       # get_clone: get the mth clonal variant as a list
                       get_clone = function(m){
                         list(
                           PfID = PfID[m],
                           damID = damID[m],
                           sireID = sireID[m]
                          )
                       },

                       ########################################
                       #  Pointers
                       ########################################

                       set_MosquitoPointer = function(MosquitoPointer){
                         private$MosquitoPointer = MosquitoPointer
                       },
                       get_MosquitoPointer = function(){
                         return(private$MosquitoPointer)
                       }


                     ),

                     #private members
                     private = list(

                       # Fields
                       MOI = NULL,
                       PfID = NULL,
                       sireID = NULL,
                       damID = NULL,

                       # Pointers
                       MosquitoPointer = NULL


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
#' @section Fields:
#'  * **History**
#'    * tEvent: time of the event (jump time for embedded CTMC)
#'    * events: the new state (state after the jump)
#'    * MOI: current multiplicity of infection
#'
#' @md
#' @export
humanPfMOI <- R6::R6Class(classname="humanPfMOI",
                     portable = TRUE,
                     cloneable = FALSE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #initialize
                       initialize = function(PfID = NULL, tInf = NULL, MOI = 0L, damID = NULL, sireID = NULL){
                         private$PfID = PfID
                         private$tInf = tInf
                         private$MOI = MOI
                         private$damID = damID
                         private$sireID = sireID


                        #  # Pathogen and immune states
                        #  MOI = NULL, # my multiplicity of infection
                        #  PfID = NULL, # vector of PfID
                        #  b = NULL, # infected mosquito to human transmission efficiency
                        #  c = NULL, # infected human to mosquito transmission efficiency
                        #  chemoprophylaxis = NULL,
                        #  history = NULL,
                         #
                        #  # Pointers
                        #  HumanPointer = NULL
                       },

                       ########################################
                       #  Infection Dynamics
                       ########################################

                       # add a new infection
                       add_Infection = function(PfID){
                         private$PfID = c(private$PfID,PfID)
                         private$MOI = private$MOI + 1L
                       },

                       # completely clear the infection assoc. with index ix
                       clear_Infection = function(ix){
                         private$PfID = private$PfID[-ix]
                         private$MOI = private$MOI - 1L
                       },

                       ########################################
                       #  Getters & Setters
                       ########################################

                       # MOI: Multiplicity of Infection
                       get_MOI = function(){
                         return(private$MOI)
                       },
                       set_MOI = function(MOI){
                         private$MOI = MOI
                       },
                       increment_MOI = function(){
                         private$MOI = private$MOI + 1L
                       },

                       # PfID: Pf IDs; indicate liver-stage infections
                       get_PfID = function(){
                         return(private$PfID)
                       },
                       set_PfID = function(PfID){
                         private$PfID = PfID
                       },
                       push_PfID = function(PfID){
                         private$PfID = c(private$PfID,PfID)
                       },

                       get_chemoprophylaxis = function(){
                         return(private$chemoprophylaxis)
                       },
                       set_chemoprophylaxis = function(chemoprophylaxis){
                         private$chemoprophylaxis = chemoprophylaxis
                       },


                       get_clone = function(m){
                         print("get the mth clonal variant as a list")
                         list(

                          )
                       },

                       ########################################
                       #  History
                       ########################################

                       track_history = function(eventT , event){
                         print("sean hasn't written track history yet for PfMOI")
                         private$history$MOI = private$MOI
                       },

                       ########################################
                       #  Pointers
                       ########################################

                       # HumanPointer: point to the human I exist in!
                       get_HumanPointer = function(){
                         return(private$HumanPointer)
                       },
                       set_HumanPointer = function(HumanPointer){
                         private$HumanPointer = HumanPointer
                       }


                     ),

                     #private members
                     private = list(

                       # Pathogen and immune states
                       MOI = NULL, # my multiplicity of infection
                       PfID = NULL, # vector of PfID
                       tInf = NULL, # vector of infection times
                       b = NULL, # infected mosquito to human transmission efficiency
                       c = NULL, # infected human to mosquito transmission efficiency
                       chemoprophylaxis = NULL,
                       history = NULL,

                       # Pointers
                       HumanPointer = NULL

                     )

) #end class definition
