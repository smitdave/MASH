#################################################################
#
#   MASH
#   R6-ified
#   MICRO Class definition for mosquito population
#   David Smith, Hector Sanchez, Sean Wu
#   April 28, 2017
#
#################################################################


#################################################################
# Generic Mosquito Population Class
#################################################################

#' MICRO Generic Mosquito Population Class Definition
#'
#' This is a generic Mosquito Population class definition, it is a superclass for \code{\link{MicroMosquitoPopFemale}} and \code{\link{MicroMosquitoPopMale}} and cannot be independently instantiated.
#' It contains methods and fields which are generic to both the male and female populations, described below.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Fields:
#' * **Fields**
#'    * pop: list of \code{\link{MicroMosquitoFemale}} or \code{\link{MicroMosquitoMale}} objects
#'    * MvOb: Movement object WRITE ME
#'    * MBITES_PAR: M-BITES parameters; see \code{\link{MBITES.Parameters}}
#' * **Pointers**
#'    * LandscapePointer: pointer to \code{\link{Landscape}} object in enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * HumansPointer: pointer to \code{\link{HumanPop}} object in enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * TilePointer: pointer to enclosing \code{\link{MicroTile}}
#' @section Methods:
#'  * **Getters & Setters**
#'    * get_MosquitoIxM: get mosquito corresponding to position \code{IxM} in list (or return all if argument \code{ixM = NULL})
#'    * get_MosquitoID: get mosquito with private id corresponding to argument \code{ID}
#'  * **Pointers**
#'    * get_LandscapePointer: get pointer to \code{\link{Landscape}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * set_LandscapePointer: set pointer to \code{\link{Landscape}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * get_HumansPointer: get pointer to \code{\link{HumanPop}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * set_HumansPointer: set pointer to \code{\link{HumanPop}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * get_TilePointer: get pointer to enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * set_TilePointer: get pointer to enclosing microsimulation Tile \code{\link{MicroTile}}
#' @md
#' @export
MicroMosquitoPop <- R6::R6Class(classname = "MosquitoPop",
                    portable = TRUE,
                    cloneable = FALSE,
                    lock_class = FALSE,
                    lock_objects = FALSE,

                    public = list(

                      # Getters & Setters
                      get_MosquitoIxM = function(ixM = NULL){
                        if(is.null(ixM)){
                          return(private$pop)
                        } else {
                          return(private$pop[[ixM]])
                        }
                      },
                      get_MosquitoID = function(ID){
                        stop("this hasn't been written yet")
                      },

                      # Pointers
                      get_LandscapePointer = function(){
                        return(private$LandscapePointer)
                      },
                      set_LandscapePointer = function(LandscapePointer){
                        private$LandscapePointer = LandscapePointer
                      },

                      get_HumansPointer = function(){
                        return(private$HumansPointer)
                      },
                      set_HumansPointer = function(HumansPointer){
                        private$HumansPointer = HumansPointer
                      },

                      get_TilePointer = function(){
                        return(private$TilePointer)
                      },
                      set_TilePointer = function(TilePointer){
                        private$TilePointer = TilePointer
                      }

                    ),

                    private = list(

                      # Fields
                      pop = NULL,        # mosquito population
                      MvOb = NULL,       # movement object
                      MBITES_PAR = NULL, # MBITES Parameters

                      # Pointers
                      LandscapePointer = NULL,  # Point to Landscape object in same microsimulation Tile
                      HumansPointer = NULL,     # Point to HumanPop object in same microsimulation Tile
                      TilePointer = NULL        # Point to enclosing microsimulation Tile

                    )
)


#################################################################
# Female Mosquito Population Class
#################################################################

MicroMosquitoPopFemale <- R6::R6Class(classname = "MicroMosquitoPopFemale",
                       inherit = MicroMosquitoPop,
                       portable = TRUE,
                       cloneable = FALSE,
                       lock_class = FALSE,
                       lock_objects = FALSE,

                       public = list(

                         # initializer
                         # initialize = function(N, )

                       ),

                       private = list(

                       )
)


#################################################################
# Male Mosquito Population Class
#################################################################

MicroMosquitoPopMale <- R6::R6Class(classname = "MicroMosquitoPopMale",
                             inherit = MicroMosquitoPop,
                             portable = TRUE,
                             cloneable = FALSE,
                             lock_class = FALSE,
                             lock_objects = FALSE,

                             public = list(

                             ),

                             private = list(

                             )
)
