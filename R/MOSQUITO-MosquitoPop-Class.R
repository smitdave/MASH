#################################################################
#
#   MASH
#   R6-ified
#   MICRO Class definition for mosquito population
#   Hector Sanchez & Sean Wu
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
#'    * pop: list of \code{\link{MicroMosquitoFemale}} objects
#'    * MvOb: time of emergence (numeric)
#'    * tNow: time of current behavioral state (numeric)
#'    * tNext: time to next behavioral state change (numeric)
#'    * genotype: genotype of mosquito (integer)
#' * **Pointers**
#'    * state: current behavioral state of mosquito (character)
#'      * M: Male Mating Bout
#'      * S: Sugar Feeding Attempt Bout
#'      * R: Male Resting Bout
#' @section Methods:
#'  * **Constructor**
#'    * new: initialize a new \code{MicroMosquitoMale} object
#'      * Arguments:
#'        * arg1: something.
#'  * **Getters & Setters**
#'    * a getter:
#'  * **Pointers**
#'    * a pointer
#' @md
#' @export
MicroMosquitoPop <- R6::R6Class(classname = "MosquitoPop",
                    portable = TRUE,
                    cloneable = FALSE,
                    lock_class = FALSE,
                    lock_objects = FALSE,

                    public = list(



                    ),

                    private = list(

                      # Fields
                      pop = NULL,       # mosquito population
                      MvOb = NULL,      # movement object
                      MBITES_PAR = NULL # MBITES Parameters

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
