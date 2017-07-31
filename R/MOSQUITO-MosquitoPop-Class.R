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
#' Mosquito populations should be initialized last in a \code{\link{MicroTile}} because their initializer functions need to take as arguments
#' pointers to the other objects in a tile.
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

                      #################################################################
                      # Getters & Setters
                      #################################################################

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

                      # getter for nullPop
                      get_nullPop = function(){return(private$nullPop)},
                      # update nullPop
                      update_nullPop = function(){
                        private$nullPop = which(vapply(X = private$pop,FUN = is.null,FUN.VALUE = logical(1)))
                      },

                      # generic get_movement method; designed to be overwritten by module-specific parameteric getter
                      get_movement = function(){
                        return(private$movement)
                      },
                      set_movment = function(movement){
                        private$movement = movement
                      },

                      # return parameter list or element of list by name
                      get_MBITES_PAR = function(ixP = NULL){
                        if(is.null(ixP)){
                          return(private$MBITES_PAR)
                        } else {
                          return(private$MBITES_PAR[[ixP]])
                        }
                      },
                      set_MBITES_PAR = function(MBITES_PAR){
                        private$MBITES_PAR = MBITES_PAR
                      },

                      #################################################################
                      # Pointers
                      #################################################################

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
                      pop = NULL,               # mosquito population
                      nullPop = NULL,           # null entries in list for memory allocation
                      movement = NULL,          # movement object (type depends on specific SEARCH module)
                      MBITES_PAR = NULL,        # MBITES Parameters

                      # Pointers
                      LandscapePointer = NULL,  # Point to Landscape object in same microsimulation Tile
                      HumansPointer = NULL,     # Point to HumanPop object in same microsimulation Tile
                      TilePointer = NULL        # Point to enclosing microsimulation Tile

                    )
)


#################################################################
# Female Mosquito Population Class
#################################################################

# MicroMosquitoPopFemale:
MicroMosquitoPopFemale <- R6::R6Class(classname = "MicroMosquitoPopFemale",
                       inherit = MicroMosquitoPop,
                       portable = TRUE,
                       cloneable = FALSE,
                       lock_class = FALSE,
                       lock_objects = FALSE,

                       public = list(

                         ##############################################################
                         # Initializer
                         ##############################################################

                         # N: size of vector to allocate (NOT number of mosquitoes)
                         initialize = function(N, id_init, time_init, ix_init, genotype_init, state_init, MBITES_PAR, movement){

                             # Initialize population level fields prior to allocating container
                             private$MBITES_PAR = MBITES_PAR
                             private$movement = movement

                             # allocate population
                             private$pop = vector(mode="list",length=N)
                             lengths = NULL; lengths[1] = length(id_init); lengths[2] = length(time_init); lengths[3] = length(ix_init); lengths[4] = length(genotype_init); lengths[5] = length(state_init);
                             if(length(unique(lengths)) != 1){
                               stop("one or more of the input vectors to MicroMosquitoPopFemale initializer is not the same length")
                             }
                             if(N < length(id_init)){
                               stop("please set N greater than the initial cohort size")
                             }

                             # allocate initial cohort
                             for(ix in 1:length(id_init)){
                               private$pop[[ix]] = MicroMosquitoFemale$new(id_init[ix], time_init[ix], ix_init[ix], genotype_init[ix], state_init[ix],
                                             MBITES_PAR = MBITES_PAR)
                               private$pop[[ix]]$set_FemalePopPointer(self)
                             }
                             # find NULL indices
                             self$update_nullPop()

                         }, # end initializer

                         ##############################################################
                         # Cohort Methods
                         ##############################################################

                         # push_pop: from a single ImagoSlot; add a cohort.
                         push_pop = function(N, tEmerge, genotype, damID, sireID){
                           if(N > length(nullPop)){
                             self$extend_pop()
                           }
                           for(i in 1:N){
                             private$pop[[private$nullPop[i]]] = MicroMosquitoFemale$new
                           }
                         },

                         # extend_pop: extend the pop vecor
                         extend_pop = function(){

                         }

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
