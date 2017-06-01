#################################################################
#
#   MASH
#   R6-ified
#   MICRO Class definition for mosquito
#   Hector Sanchez & Sean Wu
#   April 25, 2017
#
#################################################################

#################################################################
# Generic Mosquito Class
#################################################################


#' MICRO Generic Mosquito Class Definition
#'
#' This is a generic Mosquito class definition, it is a superclass for \code{\link{MosquitoFemale}} and \code{\link{MosquitoMale}} and cannot be independently instantiated.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Methods:
#'  * **Constructor**
#'    * none: see \code{\link{MosquitoFemale}} and \code{\link{MosquitoMale}} for inheriting classes
#'  * **Getters & Setters**
#'    * get_id:
#'  * **Pointers**
#'
#'
#' @md
#' @export
MicroMosquito <- R6::R6Class(classname = "Mosquito",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                 ),

                 # private members
                 private = list(

                   # Biological Fields

                   # ID and time
                   id        = NULL,        # mosquito id
                   bDay      = NULL,        # time of emergence
                   tNow      = NULL,        # time of last event
                   tNext     = NULL,        # time to next event
                   genotype  = NULL,        # genotype of mosquito

                   # State and Location
                   state      = NULL,       # {F,B,R,L,O,S,M,E,D}
                   stateNew   = NULL,       # {F,B,R,L,O,S,M,E,D}
                   inPointSet = NULL,       # class of site {f,l,s,m}
                   ix         = NULL,       # index of site
                   mature     = NULL,       # mature
                   ALIVE      = NULL,       # alive or dead?

                   # Other State Variables
                   lspot     = NULL,        # landing spot (i: inside wall, w: outside wall, v: outside vegetation, r: feed, l: leave)
                   damage    = NULL,        # wing tattering
                   energy    = NULL,        # energy reserves

                   history = list()        # history

                 )
)


#################################################################
# Female Mosquito Class
#################################################################


#' MICRO Female Mosquito Class Definition
#'
#' This is a female mosquito class definition for MICRO; it inherits (superclass) from \code{\link{MicroMosquito}}.
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Methods:
#'  * **Constructor**
#'    * new: initialize a new \code{MosquitoFemale} object
#'      * Arguments:
#'        * arg1: something.
#'  * **Getters & Setters**
#'    * get_id:
#'  * **Pointers**
#'    * get_FemalePopPointer: get pointer to enclosing \code{\link{MosquitoPopFemale}}
#'    * set_FemalePopPointer: set pointer to enclosing \code{\link{MosquitoPopFemale}}
#'    * get_MalePopPointer: get pointer to \code{\link{MosquitoPopMale}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * set_MalePopPointer: set pointer to \code{\link{MosquitoPopMale}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * get_LandscapePointer: get pointer to \code{\link{Landscape}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * set_LandscapePointer: set pointer to \code{\link{Landscape}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * get_HumansPointer: get pointer to \code{\link{HumanPop}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * set_HumansPointer: set pointer to \code{\link{HumanPop}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * get_TilePointer: get pointer to enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * set_TilePointer: set pointer to enclosing microsimulation Tile \code{\link{MicroTile}}
#'
#'
#' @md
#' @export
MosquitoFemale <- R6::R6Class(classname = "MosquitoFemale",
                          inherit = MicroMosquito,
                          portable = TRUE,
                          cloneable = FALSE,
                          lock_class = FALSE,
                          lock_objects = FALSE,
                          class = FALSE,

                          # public members
                          public = list(

                            ##############################################################
                            # Initializer
                            ##############################################################

                            initialize = function(PAR, id, time, ix, genotype = 1L, state = "M", inPointSet = "l"){

                              with(PAR,{

                                ##############################################################
                                # general moquito parameters
                                ##############################################################

                                private$id        = id        # mosquito id
                                private$bDay      = time        # time of emergence
                                private$tNow      = time        # time of last event
                                private$tNext     = time        # time to next event
                                private$genotype  = genotype        # genotype of mosquito
                                private$state      = state       # {F,B,R,L,O,S,M,E,D}
                                private$stateNew   = state       # {F,B,R,L,O,S,M,E,D}
                                private$inPointSet = inPointSet       # class of site {f,l,s,m}
                                private$ix         = ix       # index of site
                                private$mature     = FALSE       # mature
                                private$lspot     = NULL        # landing spot (i: inside wall, w: outside wall, v: outside vegetation, r: feed, l: leave)
                                private$damage    = 0        # wing tattering
                                private$energy    = 1        # energy reserves

                                ##############################################################
                                # female-specific parameters
                                ##############################################################

                                # Egg Batch Variables
                                private$bmSize = 0         # the size of the blood meal, relative to max
                                private$batch  = 0         # female eggs in batch
                                private$eggT   = 0         # the minimum time before eggs are mature
                                private$eggP   = 0         # the mimimum provision for eggs to mature

                                # Maturation & Reproduction
                                private$mated       = FALSE
                                private$sire        = 0L
                                private$energyPreG  = energyPreG  # pre-gonotrophic energy requirement

                                # Infection events
                                private$hostID  = 0L          # the id of the host: -1::none; 0::not human
                                private$EIP     = EIP        # presence/absence of sporozoites

                                ##############################################################
                                # initialize event history
                                ##############################################################

                                private$history = list(
                                  stateH     = NULL,      # state trajectory
                                  timeH      = NULL,      # transition times
                                  ixH        = ix,      # sites visited
                                  pSetH      = inPointSet,    # point sets visited

                                  feedAllH   = 0L,      # number of blood meals
                                  feedAllT   = NULL,   # times of blood meals
                                  feedHumanH = 0L,      # number of blood meals on human hosts
                                  feedHumanT = NULL,   # times of blood meals on human hosts
                                  feedIxH    = NULL,   # ids of all blood hosts

                                  bmSizeH    = NULL,
                                  batchH     = NULL
                                )

                                private$bionomics = list(
                                  mBatch = 0L, # mean egg batch size
                                  tBatch = 0L, # total egg production
                                  feedAllH = 0L, # total number of bloodmeals
                                  feedHumanH = 0L, # number of human bloodmeals
                                  bmInt = 0L, # all bloodmeal intervals
                                  bmIntH = 0L, # human bloodmeal intervals
                                  lifespan = 0L # lifespan
                                )

                              })

                            }, # end initializer

                            ##############################################################
                            # Pointers
                            ##############################################################

                            # MosquitoPopFemale
                            get_FemalePopPointer = function(){
                              return(private$FemalePopPointer)
                            },
                            set_FemalePopPointer = function(FemalePopPointer){
                              private$FemalePopPointer = FemalePopPointer
                            },

                            # MosquitoPopMale
                            get_MalePopPointer = function(){
                              return(private$MalePopPointer)
                            },
                            set_MalePopPointer = function(MalePopPointer){
                              private$MalePopPointer = MalePopPointer
                            },

                            # Landscape
                            get_LandscapePointer = function(){
                              return(private$LandscapePointer)
                            },
                            set_LandscapePointer = function(LandscapePointer){
                              private$LandscapePointer = LandscapePointer
                            },

                            # HumanPop
                            get_HumansPointer = function(){
                              return(private$HumansPointer)
                            },
                            set_HumansPointer = function(HumansPointer){
                              private$HumansPointer = HumansPointer
                            },

                            # MicroTile
                            get_TilePointer = function(){
                              return(private$TilePointer)
                            },
                            set_TilePointer = function(TilePointer){
                              private$TilePointer = TilePointer
                            }

                          ),

                          # private members
                          private = list(

                            # Biological Fields

                            # Egg Batch Variables
                            bmSize = 0,         # the size of the blood meal, relative to max
                            batch  = 0,         # female eggs in batch
                            eggT   = 0,         # the minimum time before eggs are mature
                            eggP   = 0,         # the mimimum provision for eggs to mature

                            # Maturation & Reproduction
                            mated       = FALSE,
                            sire        = 0,
                            energyPreG  = NULL,  # pre-gonotrophic energy requirement

                            # Infection events
                            hostID  = 0,           # the id of the host: -1::none; 0::not human
                            EIP     = NULL,        # length of extrinstic incubation period
                            Pf      = NULL,

                            # Pointers

                            FemalePopPointer = NULL,  # Point to enclosing MosquitoPopFemale
                            MalePopPointer = NULL,    # Point to MosquitoPopMale in the same microsimulation Tile
                            LandscapePointer = NULL,  # Point to Landscape object in same microsimulation Tile
                            HumansPointer = NULL,     # Point to HumanPop object in same microsimulation Tile
                            TilePointer = NULL        # Point to enclosing microsimulation Tile

                          )

)


#################################################################
# Male Mosquito Class
#################################################################

MosquitoMale <- R6::R6Class(classname = "MosquitoMale",
                        inherit = MicroMosquito,
                        portable = TRUE,
                        cloneable = FALSE,
                        lock_class = FALSE,
                        lock_objects = FALSE,

                        # public members
                        public = list(),

                        # private members
                        private = list()
)