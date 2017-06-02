#################################################################
#
#   MASH
#   R6-ified
#   MICRO Class definition for mosquito
#   David Smith, Hector Sanchez, Sean Wu
#   April 25, 2017
#
#################################################################

#################################################################
# Generic Mosquito Class
#################################################################


#' MICRO Generic Mosquito Class Definition
#'
#' This is a generic Mosquito class definition, it is a superclass for \code{\link{MicroMosquitoFemale}} and \code{\link{MicroMosquitoMale}} and cannot be independently instantiated.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Methods:
#'  * **Constructor**
#'    * none: see \code{\link{MicroMosquitoFemale}} and \code{\link{MicroMosquitoMale}} for inheriting classes
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
#' @section Fields:
#' * **ID and Time**
#'    * id: mosquito ID (integer)
#'    * bDay: time of emergence (numeric)
#'    * tNow: time of current behavioral state (numeric)
#'    * tNext: time to next behavioral state change (numeric)
#'    * genotype: genotype of mosquito (integer)
#' * **State and Location**
#'    * state: current behavioral state of mosquito (character)
#'      * F: Blood Feeding Search Bout
#'      * B: Blood Feeding Attempt Bout
#'      * R: Post-Prandial Resting Bout
#'      * L: Egg Laying Search Bout
#'      * O: Egg Laying Attempt Bout
#'      * S: Sugar Feeding Attempt Bout
#'      * M: Female Mating Bout
#'      * E: Estivation Bout
#'      * D: Death
#'    * stateNew: next behavioral state of mosquito (see above)
#'    * inPointSet: class of site (character)
#'      * f: feeding site \code{\link{FeedingSite}}
#'      * l: aquatic habitat \code{\link{AquaticSite}}
#'      * s: sugar feeding site \code{\link{SugarSite}}
#'      * m: mating site \code{\link{MatingSite}}
#'    * ix: index of site (integer)
#'    * MATURE: mature (logical)
#'    * ALIVE: alive or dead? (logical)
#' * **Other State Fields**
#'    * lspot: landing spot (character)
#'      * l: Leave the area
#'      * r: Reattempt Without Resting
#'      * v: Rest on vegetation
#'      * w: Rest on the Outside wall of a structure
#'      * i: Rest on the Inside wall of a structure
#'    * damage: wing tattering (numeric)
#'    * energy: energy reserves (numeric)
#'    * history: list; see \code{\link{MosquitoFemaleHistory}}
#' * **Egg Production**
#'    * bmSize: the size of the blood meal, relative to max
#'    * batch: female eggs in batch
#'    * eggT: the minimum time before eggs are mature
#'    * eggP: the mimimum provision for eggs to mature
#' * **Maturation & Reproduction**
#'    * sire: ID of mate
#'    * energyPreG: pre-gonotrophic energy requirement
#'    * hostID: the id of the host: -1 zoo, 0 null host, otherwise human ID
#' * **Pathogen Module Specific Fields**
#'    * Pathogen: Module specific Pathogen object
#'      * PfSI: \code{\link{mosquitoPfSI}}
#' @section Methods:
#'  * **Constructor**
#'    * new: initialize a new \code{MicroMosquitoFemale} object
#'      * Arguments:
#'        * arg1: something.
#'  * **Getters & Setters**
#'    * get_id:
#'  * **Pointers**
#'    * get_FemalePopPointer: get pointer to enclosing \code{\link{MicroMosquitoPopFemale}}
#'    * set_FemalePopPointer: set pointer to enclosing \code{\link{MicroMosquitoPopFemale}}
#'    * get_MalePopPointer: get pointer to \code{\link{MicroMosquitoPopMale}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * set_MalePopPointer: set pointer to \code{\link{MicroMosquitoPopMale}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
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
MicroMosquitoFemale <- R6::R6Class(classname = "MicroMosquitoFemale",
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

                              # with(PAR,{
                              #
                              #   ##############################################################
                              #   # general moquito parameters
                              #   ##############################################################
                              #
                              #   private$id        = id        # mosquito id
                              #   private$bDay      = time        # time of emergence
                              #   private$tNow      = time        # time of last event
                              #   private$tNext     = time        # time to next event
                              #   private$genotype  = genotype        # genotype of mosquito
                              #   private$state      = state       # {F,B,R,L,O,S,M,E,D}
                              #   private$stateNew   = state       # {F,B,R,L,O,S,M,E,D}
                              #   private$inPointSet = inPointSet       # class of site {f,l,s,m}
                              #   private$ix         = ix       # index of site
                              #   private$mature     = FALSE       # mature
                              #   private$lspot     = NULL        # landing spot (i: inside wall, w: outside wall, v: outside vegetation, r: feed, l: leave)
                              #   private$damage    = 0        # wing tattering
                              #   private$energy    = 1        # energy reserves
                              #
                              #   ##############################################################
                              #   # female-specific parameters
                              #   ##############################################################
                              #
                              #   # Egg Batch Variables
                              #   private$bmSize = 0         # the size of the blood meal, relative to max
                              #   private$batch  = 0         # female eggs in batch
                              #   private$eggT   = 0         # the minimum time before eggs are mature
                              #   private$eggP   = 0         # the mimimum provision for eggs to mature
                              #
                              #   # Maturation & Reproduction
                              #   private$mated       = FALSE
                              #   private$sire        = 0L
                              #   private$energyPreG  = energyPreG  # pre-gonotrophic energy requirement
                              #
                              #   # Infection events
                              #   private$hostID  = 0L          # the id of the host: -1::none; 0::not human
                              #   private$EIP     = EIP        # presence/absence of sporozoites
                              #
                              #   ##############################################################
                              #   # initialize event history
                              #   ##############################################################
                              #
                              #   private$history = list(
                              #     stateH     = NULL,      # state trajectory
                              #     timeH      = NULL,      # transition times
                              #     ixH        = ix,      # sites visited
                              #     pSetH      = inPointSet,    # point sets visited
                              #
                              #     feedAllH   = 0L,      # number of blood meals
                              #     feedAllT   = NULL,   # times of blood meals
                              #     feedHumanH = 0L,      # number of blood meals on human hosts
                              #     feedHumanT = NULL,   # times of blood meals on human hosts
                              #     feedIxH    = NULL,   # ids of all blood hosts
                              #
                              #     bmSizeH    = NULL,
                              #     batchH     = NULL
                              #   )
                              #
                              #   private$bionomics = list(
                              #     mBatch = 0L, # mean egg batch size
                              #     tBatch = 0L, # total egg production
                              #     feedAllH = 0L, # total number of bloodmeals
                              #     feedHumanH = 0L, # number of human bloodmeals
                              #     bmInt = 0L, # all bloodmeal intervals
                              #     bmIntH = 0L, # human bloodmeal intervals
                              #     lifespan = 0L # lifespan
                              #   )
                              #
                              # })

                            }, # end initializer

                            ##############################################################
                            # Pointers
                            ##############################################################

                            # MicroMosquitoPopFemale
                            get_FemalePopPointer = function(){
                              return(private$FemalePopPointer)
                            },
                            set_FemalePopPointer = function(FemalePopPointer){
                              private$FemalePopPointer = FemalePopPointer
                            },

                            # MicroMosquitoPopMale
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
                            sire        = 0,
                            energyPreG  = NULL,  # pre-gonotrophic energy requirement
                            hostID        = 0,           # the id of the host: -1::none; 0::not human


                            # Infection events
                            EIP           = NULL,        # length of extrinstic incubation period
                            Pathogen      = NULL,

                            # Pointers

                            FemalePopPointer = NULL,  # Point to enclosing MicroMosquitoPopFemale
                            MalePopPointer = NULL,    # Point to MicroMosquitoPopMale in the same microsimulation Tile
                            LandscapePointer = NULL,  # Point to Landscape object in same microsimulation Tile
                            HumansPointer = NULL,     # Point to HumanPop object in same microsimulation Tile
                            TilePointer = NULL        # Point to enclosing microsimulation Tile

                          )

)


#################################################################
# Male Mosquito Class
#################################################################

#' MICRO Male Mosquito Class Definition
#'
#' This is a male mosquito class definition for MICRO; it inherits (superclass) from \code{\link{MicroMosquito}}.
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Fields:
#' * **ID and Time**
#'    * id: mosquito ID (integer)
#'    * bDay: time of emergence (numeric)
#'    * tNow: time of current behavioral state (numeric)
#'    * tNext: time to next behavioral state change (numeric)
#'    * genotype: genotype of mosquito (integer)
#' * **State and Location**
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
MicroMosquitoMale <- R6::R6Class(classname = "MicroMosquitoMale",
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




#################################################################
# History and Bionomic Objects
#################################################################

#' \code{\link{MicroMosquitoFemale}} History Object
#'
#' Generate the history object for \code{\link{MicroMosquitoFemale}}
#'
#' @param stateH state trajectory
#' @param timeH transition times
#' @param ixH sites visited (initialized to site of emergence)
#' @param pSetH point sets visited (initialized to site of emergence)
#' @param feedAllH number of blood meals
#' @param feedAllT times of blood meals
#' @param feedHumanH number of blood meals on human hosts
#' @param feedHumanT times of blood meals on human hosts
#' @param feedIxH ids of all blood hosts
#' @param bmSizeH blood meal sizes
#' @param batchH size of egg batch
#' @return list
#' @examples
#' MosquitoFemaleHistory(ixH = 0L, pSetH = "l")
MosquitoFemaleHistory <- function(
    stateH = NULL,
    timeH = NULL,
    ixH,
    pSetH,
    feedAllH = 0L,
    feedAllT = NULL,
    feedHumanH = 0L,
    feedHumanT = NULL,
    feedIxH = NULL,
    bmSizeH = NULL,
    batchH = NULL
  ){
    list(
      stateH     = stateH,
      timeH      = timeH,
      ixH        = ixH,
      pSetH      = pSetH,
      feedAllH   = feedAllH,
      feedAllT   = feedAllT,
      feedHumanH = feedHumanH,
      feedHumanT = feedHumanT,
      feedIxH    = feedIxH,
      bmSizeH    = bmSizeH,
      batchH     = batchH
      )
}


#' \code{\link{MicroMosquitoMale}} History Object
#'
#' Generate the history object for \code{\link{MicroMosquitoMale}}
#'
#' @param stateH state trajectory
#' @param timeH transition times
#' @param ixH sites visited (initialized to site of emergence)
#' @param pSetH point sets visited (initialized to site of emergence)
#' @return list
#' @examples
#' MosquitoMaleHistory(ixH = 0L, pSetH = "l")
MosquitoMaleHistory <- function(
    stateH = NULL,
    timeH = NULL,
    ixH,
    pSetH
  ){
    list(
      stateH     = stateH,
      timeH      = timeH,
      ixH        = ixH,
      pSetH      = pSetH
      )
}
