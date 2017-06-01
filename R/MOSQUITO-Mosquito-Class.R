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
Mosquito <- R6::R6Class(classname = "Mosquito",
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

                   history = list(),        # history

                   # Pointers
                   MosquitoPopPointer = NULL

                 )
)


#################################################################
# Female Mosquito Class
#################################################################

MosquitoFemale <- R6::R6Class(classname = "MosquitoFemale",
                          inherit = Mosquito,
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

                            } # end initializer

                          ),

                          # private members
                          private = list(

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
                            Pf      = list(nInf = 0, tInf = 0, spz = FALSE, PfM = list())

                          )

)


#################################################################
# Male Mosquito Class
#################################################################

MosquitoMale <- R6::R6Class(classname = "MosquitoMale",
                        inherit = Mosquito,
                        portable = TRUE,
                        cloneable = FALSE,
                        lock_class = FALSE,
                        lock_objects = FALSE,

                        # public members
                        public = list(),

                        # private members
                        private = list()
)
