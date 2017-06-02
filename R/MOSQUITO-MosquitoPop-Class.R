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
