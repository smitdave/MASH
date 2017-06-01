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

MosquitoPop <- R6Class(classname = "MosquitoPop",
                    portable = TRUE,
                    cloneable = FALSE,
                    lock_class = FALSE,
                    lock_objects = FALSE,

                    public = list(

                    ),

                    private = list(

                      pop = NULL,  # mosquito population
                      MvOb = NULL  # movement object

                    )
)


#################################################################
# Female Mosquito Population Class
#################################################################

MosquitoPopFemale <- R6Class(classname = "MosquitoPopFemale",
                       inherit = MosquitoPop,
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

MosquitoPopMale <- R6Class(classname = "MosquitoPopMale",
                             inherit = MosquitoPop,
                             portable = TRUE,
                             cloneable = FALSE,
                             lock_class = FALSE,
                             lock_objects = FALSE,

                             public = list(

                             ),

                             private = list(

                             )
)
