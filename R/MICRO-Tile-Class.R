#################################################################
#
#   MASH
#   R6-ified
#   Well-mixed Patch Class
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   May 9, 2017
#
#################################################################

#################################################################
# Patch Definition
#################################################################

#' MICRO Tile Class Definition
#'
#' This is a generic MICRO microsimulation tile blah blah ...
#' I talk about something here
#' * somewhere 1
#' * somewhere 2
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Methods:
#'  * **Constructor**
#'    * new: initialize a new \code{MicroTile} object
#'      * Arguments:
#'        * \code{MicroTile_PAR}: see \code{\link{MICRO.Tile.Parameters}} for parameter generation function.
#'  * **Getters & Setters**
#'    * get_tNow:
#'    * set_tNow:
#'    * get_MicroTile_PAR:
#'    * set_MicroTile_PAR:
#'    * get_HumanPop:
#'    * get_Landscape:
#'    * get_FemalePop:
#'    * get_MalePop:
#'
#'
#'
#'
#'
#' @md
#' @export
MicroTile <- R6::R6Class(classname = "MicroTile",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   #################################################
                   # Initialize
                   #################################################

                   initialize = function(MicroTile_PAR){

                     # generate objects
                     private$Landscape = Landscape$new(MicroTile_PAR$Landscape_PAR)
                     private$HumanPop = HumanPop$new(HumanPop_PAR = MicroTile_PAR$HumanPop_PAR)

                     private$tNow = 0
                     private$MicroTile_PAR = MicroTile_PAR

                     # Human & HumanPop Pointers (duplicate for Humans in HumanPop$pop)
                     private$HumanPop$set_TilePointer(self)
                     private$HumanPop$set_LandscapePointer(private$Landscape)

                     # Human & HumanPop initilization
                     for(ixH in 1:private$HumanPop$nHumans){
                       # pointers
                       private$HumanPop$get_Human(ixH)$set_TilePointer(self)
                       private$HumanPop$get_Human(ixH)$set_LandscapePointer(private$Landscape)
                      #  # travel
                      #  private$HumanPop$get_Human(ixH)$set_location(MacroTile_PAR$HumanPop_PAR$homeIDs[ixH])
                      #  private$HumanPop$get_Human(ixH)$set_patchID(MacroTile_PAR$HumanPop_PAR$homeIDs[ixH])
                      #  private$HumanPop$get_Human(ixH)$init_travel(n=2)
                      #  # update baseline human biting weight
                      #  myPatch = private$HumanPop$get_Human(ixH)$get_patchID()
                      #  private$Patches$accumulate_bWeightHuman(bWeightHuman = private$HumanPop$get_Human(ixH)$get_bWeight(), ix = myPatch)
                     }

                     # Landscape Pointers
                     private$Landscape$set_TilePointer(self)
                     private$Landscape$set_HumansPointer(private$HumanPop)

                   },

                   #################################################################
                   # Getters & Setters
                   #################################################################

                   get_tNow = function(){
                     return(private$tNow)
                   },
                   set_tNow = function(tNow){
                     private$tNow = tNow
                   },

                   get_MicroTile_PAR = function(){
                     return(private$tNow)
                   },
                   set_MicroTile_PAR = function(MicroTile_PAR){
                     private$MicroTile_PAR = MicroTile_PAR
                   },

                   get_HumanPop = function(){
                     return(private$HumanPop)
                   },

                   get_Landscape = function(){
                     return(private$Landscape)
                   },

                   get_FemalePop = function(){
                     return(private$FemalePop)
                   },

                   get_MalePop = function(){
                     return(private$MalePop)
                   }

                 ),

                 # private members
                 private = list(

                   # Tile level fields
                   tNow = NULL,
                   MicroTile_PAR = NULL,

                   # objects interacting on a tile
                   HumanPop = NULL,
                   Landscape = NULL,
                   FemalePop = NULL,
                   MalePop = NULL

                 )
)
