#################################################################
#
#   MASH
#   R6-ified
#   MACRO MacroTile Class Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   May 11, 2016
#
#################################################################

#' MACRO Tile Class Definition
#'
#' This is a generic MACRO metapopulation tile blah blah ...
#'  below i describe the basic structure of the patch. methods and fields for specific COMPONENTS can be found in:
#' * somewhere 1
#' * somewhere 2
#' @md
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#' @details
#' talk about me in detail!
#' @section public:
#' \itemize{
#'   \item{\code{initialize(N)}}{
#'        talk about me!
#'        }
#'   \item{\code{function()}}{
#'        talk about me!
#'        }
#' }
#' @section private:
#' \itemize{
#'   \item{\code{function()}}{
#'        talk about me!
#'        }
#'   \item{\code{function()}}{
#'        talk about me!
#'        }
#' }
#' @section Active Bindings:
#' \itemize{
#'   \item{\code{coolActiveBinding}}{...}
#' }
#' @export
MacroTile <- R6::R6Class(classname = "MacroTile",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public methods & fields
                 public = list(

                   # class initialize
                   initialize = function(nHum,nPatch){

                     # generate objects
                     private$HumanPop = HumanPop$new(nHum = nHum)
                     private$Patches = Patch$new(N = nPatch)
                     private$MosquitoPop = MacroMosquitoPop$new()

                     # Human & HumanPop Pointers (duplicate for Humans in HumanPop$pop)
                     private$HumanPop$set_TilePointer(self)
                     private$HumanPop$set_MosquitoPointer(private$MosquitoPop)
                     private$HumanPop$set_PatchesPointer(private$Patches)

                     for(ixH in 1:private$HumanPop$nHum){
                       private$HumanPop$get_Human(ixH)$set_TilePointer(self)
                       private$HumanPop$get_Human(ixH)$set_MosquitoPointer(private$MosquitoPop)
                       private$HumanPop$get_Human(ixH)$set_PatchesPointer(private$Patches)
                     }

                     # Patches Pointers
                     private$Patches$set_TilePointer(self)
                     private$Patches$set_MosquitoPointer(private$MosquitoPop)
                     private$Patches$set_HumansPointer(private$HumanPop)

                     # MosquitoPop Pointers
                     private$MosquitoPop$set_TilePointer(self)
                     private$MosquitoPop$set_PatchesPointer(private$Patches)
                     private$MosquitoPop$set_HumansPointer(private$HumanPop)

                   },

                   # oneDayRM
                   oneDayRM = function(){
                     stop("sean hasn't written me yet!")
                   },

                   get_tNow = function(){
                     return(private$tNow)
                   },
                   set_tNow = function(tNow){
                     private$tNow = tNow
                   }

                  ),

                  # private methods & fields
                  private = list(
                    tNow = NULL,
                    HumanPop = NULL,
                    Patches = NULL,
                    MosquitoPop = NULL
                  )

)
