#############################################
#
# Alpha version of MACRO
# R6 secret sauce version
# David Smith & Sean Wu
# May 11, 2017
#
#############################################

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
                     private$HumanPop = HumanPop$new(nHum = nHum)
                     private$Patches = Patch$new(N = nPatch)
                     private$MosquitoPop = MacroMosquitoPop$new()
                     # set everyones pointers to each other
                   },

                   # oneDayRM
                   oneDayRM = function(){
                     stop("sean hasn't written me yet!")
                   },

                   get_Pointers = function(){
                     return(private$Pointers)
                   }

                  ),

                  # private methods & fields
                  private = list(
                    HumanPop = NULL,
                    Patches = NULL,
                    MosquitoPop = NULL
                  )
                  
)
