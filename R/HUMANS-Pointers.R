#################################################################
#
#   MASH
#   R6-ified
#   Pointer Class for HUMANS
#   Sean Wu
#   May 20, 2016
#
#################################################################

#' HUMANS Pointer Class Definition
#'
#' This is a generic HUMANS pointer blah blah ...
#'  this class does some stuff:
#' * stuff 1
#' * stuff 2
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
macroPointer <- R6::R6Class(classname = "macroPointer",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public methods & fields
                 public = list(

                   # class initialize
                   initialize = function(pointPatch, pointHumanPop){
                     private$patchPointer = pointPatch
                     private$humanPopPointer = pointHumanPop
                   },

                  #  pointPatch
                  pointPatch = function(){
                    return(private$patchPointer)
                  },

                  # pointHumanPop
                  pointHumanPop = function(){
                    return(private$humanPopPointer)
                  }

                  ),

                  # private methods & fields
                  private = list(
                    patchPointer = NULL,
                    humanPopPointer = NULL
                  ),

                  # active bindings
                  active = list(

                  )

)
