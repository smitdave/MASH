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

                     # set everyone's pointers to each other.

                   },

                   #################################################################
                   # Getters & Setters
                   #################################################################

                   get_tNow = function(){
                     return(private$tNow)
                   },
                   set_tNow = function(tNow){
                     private$tNow = tNow
                   }

                 ),

                 # private members
                 private = list(

                   tNow = NULL,
                   humans = NULL,
                   mosquito = NULL,
                   sites = NULL

                 )
)
