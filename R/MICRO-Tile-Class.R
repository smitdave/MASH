#################################################################
#
#   MASH
#   R6-ified
#   Well-mixed Patch Class
#   Hector Sanchez & Sean Wu
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

                   initialize = function(numMosquito,     # number of mosquitoes
                                         feedN,           # feeding sites
                                         aquaN,           # aquatic habitat
                                         pointGen,        # point generation function
                                         hhSize,          # average number of humans at feeding sites
                                         hhMin,           # minimum number of humans at feeding sites
                                         xLim = c(0,1),   # x-axis bounds for simulated points
                                         yLim = c(0,1),   # y-axis bounds for simulated points
                                         aquaSD = 0.01,   # standard deviation of aquatic habitat scatter around feeding sites
                                         ...              # additional named arguments for pointGen
                                         ){

                    private$sites = Landscape$new()
                    private$humans
                    private$mosquito

                   }

                 ),

                 # private members
                 private = list(

                   humans = NULL,
                   mosquito = NULL,
                   sites = NULL

                 )
)
