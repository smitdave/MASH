#############################################
#
# Alpha version of MACRO
# R6 secret sauce version
# David Smith & Sean Wu
# May 11, 2017
#
#############################################

library(R6)

#' MACRO Patch Class Definition
#'
#' This is a generic MACRO patch blah blah ...
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
Patch <- R6::R6Class(classname = "Patch",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public methods & fields
                 public = list(

                   # class initialize
                   initialize = function(N, RMparameters=NULL, layingProportions=NULL){
                     private$N = N

                    #  testing stuff, delete later
                     private$kappa = rnorm(1e2)
                   },

                   ########################################
                   #  Accessors, Pointers, and Setters
                   ########################################

                   getkappa = function(){
                     return(private$kappa)
                   }

                  ),

                  # private methods & fields
                  private = list(

                    N         = NULL, # number of humans

                    # Houses, for effect size estimation
                    # Can use same structures as MICRO for
                    # consistent modeling of vector control.
                    hhID      = NULL,

                    # How are infectious bites divided up?
                    w.human   = NULL,
                    w.zoo     = NULL,
                    w.zootox  = NULL,

                    # Net infectiousness
                    Q         = NULL,
                    kappa     = NULL,
                    humanIDs  = list(id=NULL, c=NULL),

                    #Egg laying
                    aqua      = list(id=NULL, p = NULL, newM = NULL),
                    w.aqua    = NULL,   # For modeling movement
                    w.ovitrap = NULL,

                    #Sugar feeding
                    w.sugar   = NULL,
                    w.bait    = NULL,

                    #Mating
                    w.mate    = NULL,

                    # Parasite
                    PfTypes = list(dameID=NULL, sireID=NULL),

                    RMparameters = NULL,

                    layingProportions = NULL

                  ),

                  # active bindings
                  active = list(

                  )
)
