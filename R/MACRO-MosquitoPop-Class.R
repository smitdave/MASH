#############################################
#
# Alpha version of MACRO
# R6 secret sauce version
# David Smith & Sean Wu
# May 11, 2017
#
#############################################

#' MACRO Mosquito Population Class Definition
#'
#' This is a generic MACRO mosquito population blah blah ...
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
#' @section Private:
#' \itemize{
#'   \item{maxEIP: maximum value of extrinsic incubation period}
#'   \item{nPatches: number of patches in \code{\link{Tile}}}
#'   \item{RMparameters}{
#'      \itemize{
#'          \item{p: the probability of surviving one day}
#'          \item{f: the proportion of mosquitoes that blood feed each day}
#'          \item{Q: the proportion of mosquitoes that feed on humans}
#'          \item{v: ?}
#'      }
#'   }
#'   \item{MP}{
#'      \itemize{
#'          \item{M: the number of mosquitoes}
#'          \item{Y: the number of infected mosquitoes (including Z)}
#'          \item{Z: the number of infectious mosquitoes}
#'          \item{ZZ: matrix where each row is the number that will be added to the infectious state on that day}
#'      }
#'   }
#'   \item{P: p^EIP}
#'   \item{Psi: psi^EIP}
#' }
#' @section Active Bindings:
#' \itemize{
#'   \item{\code{coolActiveBinding}}{...}
#' }
#' @export
MacroMosquitoPop <- R6::R6Class(classname = "MacroMosquitoPop",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public methods & fields
                 public = list(

                   # class initialize
                   initialize = function(nPatches = 10, maxEIP = 10, RMparameters){

                      private$maxEIP = maxEIP
                      private$nPatches = nPatches

                      private$RMparameters = RMparameters
                      private$psi = diag(nPatches)

                      private$MP = list(
                        M=rep(10, nPatches),
                        Y=rep(0, nPatches),
                        Z=rep(0, nPatches),
                        ZZ = matrix(0,maxEIP,nPatches),
                      )

                      private$P = RMparameters$p^c(1:maxEIP)
                      private$Psi = private$psi


                   }

                  ),

                  # private methods & fields
                  private = list(

                    maxEIP = NULL,
                    nPatches = NULL,
                    MP = NULL,
                    RMparameters = NULL,
                    psi = NULL,
                    P = NULL,
                    Psi = NULL

                  ),

                  # active bindings
                  active = list(

                  )
)
