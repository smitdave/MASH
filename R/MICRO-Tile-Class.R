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

#' MICRO-Tile Class Definition
#'
#' This is a tile blah blah.......
#'  categorical summary measure \code{A[j]}. This class inherits from \code{\link{GenericModel}} class.
#'  Defines the fitting algorithm for a regression model \code{A[j] ~ W + ...}.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#' @details
#' \itemize{
#' \item{\code{reg}} - .
#' \item{\code{outvar}} - .
#' \item{\code{levels}} - .
#' \item{\code{nbins}} - .
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{new(reg, DataStorageClass.g0, ...)}}{...}
#'   \item{\code{fit(data)}}{...}
#'   \item{\code{predict(newdata)}}{...}
#'   \item{\code{predictAeqa(newdata)}}{...}
#' }
#' @section Active Bindings:
#' \describe{
#'   \item{\code{cats}}{...}
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
