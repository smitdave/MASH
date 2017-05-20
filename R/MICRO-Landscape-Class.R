#################################################################
#
#   MASH
#   R6-ified
#   Minimal Landscape for Well-mixed Patch
#   Hector Sanchez & Sean Wu
#   May 9, 2017
#
#################################################################


#################################################################
# Landscape Definition
#################################################################

#' MICRO-Landscape Class Definition
#'
#' This is a landscape blah blah.......
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
Landscape <- R6::R6Class(classname = "Landscape",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                  #  initialize
                   initialize = function(landscape.PAR){

                     with(landscape.PAR,{

                         #########################################
                         # Generate Feeding Sites
                         #########################################

                         private$FeedingSites = vector(mode="list",length=nFeed)
                         for(ix in 1:nFeed){
                           private$FeedingSites[[ix]] = FeedingSite$new(
                             ix = ix,
                             siteXY = c(feedXY$x[ix],feedXY$y[ix]),
                             searchWt = feedWt[ix],
                             enterP = enterP[ix],
                             hazV = hazV[ix],
                             hazW = hazW[ix],
                             hazI = hazI[ix],
                             sugar = sugar[ix])
                         }

                         #########################################
                         # Generate Aquatic Habitats
                         #########################################

                         private$AquaSites = vector(mode="list",length=nAqua)
                         for(ix in 1:nAqua){
                           private$AquaSites[[ix]] = AquaticSite$new(ix = ix,
                            siteXY = c(aquaXY$x[ix],aquaXY$y[ix]),
                            searchWt = aquaWt[ix],
                            lambda = lambda[[ix]],
                            haz = haz[ix])
                         }

                       })
                   },

                  #  accessors & modifiers
                  get__FeedingSites = function(){return(private$FeedingSites)},
                  get_AquaSites = function(){return(private$AquaSites)},

                  get_FeedingSitesIx = function(ix){return(private$FeedingSites[[ix]])},
                  get_AquaSitesIx = function(ix){return(private$AquaSites[[ix]])}

                 ),

                 # private members
                 private = list(

                   FeedingSites = NULL,
                   AquaSites = NULL

                 )
)
