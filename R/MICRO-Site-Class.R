#################################################################
#
#   MASH
#   R6-ified
#   Site Classes
#   Hector Sanchez & Sean Wu
#   April 28, 2017
#
#################################################################

#################################################################
# Generic Site Definition
#################################################################

#' MICRO-Site Generic Class Definition
#'
#' This is a generic site blah blah ...
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
Site <- R6::R6Class(classname = "Site",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                  #  modifiers & accessors

                  getIx = function(){return(private$ix)},
                  setIx = function(newIx){private$ix <- newIx},

                  getSiteXY = function(){return(private$siteXY)},
                  setSiteXY = function(newSiteXY){private$siteXY <- newSiteXY},

                  getSearchWt = function(){return(private$searchWt)},
                  setSearchWt = function(newSearchWt){private$searchWt <- newSearchWt}

                 ),

                 # private members
                 private = list(

                   ix = 0L,
                   siteXY = vector(mode="numeric",length=2L),
                   searchWt = 0L

                 )
)


#################################################################
# Blood Feeding Site
#################################################################

#' MICRO-Site Feeding Site Class Definition
#'
#' This is a generic site blah blah ...
#'  This class inherits from \code{\link{Site}} class.
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
FeedingSite <- R6::R6Class(classname = "FeedingSite",
                 inherit = Site,
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                  #  initializer
                   initialize = function(ix, siteXY, searchWt, enterP, hazV = 0, hazW = 0, hazI = 0, sugar = 1){

                     private$ix = ix
                     private$siteXY = siteXY
                     private$searchWt = searchWt
                     private$hazV = hazV
                     private$hazW = hazW
                     private$hazI = hazI
                     private$sugar = sugar
                     private$enterP = enterP

                   },

                  #  modifiers & accessors
                  getHazV = function(){return(private$hazV)},
                  setHazV = function(newHazV){private$hazV <- newHazV},

                  getHazW = function(){return(private$hazW)},
                  setHazW = function(newHazW){private$hazW <- newHazW},

                  getHazI = function(){return(private$hazI)},
                  setHazI = function(newHazI){private$hazI <- newHazI},

                  getSugar = function(){return(private$sugar)},
                  setSugar = function(newSugar){private$sugar <- newSugar},

                  getEnterP = function(){return(private$enterP)},
                  setEnterP = function(newEnterP){private$enterP <- newEnterP},

                  getRiskList = function(){return(private$riskList)},
                  setRiskList = function(newRiskList){private$riskList <- newRiskList}

                 ),

                 # private members
                 private = list(

                   hazV = NULL,      # vegetation hazards
                   hazW = NULL,      # outside wall hazards
                   hazI = NULL,      # inside wall hazards
                   sugar = NULL,     # sugar source
                   enterP = NULL,    # house entry probability
                   riskList = NULL   # biting risk

                 )
)


#################################################################
# Aquatic Habitat Site
#################################################################

#' MICRO-Site Aquatic Habitat Class Definition
#'
#' This is a generic site blah blah ...
#'  This class inherits from \code{\link{Site}} class.
#'  below i describe the basic structure of the site. methods and fields for specific COMPONENTS can be found in:
#' * \code{\link{init.AquaticEcology}}: generic functions and structures for mangaging Aquatic Ecology COMPONENT
#' * \code{\link{init.Emerge}}: specific functions and structures for 'Emerge' MODULE
#' @md
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
AquaticSite <- R6::R6Class(classname = "AquaticSite",
                 inherit = Site,
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   initialize = function(ix, siteXY, searchWt, lambda, haz = 0){

                     private$ix = ix
                     private$siteXY = siteXY
                     private$searchWt = searchWt
                     private$lambda = lambda
                     private$haz = haz
                     private$ImagoQ = allocImagoQ(N = 1e2)
                     private$EggQ = allocEggQ(N = 1e2)

                   },

                   #  modifiers & accessors
                   getHaz = function(){return(private$haz)},
                   setHaz = function(newHaz){private$haz <- newHaz},

                   getLambda = function(){return(private$lambda)},
                   setLambda = function(newLambda){private$lambda <- newLambda}


                 ),

                 # private members
                 private = list(

                   haz = NULL,    # local hazards
                   lambda = vector(mode="numeric",length=365) # daily emergence

                 )
)
