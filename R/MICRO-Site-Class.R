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

                  get_ix = function(){return(private$ix)},
                  set_ix = function(newIx){private$ix <- newIx},

                  get_siteXY = function(){return(private$siteXY)},
                  set_siteXY = function(newSiteXY){private$siteXY <- newSiteXY},

                  get_searchWt = function(){return(private$searchWt)},
                  set_searchWt = function(newSearchWt){private$searchWt <- newSearchWt}

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

                  #  initialize
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
                  get_hazV = function(){return(private$hazV)},
                  set_hazV = function(newHazV){private$hazV <- newHazV},

                  get_hazW = function(){return(private$hazW)},
                  set_hazW = function(newHazW){private$hazW <- newHazW},

                  get_hazI = function(){return(private$hazI)},
                  set_hazI = function(newHazI){private$hazI <- newHazI},

                  get_sugar = function(){return(private$sugar)},
                  set_sugar = function(newSugar){private$sugar <- newSugar},

                  get_enterP = function(){return(private$enterP)},
                  set_enterP = function(newEnterP){private$enterP <- newEnterP},

                  get_riskList = function(){return(private$riskList)},
                  set_riskList = function(newRiskList){private$riskList <- newRiskList}

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
                   get_haz = function(){return(private$haz)},
                   set_haz = function(newHaz){private$haz <- newHaz},

                   get_lambda = function(){return(private$lambda)},
                   set_lambda = function(newLambda){private$lambda <- newLambda}


                 ),

                 # private members
                 private = list(

                   haz = NULL,    # local hazards
                   lambda = vector(mode="numeric",length=365), # daily emergence
                   ImagoQ = NULL,
                   EggQ = NULL

                 )
)
