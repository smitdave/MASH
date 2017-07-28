#################################################################
#
#   MASH
#   R6-ified
#   MICRO Site Classes
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   April 28, 2017
#
#################################################################

#################################################################
# Generic Site Definition
#################################################################

#' MICRO Generic Site Class Definition
#'
#'
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#' @details
#'
#'
#'
#'
#'
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
MicroSite <- R6::R6Class(classname = "Site",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   #################################################
                   # Getters and Setters
                   #################################################

                   # site index
                   get_ix = function(){return(private$ix)},
                   set_ix = function(ix){private$ix = ix},

                   # site coordinates
                   get_siteXY = function(){return(private$siteXY)},
                   set_siteXY = function(newSiteXY){private$siteXY = siteXY},

                   # search weight
                   get_searchWt = function(){return(private$searchWt)},
                   set_searchWt = function(searchWt){private$searchWt = searchWt},

                   # site type (1 is domestic, 0 is not peri-domestic)
                   get_siteType = function(){return(private$siteType)},
                   set_siteType = function(siteType){priate$siteType = siteType},

                   #################################################
                   # Pointers
                   #################################################

                   # landscape pointer
                   get_LandscapePointer = function(){return(private$LandscapePointer)},
                   set_LandscapePointer = function(LandscapePointer){private$LandscapePointer = LandscapePointer}

                 ),

                 # private members
                 private = list(

                   ix = 0L,
                   siteXY = vector(mode="numeric",length=2L),
                   searchWt = 0L,
                   siteType = NULL,   # an aquatic or sugar site could be inside of a house, for example.

                   # Pointers
                   LandscapePointer = NULL

                 )
)


#################################################################
# Blood Feeding Site
#################################################################

#' MICRO Feeding Site Class Definition
#'
#' This is a generic site blah blah ...
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#' @details
#' * some deets
#' @section Methods:
#' * a method
#' * another method
#' * even more methods
#' * \code{\link{init_riskList}}: initialize a risk list for this site.
#' * \code{\link{extend_riskList}}: extend a risk list for this site.
#' * \code{\link{add_riskList}}: add to a risk list for this site.
#'
#' @section Active Bindings:
#' \describe{
#'   \item{\code{cats}}{...}
#' }
#'
#' @md
#' @export
FeedingSite <- R6::R6Class(classname = "FeedingSite",
                 inherit = MicroSite,
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   #################################################
                   # Initialize
                   #################################################

                   initialize = function(ix, siteXY, searchWt, enterP, hazV = 0, hazW = 0, hazI = 0, sugar = NULL, siteType = 1L){

                     private$ix = ix
                     private$siteXY = siteXY
                     private$searchWt = searchWt
                     private$hazV = hazV
                     private$hazW = hazW
                     private$hazI = hazI
                     private$sugar = sugar
                     private$enterP = enterP
                     private$siteType = siteType
                     private$RiskQ = MASH::RiskQ()

                   },

                   #################################################
                   # Getters and Setters
                   #################################################

                   # vegetation hazards
                   get_hazV = function(){return(private$hazV)},
                   set_hazV = function(hazV){private$hazV = hazV},

                   # outside wall hazards
                   get_hazW = function(){return(private$hazW)},
                   set_hazW = function(hazW){private$hazW = hazW},

                   # inside wall hazards
                   get_hazI = function(){return(private$hazI)},
                   set_hazI = function(hazI){private$hazI = hazI},

                   # sugar source (only used in MBITES-BRO and MBITES-BROM)
                   get_sugar = function(){return(private$sugar)},
                   set_sugar = function(sugar){private$sugar = sugar},

                   # house entry probability
                   get_enterP = function(){return(private$enterP)},
                   set_enterP = function(enterP){private$enterP = enterP}

                 ),

                 # private members
                 private = list(

                   hazV = NULL,      # vegetation hazards
                   hazW = NULL,      # outside wall hazards
                   hazI = NULL,      # inside wall hazards
                   sugar = NULL,     # sugar source (only used in MBITES-BRO and MBITES-BROM)
                   enterP = NULL,    # house entry probability
                   RiskQ = NULL      # host risk queue

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
                 inherit = MicroSite,
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   #################################################
                   # Initialize
                   #################################################

                   # maxQ: pre-alloc ImagoQ and EggQ size
                   initialize = function(ix, siteXY, searchWt, module, lambda = NULL, haz = 0, siteType = 0L maxQ = 20L){

                     # generic fields
                     private$ix = ix
                     private$siteXY = siteXY
                     private$searchWt = searchWt
                     private$haz = haz

                     # shared Aquatic Ecology fields
                     # private$ImagoQ = allocImagoQ(N = maxQ)
                     private$ImagoQ = MASH::ImagoQ()

                     # Aquatic Ecology Emerge module fields
                     if(module == "emerge"){
                       # Emerge module
                       private$lambda = lambda
                     } else if(module == "EL4P"){
                       # EL4P module
                       private$EggQ = allocEggQ(N = maxQ)
                     } else {
                       stop("unrecognized module")
                     }

                   },

                   #################################################
                   # Getters and Setters
                   #################################################

                   get_haz = function(){return(private$haz)},
                   set_haz = function(haz){private$haz = haz},

                   get_ImagoQ = function(){return(private$ImagoQ)},
                   set_ImagoQ = function(ImagoQ){private$ImagoQ = ImagoQ},

                   get_EggQ = function(){return(private$EggQ)},
                   set_EggQ = function(EggQ){private$EggQ = EggQ},

                   get_lambda = function(){return(private$lambda)},
                   set_lambda = function(lambda){private$lambda = lambda}

                 ),

                 # private members
                 private = list(

                   haz = NULL,

                   ImagoQ = NULL,
                   EggQ = NULL,

                   # Emerge fields
                   lambda = NULL

                 )
)
