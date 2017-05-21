#################################################################
#
#   MASH-R6
#   PATHOGEN component
#   PfSI module R6 class definitions
#   Sean Wu
#   May 19, 2016
#
#################################################################

#' PfSI Mosquito-stage Pathogen Object Class Definition
#'
#' This is a generic PfSI Pathogen object blah blah ...
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
pathogenObjMosquito_PfSI <- R6::R6Class(classname="pathogenObjMosquito_PfSI",
                     portable = TRUE,
                     cloneable = FALSE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #initialize
                       initialize = function(PfID, tBite, damID = NULL, sireID = NULL){
                         private$PfID = PfID
                         private$tBite = tBite
                         private$damID = damID
                         private$sireID = sireID
                         private$infected = TRUE
                       },

                       #finalize
                       finalize = function(){
                         print("i got garbage collected!")
                       },

                       ########################################
                       #  Accessors, Pointers, and Setters
                       ########################################

                       get_PfID = function(){
                         return(private$PfID)
                       },
                       get_damID = function(){
                         return(private$damID)
                       },
                       get_sireID = function(){
                         return(private$sireID)
                       },
                       get_tInf = function(){
                         return(private$tInf)
                       },
                       get_Private = function(){
                         return(as.list(private))
                       }

                     ),

                     #private members
                     private = list(

                       PfID = NULL, # pathogen ID
                       tBite = NULL, # time of infection
                       damID = NULL, # female gametocyte 'mother'
                       sireID = NULL, # male gametocyte 'father'
                       infected = logical(0) # infection status

                     )

) #end class definition


#' PfSI Human-stage Pathogen Object Class Definition
#'
#' This is a generic PfSI Pathogen object blah blah ...
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
pathogenObjHuman_PfSI <- R6::R6Class(classname="pathogenObjHuman_PfSI",
                     portable = TRUE,
                     cloneable = FALSE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #initialize
                       initialize = function(PfID, tInf, damID = NULL, sireID = NULL){
                         private$PfID = PfID
                         private$tInf = tInf
                         private$damID = damID
                         private$sireID = sireID
                       },

                       #finalize
                       finalize = function(){
                         print("i got garbage collected!")
                       },

                       ########################################
                       #  Accessors, Pointers, and Setters
                       ########################################

                       get_PfID = function(){
                         return(private$PfID)
                       },
                       get_damID = function(){
                         return(private$damID)
                       },
                       get_sireID = function(){
                         return(private$sireID)
                       },
                       get_tInf = function(){
                         return(private$tInf)
                       },
                       get_Private = function(){
                         return(as.list(private))
                       }

                     ),

                     #private members
                     private = list(

                       PfID = NULL, # pathogen ID
                       damID = NULL, # female gametocyte 'mother'
                       sireID = NULL, # male gametocyte 'father'
                       tInf = NULL # time of infection

                     )

) #end class definition
