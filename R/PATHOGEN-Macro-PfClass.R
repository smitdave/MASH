#################################################################
#
#   MASH
#   R6-ified
#   Class definition for Pf PATHOGEN component for MACRO
#   David Smith, Hector Sanchez, Sean Wu
#   May 22, 2016
#
#################################################################

#' MACRO Pf Pathogen Object Class Definition
#'
#' This is a generic Pf Pathogen object for a patch blah blah ...
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
patchPf <- R6::R6Class(classname="patchPf",
                     portable = TRUE,
                     cloneable = FALSE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #initialize
                       initialize = function(damID = NULL, sireID = NULL){
                         private$damID = damID
                         private$sireID = sireID
                       },

                      #  #finalize
                      #  finalize = function(){
                      #   #  print(paste0("mosquitoPfSI object PfID: ",PfID," was garbage collected!"))
                      #  },

                       ########################################
                       #  Accessors, Pointers, and Setters
                       ########################################

                       # damID
                       get_damID = function(){
                         return(private$damID)
                       },
                       set_damID = function(damID){
                         private$damID = damID
                       },
                       push_damID = function(damID){
                         private$damID = c(private$damID,damID)
                       },

                       # sireID
                       get_sireID = function(){
                         return(private$sireID)
                       },
                       set_sireID = function(sireID){
                         private$sireID = sireID
                       },
                       push_sireID = function(sireID){
                         private$sireID = c(private$sireID,sireID)
                       }

                     ),

                     #private members
                     private = list(

                       damID = NULL, # female gametocyte 'mother'
                       sireID = NULL # male gametocyte 'father'

                     )

) #end class definition
