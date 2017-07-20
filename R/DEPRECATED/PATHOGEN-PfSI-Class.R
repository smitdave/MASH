

#' PfSI Human-stage Pathogen Class Definition
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
humanPfSI <- R6::R6Class(classname="humanPfSI",
                     portable = TRUE,
                     cloneable = FALSE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #initialize
                       initialize = function(PfID, tInf = NULL, b = 0.55, c = 0.15, damID = NULL, sireID = NULL, infected = FALSE, chemoprophylaxis = FALSE){
                         private$PfID = PfID
                         private$tInf = tInf
                         private$b = b
                         private$c = c
                         private$damID = damID
                         private$sireID = sireID
                         private$infected = infected
                         private$chemoprophylaxis = chemoprophylaxis
                       },

                       #finalize
                       finalize = function(){
                        #  print(paste0("humanPfSI object PfID: ",PfID," was garbage collected!"))
                       },

                       ########################################
                       #  Accessors, Pointers, and Setters
                       ########################################

                       get_PfID = function(){
                         return(private$PfID)
                       },
                       set_PfID = function(PfID){
                         private$PfID = PfID
                       },
                       push_PfID = function(PfID){
                         private$PfID = c(private$PfID,PfID)
                       },

                       get_tInf = function(){
                         return(private$tInf)
                       },
                       set_tInf = function(tInf){
                         private$tInf = tInf
                       },
                       push_tInf = function(tInf){
                         private$tInf = c(private$tInf,tInf)
                       },

                       get_b = function(){
                         return(private$b)
                       },
                       set_b = function(b){
                         private$b = b
                       },

                       get_c = function(){
                         return(private$c)
                       },
                       set_c = function(c){
                         private$c = c
                       },

                       get_damID = function(){
                         return(private$damID)
                       },
                       set_damID = function(damID){
                         private$damID = damID
                       },
                       push_damID = function(damID){
                         private$damID = c(private$damID,damID)
                       },

                       get_sireID = function(){
                         return(private$sireID)
                       },
                       set_sireID = function(sireID){
                         private$sireID = sireID
                       },
                       push_sireID = function(sireID){
                         private$sireID = c(sireID,private$sireID)
                       },

                       get_infected = function(){
                         return(private$infected)
                       },
                       set_infected = function(infected){
                         private$infected = infected
                       },

                       get_chemoprophylaxis = function(){
                         return(private$chemoprophylaxis)
                       },
                       set_chemoprophylaxis = function(chemoprophylaxis){
                         private$chemoprophylaxis = chemoprophylaxis
                       },

                       get_private = function(){
                         return(as.list(private))
                       }

                     ),

                     #private members
                     private = list(

                       PfID = NULL, # pathogen ID
                       tInf = NULL, # time of infection (mosquito to human transmission)
                       b = NULL, # transmission efficiency: infected mosquito to human
                       c = NULL, # transmission efficiency: infected human to mosquito
                       damID = NULL, # female gametocyte 'mother'
                       sireID = NULL, # male gametocyte 'father'
                       infected = NULL,
                       chemoprophylaxis = NULL

                     )

) #end class definition
