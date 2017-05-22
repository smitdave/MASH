#############################################
#
# Alpha version of MACRO
# R6 secret sauce version
# David Smith & Sean Wu
# May 11, 2017
#
#############################################

#' MACRO Patch Class Definition
#'
#' This is a generic collection MACRO patches blah blah ...
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
MacroPatch <- R6::R6Class(classname = "MacroPatch",
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

                   # hhID
                   get_hhID = function(ix = NULL){
                     if(is.null(ix)){
                       return(hhID)
                     } else {
                       return(hhID[[ix]])
                     }
                   },

                   # biting weights
                   get_bWeightHuman = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$bWeightHuman)
                     } else {
                       return(private$bWeightHuman[ix])
                     }
                   },
                   set_bWeightHuman = function(bWeightHuman, ix = NULL){
                     # set one element or entire vector
                     if(!is.null(ix)){
                       private$bWeightHuman[ix] = bWeightHuman
                     } else {
                       private$bWeightHuman = bWeightHuman
                     }
                   },

                   get_bWeightZoo = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$bWeightZoo)
                     } else {
                       return(private$bWeightZoo[ix])
                     }
                   },
                   set_bWeightZoo = function(bWeightZoo, ix = NULL){
                     # set one element or entire vector
                     if(!is.null(ix)){
                       private$bWeightZoo[ix] = bWeightZoo
                     } else {
                       private$bWeightZoo = bWeightZoo
                     }
                   },

                   get_bWeightZootox = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$bWeightZootox)
                     } else {
                       return(private$bWeightZootox[ix])
                     }
                   },
                   set_bWeightZootox = function(bWeightZootox, ix = NULL){
                     # set one element or entire vector
                     if(!is.null(ix)){
                       private$bWeightZootox[ix] = bWeightZootox
                     } else {
                       private$bWeightZootox = bWeightZootox
                     }
                   },

                   # net infectiousness
                   get_Q = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$Q)
                     } else {
                       return(private$Q[ix])
                     }
                   },
                   set_Q = function(Q, ix = NULL){
                     # set one element or entire vector
                     if(!is.null(ix)){
                       private$Q[ix] = Q
                     } else {
                       private$Q = Q
                     }
                   },

                   get_kappa = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$kappa)
                     } else {
                       return(private$kappa[ix])
                     }
                   },
                   set_kappa = function(kappa, ix = NULL){
                     # set one element or entire vector
                     if(!is.null(ix)){
                       private$kappa[ix] = kappa
                     } else {
                       private$kappa = kappa
                     }
                   },

                   get_humanIDs = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$humanIDs)
                     } else {
                       return(private$humanIDs[[ix]])
                     }
                   },
                   set_humanIDs = function(humanIDs, ix = NULL){
                     if(!is.null(ix)){
                       private$humanIDs[[ix]] = humanIDs
                     } else {
                       private$humanIDs = humanIDs
                     }
                   },

                   # Egg laying
                   get_humanIDs = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$humanIDs)
                     } else {
                       return(private$humanIDs[[ix]])
                     }
                   },
                   set_humanIDs = function(humanIDs, ix = NULL){
                     if(!is.null(ix)){
                       private$humanIDs[[ix]] = humanIDs
                     } else {
                       private$humanIDs = humanIDs
                     }
                   },

                   get_humanIDs = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$humanIDs)
                     } else {
                       return(private$humanIDs[[ix]])
                     }
                   },
                   set_humanIDs = function(humanIDs, ix = NULL){
                     if(!is.null(ix)){
                       private$humanIDs[[ix]] = humanIDs
                     } else {
                       private$humanIDs = humanIDs
                     }
                   },

                   get_humanIDs = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$humanIDs)
                     } else {
                       return(private$humanIDs[[ix]])
                     }
                   },
                   set_humanIDs = function(humanIDs, ix = NULL){
                     if(!is.null(ix)){
                       private$humanIDs[[ix]] = humanIDs
                     } else {
                       private$humanIDs = humanIDs
                     }
                   },





                  ),

                  # private methods & fields
                  private = list(

                    N         = NULL, # number of humans

                    # Houses, for effect size estimation
                    # Can use same structures as MICRO for
                    # consistent modeling of vector control.
                    hhID      = list(),

                    # How are infectious bites divided up?
                    bWeightHuman   = NULL,
                    bWeightZoo     = NULL,
                    bWeightZootox  = NULL,

                    # Net infectiousness
                    Q         = NULL,
                    kappa     = NULL,
                    humanIDs  = list(),

                    #Egg laying
                    aquaID        = NULL,
                    aquaP         = NULL,
                    aquaNewM      = NULL,
                    weightAqua    = NULL,   # For modeling movement
                    weightOvitrap = NULL,

                    #Sugar feeding
                    weightSugar   = NULL,
                    weightBait    = NULL,

                    #Mating
                    weightMate    = NULL,

                    # Parasite
                    PfTypes = list(dameID=NULL, sireID=NULL),

                    RMparameters = NULL,

                    layingProportions = NULL

                  ),

                  # active bindings
                  active = list(

                  )
)
