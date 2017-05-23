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
                   initialize = function(MacroPatch_PAR){
                     private$N = N

                     private$PfTypes = vector(mode="list",length=N)
                     for(ixP in 1:N){
                       private$PfTypes[[ixP]] = patchPf(damID = NULL, sireID = NULL)
                     }
                   },

                   ########################################
                   #  Accessors, Pointers, and Setters
                   ########################################

                   get_N = function(){
                     return(private$N)
                   },

                   # Houses
                   get_hhID = function(ix = NULL){
                     if(is.null(ix)){
                       return(hhID)
                     } else {
                       return(hhID[[ix]])
                     }
                   },

                   # Biting weights
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

                   # Net infectiousness
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
                   get_aquaID = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$aquaID)
                     } else {
                       return(private$aquaID[ix])
                     }
                   },
                   set_aquaID = function(aquaID, ix = NULL){
                     if(!is.null(ix)){
                       private$aquaID[ix] = aquaID
                     } else {
                       private$aquaID = aquaID
                     }
                   },

                   get_aquaP = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$aquaP)
                     } else {
                       return(private$aquaP[ix])
                     }
                   },
                   set_aquaP = function(aquaP, ix = NULL){
                     if(!is.null(ix)){
                       private$aquaP[ix] = aquaP
                     } else {
                       private$aquaP = aquaP
                     }
                   },

                   get_aquaNewM = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$aquaNewM)
                     } else {
                       return(private$aquaNewM[ix])
                     }
                   },
                   set_aquaNewM = function(aquaNewM, ix = NULL){
                     if(!is.null(ix)){
                       private$aquaNewM[ix] = aquaNewM
                     } else {
                       private$aquaNewM = aquaNewM
                     }
                   },

                   get_weightAqua = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$weightAqua)
                     } else {
                       return(private$weightAqua[ix])
                     }
                   },
                   set_weightAqua = function(weightAqua, ix = NULL){
                     if(!is.null(ix)){
                       private$weightAqua[ix] = weightAqua
                     } else {
                       private$weightAqua = weightAqua
                     }
                   },

                   get_weightOvitrap = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$weightOvitrap)
                     } else {
                       return(private$weightOvitrap[ix])
                     }
                   },
                   set_weightOvitrap = function(weightOvitrap, ix = NULL){
                     if(!is.null(ix)){
                       private$weightOvitrap[ix] = weightOvitrap
                     } else {
                       private$weightOvitrap = weightOvitrap
                     }
                   },

                   # Sugar feeding
                   get_weightSugar = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$weightSugar)
                     } else {
                       return(private$weightSugar[ix])
                     }
                   },
                   set_weightSugar = function(weightSugar, ix = NULL){
                     if(!is.null(ix)){
                       private$weightSugar[ix] = weightSugar
                     } else {
                       private$weightSugar = weightSugar
                     }
                   },

                   get_weightBait = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$weightBait)
                     } else {
                       return(private$weightBait[ix])
                     }
                   },
                   set_weightBait = function(weightBait, ix = NULL){
                     if(!is.null(ix)){
                       private$weightBait[ix] = weightBait
                     } else {
                       private$weightBait = weightBait
                     }
                   },

                   # Mating
                   get_weightMate = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$weightMate)
                     } else {
                       return(private$weightMate[ix])
                     }
                   },
                   set_weightMate = function(weightMate, ix = NULL){
                     if(!is.null(ix)){
                       private$weightMate[ix] = weightMate
                     } else {
                       private$weightMate = weightMate
                     }
                   },

                   # Parasite
                   get_PfTypes = function(ix){
                     if(is.null(ix)){
                       return(private$PfTypes)
                     } else {
                       return(private$PfTypes[[ix]])
                     }
                   }

                  ),

                  # private methods & fields
                  private = list(

                    N         = NULL, # number of patches

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
                    PfTypes = list()

                  )

                  # # active bindings
                  # active = list(
                  #
                  # )
)
