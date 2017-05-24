#############################################
#
# Alpha version of MACRO
# R6 secret sauce version
# David Smith & Sean Wu
# May 11, 2017
#
#############################################

#' MACRO Mosquito Population Class Definition
#'
#' This is a generic MACRO mosquito population blah blah ...
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
#' @section Private:
#' \itemize{
#'   \item{maxEIP: maximum value of extrinsic incubation period}
#'   \item{nPatches: number of patches in \code{\link{Tile}}}
#'   \item{RMparameters}{
#'      \itemize{
#'          \item{p: the probability of surviving one day}
#'          \item{f: the proportion of mosquitoes that blood feed each day}
#'          \item{Q: the proportion of mosquitoes that feed on humans}
#'          \item{v: ?}
#'      }
#'   }
#'   \item{MP}{
#'      \itemize{
#'          \item{M: the number of mosquitoes}
#'          \item{Y: the number of infected mosquitoes (including Z)}
#'          \item{Z: the number of infectious mosquitoes}
#'          \item{ZZ: matrix where each row is the number that will be added to the infectious state on that day}
#'      }
#'   }
#'   \item{P: p^EIP}
#'   \item{Psi: psi^EIP}
#' }
#' @section Active Bindings:
#' \itemize{
#'   \item{\code{coolActiveBinding}}{...}
#' }
#' @export
MacroMosquitoPop <- R6::R6Class(classname = "MacroMosquitoPop",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public methods & fields
                 public = list(

                   # class initialize
                   # N: number of patches
                   # M_density: mosquito density at each patch
                   # RMparameters: a list of RM parameters
                   initialize = function(N, M_density, RMparameters){

                     private$p = RMparameters$p
                     private$f = RMparameters$f
                     private$Q = RMparameters$Q
                     private$v = RMparameters$v
                     private$maxEIP = RMparameters$maxEIP

                     if(length(M_density)==1){
                       private$M = rep(M_density,N) # mosquito density
                     } else if(length(M_density)!=N){
                       stop("M_density must either be of length 1 or equal to N!")
                     } else {
                       private$M = M_density
                     }
                     private$Y   = rep(0L, N) # infected (incubating)
                     private$Z   = rep(0L, N) # infectious
                     private$ZZ  = matrix(data=0L,nrow=RMparameters$maxEIP,ncol=N) # each row is the number that will be added to the infectious state on that day
                     private$P   = p^c(1:RMparameters$maxEIP) # survival over EIP

                   },

                   #################################################
                   # Getters and Setters
                   #################################################

                   # p: daily survival; lifetime is geometric(p)
                   get_p = function(){
                     return(private$p)
                   },
                   set_p = function(p){
                     private$p = p
                   },

                   # f: feeding rate
                   get_f = function(){
                     return(private$f)
                   },
                   set_f = function(f){
                     private$f = f
                   },

                   # Q: human blood index
                   get_Q = function(){
                     return(private$Q)
                   },
                   set_Q = function(Q){
                     private$Q = Q
                   },

                   # v: daily egg laying rate
                   get_v = function(){
                     return(private$v)
                   },
                   set_v = function(v){
                     private$v = v
                   },

                   # maxEIP: maximum length of EIP
                   get_maxEIP = function(){
                     return(private$maxEIP)
                   },
                   set_maxEIP = function(maxEIP){
                     private$maxEIP = maxEIP
                   },

                   # M: mosquito density
                   get_M = function(){
                     return(private$M)
                   },
                   set_M = function(M){
                     private$M = M
                   },

                   # Y: incubating mosquitoes
                   get_Y = function(){
                     return(private$Y)
                   },
                   set_Y = function(Y){
                     private$Y = Y
                   },

                   # Z: infectious mosquitoes
                   get_Z = function(){
                     return(private$Z)
                   },
                   set_Z = function(Z){
                     private$Z = Z
                   },

                   # ZZ: mosquito progression through EIP
                   get_ZZ = function(){
                     return(private$ZZ)
                   },
                   set_ZZ = function(ZZ){
                     private$ZZ = ZZ
                   },

                   # P: fraction of cohort to survive over EIP
                   get_P = function(){
                     return(private$P)
                   },
                   set_P = function(P){
                     private$P = P
                   },

                   #################################################
                   # Pointers
                   #################################################

                   # PatchesPointer
                   get_PatchesPointer = function(){
                     return(private$PatchesPointer)
                   },
                   set_PatchesPointer = function(PatchesPointer){
                     private$PatchesPointer = PatchesPointer
                   },

                   # HumansPointer
                   get_HumansPointer = function(){
                     return(private$HumansPointer)
                   },
                   set_HumansPointer = function(HumansPointer){
                     private$HumansPointer = HumansPointer
                   }

                  ),

                  # private methods & fields
                  private = list(

                    # RM parameters
                    p = NULL,
                    f = NULL,
                    Q = NULL,
                    v = NULL,
                    maxEIP = NULL,

                    # Life stages
                    M   = NULL, # mosquito density
                    Y   = NULL, # infected (incubating)
                    Z   = NULL, # infectious
                    ZZ  = NULL, # each row is the number that will be added to the infectious state on that day

                    # Survival & Dispersion
                    P   = NULL,

                    # Pointers
                    PatchesPointer = NULL, # point to the enclosing Patches (a network of patches) in this metapopulation TILE (MACRO)
                    HumansPointer = NULL # point to the HumanPop class that also lives in this metapopulation TILE

                  )

)
