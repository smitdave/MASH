#################################################################
#
#   MASH
#   R6-ified
#   Minimal Landscape for Well-mixed Patch
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   May 9, 2017
#
#################################################################


#################################################################
# Landscape Definition
#################################################################

#' MICRO Landscape Class Definition
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
Landscape <- R6::R6Class(classname = "Landscape",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                  #################################################
                  # Initialize
                  #################################################

                  # #  initialize
                  #  initialize = function(landscape.PAR){
                  #
                  #    with(landscape.PAR,{
                  #
                  #        #########################################
                  #        # Generate Feeding Sites
                  #        #########################################
                  #
                  #        private$FeedingSites = vector(mode="list",length=nFeed)
                  #        for(ix in 1:nFeed){
                  #          private$FeedingSites[[ix]] = FeedingSite$new(
                  #            ix = ix,
                  #            siteXY = c(feedXY$x[ix],feedXY$y[ix]),
                  #            searchWt = feedWt[ix],
                  #            enterP = enterP[ix],
                  #            hazV = hazV[ix],
                  #            hazW = hazW[ix],
                  #            hazI = hazI[ix],
                  #            sugar = sugar[ix])
                  #        }
                  #
                  #        #########################################
                  #        # Generate Aquatic Habitats
                  #        #########################################
                  #
                  #        private$AquaSites = vector(mode="list",length=nAqua)
                  #        for(ix in 1:nAqua){
                  #          private$AquaSites[[ix]] = AquaticSite$new(ix = ix,
                  #           siteXY = c(aquaXY$x[ix],aquaXY$y[ix]),
                  #           searchWt = aquaWt[ix],
                  #           lambda = lambda[[ix]],
                  #           haz = haz[ix])
                  #        }
                  #
                  #      })
                  #  },

                  #################################################################
                  # Getters & Setters
                  #################################################################

                  # FeedingSites
                  get_FeedingSites = function(ixS = NULL){
                    if(is.null(ixS)){
                      return(private$FeedingSites[[ixS]])
                    } else {
                      return(private$FeedingSites)
                    }
                  },
                  set_FeedingSites = function(FeedingSites, ixS = NULL){
                    if(is.null(ixS)){
                      private$FeedingSites = FeedingSites
                    } else {
                      private$FeedingSites[[ixS]] = FeedingSites
                    }
                  },

                  # AquaSites
                  get_AquaSites = function(ixS = NULL){
                    if(is.null(ixS)){
                      return(private$AquaSites[[ixS]])
                    } else {
                      return(private$AquaSites)
                    }
                  },
                  set_AquaSites = function(AquaSites, ixS = NULL){
                    if(is.null(ixS)){
                      private$AquaSites = AquaSites
                    } else {
                      private$AquaSites[[ixS]] = AquaSites
                    }
                  },

                  # SugarSites
                  get_SugarSites = function(ixS = NULL){
                    if(is.null(ixS)){
                      return(private$MatingSites[[ixS]])
                    } else {
                      return(private$MatingSites)
                    }
                  },
                  set_SugarSites = function(SugarSites, ixS = NULL){
                    if(is.null(ixS)){
                      private$SugarSites = SugarSites
                    } else {
                      private$SugarSites[[ixS]] = SugarSites
                    }
                  },

                  # MatingSites
                  get_MatingSites = function(ixS = NULL){
                    if(is.null(ixS)){
                      return(private$MatingSites[[ixS]])
                    } else {
                      return(private$MatingSites)
                    }
                  },
                  set_MatingSites = function(MatingSites, ixS = NULL){
                    if(is.null(ixS)){
                      private$MatingSites = MatingSites
                    } else {
                      private$MatingSites[[ixS]] = MatingSites
                    }
                  },


                  #################################################################
                  # Pointers
                  #################################################################

                  # TilePointer
                  get_TilePointer = function(){
                    return(private$TilePointer)
                  },
                  set_TilePointer = function(TilePointer){
                    private$TilePointer = TilePointer
                  },

                  # MosquitoPopFemalePointer
                  get_MosquitoPopFemalePointer = function(){
                    return(private$MosquitoPopFemalePointer)
                  },
                  set_MosquitoPopFemalePointer = function(MosquitoPopFemalePointer){
                    private$MosquitoPopFemalePointer = MosquitoPopFemalePointer
                  },

                  # MosquitoPopMalePointer
                  get_MosquitoPopMalePointer = function(){
                    return(private$MosquitoPopMalePointer)
                  },
                  set_MosquitoPopMalePointer = function(MosquitoPopMalePointer){
                    private$MosquitoPopMalePointer = MosquitoPopMalePointer
                  },

                  # HumansPointer
                  get_HumansPointer = function(){
                    return(private$HumansPointer)
                  },
                  set_HumansPointer = function(HumansPointer){
                    private$HumansPointer = HumansPointer
                  },


                  #################################################################
                  # Public Fields
                  #################################################################

                  # Number of Sites
                  FeedingSitesN = NULL,
                  AquaSitesN = NULL,
                  SugarSitesN = NULL,
                  MatingSitesN = NULL

                 ),

                 # private members
                 private = list(

                   # Site Types
                   FeedingSites = NULL,
                   AquaSites = NULL,
                   SugarSites = NULL,
                   MatingSites = NULL,

                   # Pointers
                   TilePointer = NULL,                    # point to the enclosing microsimulation TILE (MICRO)
                   MosquitoPopFemalePointer = NULL,       # point to the MosquitoPopFemale in this enclosing microsimulation TILE (MICRO)
                   MosquitoPopMalePointer = NULL,         # point to the MosquitoPopMale in this enclosing microsimulation TILE (MICRO)
                   HumansPointer = NULL                   # point to the HumanPop in this enclosing microsimulation TILE (MICRO)


                 )
)
