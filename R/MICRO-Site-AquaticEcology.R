#################################################################
#
#   MASH
#   R6-ified
#   Aquatic Ecology for Sites:
#   Generic Queue Structures and Management for Aquatic Habitats
#   Hector Sanchez & Sean Wu
#   May 10, 2017
#
#################################################################


#################################################################
# Generic Methods
#################################################################

#' Initialize Aquatic Ecology COMPONENT
#'
#' This function initializes generic methods and fields for the Aquatic Ecology COMPONENT.
#' It modifies \code{\link{AquaticSite}} and \code{\link{Landscape}} classes.
#'
#' @param hi spacecats
#' @return stuff
#' @examples
#' init.Emerge()
#' @export
init.AquaticEcology <- function(){

  ###########################
  # Egg Queue
  ###########################

  # clear the EggQ
  AquaticSite$set(which = "public",name = "clearEggQ",
            value = function(){
              fullIx = self$fullIxEggQ
              if(fullIx!=0L){
                for(ix in fullIx){
                  private$EggQ[[ix]]$N    = 0L
                  private$EggQ[[ix]]$tm   = 0
                  private$EggQ[[ix]]$ix   = 0L
                  private$EggQ[[ix]]$dam  = 0L
                  private$EggQ[[ix]]$sire = 0L
                }
              }
            }
  )

  # add a egg batch to the EggQ
  # argument is from eggBatch which should be called in MBITES
  AquaticSite$set(which = "public",name = "addBatch2Q",
            value = function(eggBatch){

              # manage EggQ
              emptyIx = self$emptyIxEggQ
              if(emptyIx==0L){
                self$extendEggQ()
                emptyIx = self$emptyIxEggQ
              }

              private$EggQ[[emptyIx[1]]]$N    = eggBatch$N
              private$EggQ[[emptyIx[1]]]$tm   = eggBatch$tm
              private$EggQ[[emptyIx[1]]]$ix   = eggBatch$ix
              private$EggQ[[emptyIx[1]]]$dam  = eggBatch$dam
              private$EggQ[[emptyIx[1]]]$sire = eggBatch$sire
            }
  )

  # get ixQ of full slots: return 0 if none
  AquaticSite$set(which = "public",name = "fullIxEggQ",
            value = function(){
              fullIx = vapply(X = private$EggQ,FUN = function(x){x$N != 0L},FUN.VALUE = logical(1))
              if(any(!fullIx)){
                return(0L)
              } else {
                return(which(fullIx))
              }
            }
  )

  # get ixQ of empty slots: return 0 if none
  AquaticSite$set(which = "public",name = "emptyIxEggQ",
            value = function(){
              emptyIx = vapply(X = private$EggQ,FUN = function(x){x$N == 0L},FUN.VALUE = logical(1))
              if(any(!emptyIx)){
                return(0L)
              } else {
                return(which(emptyIx))
              }
            }
  )

  ###########################
  # Imago Queue
  ###########################

  # AquaticSite$set(which = "active",name = "ImagoQueueN",
  #           value = function(){
  #
  #           }
  # )


}

#################################################################
# Egg Queue
#################################################################

#' Empty Egg Batch Slot for EggQ
#'
#' This function is a low-level utility to generate a null placeholder value for an egg batch in EggQ.
#' It is called by \code{\link{allocEggQ}} and \code{\link{extendEggQ}}.
#'
#' @param N number of eggs in this batch
#' @param tm time of oviposition
#' @param ix index of aquatic habitat
#' @param dam id of mother
#' @param sire id of father
#' @return a list of named elements
#' @examples
#' eggBatch(N=0,tm=0,ix=0,dam=0,sire=0)
#' @export
eggBatch = function(N=0L,tm=0,ix=0L,dam=0L,sire=0L){
  list(
        N       = N,    # Eggs
        tm      = tm,   # Time of oviposition
        ix      = ix,   # ix of aqua site
        dam     = dam,  # id of dam
        sire    = sire  # id of sire
  )
}

#' Allocate EggQ
#'
#' This function allocates an empty EggQ to an object of class \code{\link{AquaticSite}}
#'
#' @param N size of empty queue
#' @return empty EggQ
#' @examples
#' allocEggQ(N=N)
#' @export
allocEggQ = function(N){
  return(replicate(n = N,expr = eggBatch(),simplify = FALSE))
}


#################################################################
# Imago Queue
#################################################################

#' Empty Adult Slot for ImagoQ
#'
#' This function is a low-level utility to generate a null placeholder value for adults in ImagoQ.
#' It is called by \code{\link{allocImagoQ}} and \code{\link{extendImagoQ}}.
#'
#' @param N number of emerging adults from this element of queue
#' @param tm time of emergence
#' @param ix index of aquatic habitat
#' @param dam id of mother
#' @param sire id of father
#' @return a list of named elements
#' @examples
#' newImago(N=0,tm=0,ix=0,dam=0,sire=0)
#' @export
newImago = function(N=0L,tm=0,ix=0L,dam=0L,sire=0L){
  list(
    N    = N,
    tm   = tm,
    ix   = ix,
    dam  = dam,
    sire = sire)
}

#' Allocate ImagoQ
#'
#' This function allocates an empty ImagoQ to an object of class \code{\link{AquaticSite}}
#'
#' @param N size of empty queue
#' @return empty ImagoQ
#' @examples
#' allocImagoQ(N=N)
#' @export
allocImagoQ <- function(N){
  return(replicate(n=N,expr=newImago(),simplify=FALSE))
}
