#################################################################
#
#   MASH
#   R6-ified
#   Aquatic Ecology for Sites:
#   Generic Queue Structures and Management for Aquatic Habitats
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
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
#' MICRO.Aqua.Setup()
#' @export
MICRO.Aqua.Setup <- function(){

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

  # extend the EggQ
  AquaticSite$set(which = "public",name = "extendEggQ",
            value = function(){
              offset = length(private$EggQ)*2L
              private$EggQ = c(private$EggQ,replicate(n=offset,expr=eggBatch(),simplify=FALSE))
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

  # zeroBatch: zero out an egg batch in EggQ
  AquaticSite$set(which = "public",name = "zeroBatch",
            value = function(ixQ){
              private$EggQ[[ixQ]]$N    = 0L
              private$EggQ[[ixQ]]$tm   = 0
              private$EggQ[[ixQ]]$ix   = 0L
              private$EggQ[[ixQ]]$dam  = 0L
              private$EggQ[[ixQ]]$sire = 0L
            }
  )

  # get ixQ of full slots: return 0 if none (ACTIVE BINDING)
  AquaticSite$set(which = "active",name = "fullIxEggQ",
            value = function(){
              fullIx = vapply(X = private$EggQ,FUN = function(x){x$N != 0L},FUN.VALUE = logical(1))
              if(any(!fullIx)){
                return(0L)
              } else {
                return(which(fullIx))
              }
            }
  )

  # get ixQ of empty slots: return 0 if none (ACTIVE BINDING)
  AquaticSite$set(which = "active",name = "emptyIxEggQ",
            value = function(){
              emptyIx = vapply(X = private$EggQ,FUN = function(x){x$N == 0L},FUN.VALUE = logical(1))
              if(any(!emptyIx)){
                return(0L)
              } else {
                return(which(emptyIx))
              }
            }
  )

  # modifiers & accessors
  AquaticSite$set(which = "public",name = "get_EggQ",
            value = function(){return(private$EggQ)}
  )
  AquaticSite$set(which = "public",name = "set_EggQ",
            value = function(newEggQ){private$EggQ <- newEggQ}
  )

  AquaticSite$set(which = "public",name = "get_EggQixQ",
            value = function(ixQ){return(private$EggQ[[ixQ]])}
  )
  AquaticSite$set(which = "public",name = "set_EggQixQ",
            value = function(newBatch){private$EggQ[[ixQ]] <- newBatch}
  )

  # data logging
  AquaticSite$set(which = "public",name = "get_EggQTot",
            value = function(){
              return(sum(vapply(X = private$EggQ,FUN = function(x){x$N},FUN.VALUE = integer(1))))
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
