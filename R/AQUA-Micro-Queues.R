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
# SETUP
#################################################################

#' Initialize Aquatic Ecology COMPONENT
#'
#' This function initializes generic methods and fields for the Aquatic Ecology COMPONENT.
#' It modifies \code{\link{AquaticSite}} and \code{\link{Landscape}} classes.
#'
#' @param module character
#' @return stuff
#' @examples
#' MICRO.Aqua.Setup()
#' @export
MICRO.Aqua.Setup <- function(module = "emerge", overwrite = TRUE){

  #################################################################
  # Egg Queue: only used in EL4P
  #################################################################

  if(module=="EL4P"){

    # clear the EggQ
    AquaticSite$set(which = "public",name = "clear_EggQ",
              value = clear_MicroEggQ,
              overwrite = overwrite
    )

    # extend the EggQ
    AquaticSite$set(which = "public",name = "extend_EggQ",
              value = extend_MicroEggQ,
              overwrite = overwrite
    )

    # add a egg batch to the EggQ
    AquaticSite$set(which = "public",name = "add_EggQ",
              value = add_MicroEggQ,
              overwrite = overwrite
    )

    # zeroBatch: zero out an egg batch in EggQ
    AquaticSite$set(which = "public",name = "zero_EggQ",
              value = zero_MicroEggQ,
              overwrite = overwrite
    )

    # get indices of full slots: return 0 if none
    AquaticSite$set(which = "public",name = "full_EggQ",
              value = full_MicroEggQ,
              overwrite = overwrite
    )

    # get indices of empty slots: return 0 if none
    AquaticSite$set(which = "public",name = "empty_EggQ",
              value = empty_MicroEggQ,
              overwrite = overwrite
    )

    # modifiers & accessors
    AquaticSite$set(which = "public",name = "get_EggQ",
              value = get_MicroEggQ,
              overwrite = overwrite
    )
    AquaticSite$set(which = "public",name = "set_EggQ",
              value = set_MicroEggQ,
              overwrite = overwrite
    )

    # data logging
    AquaticSite$set(which = "public",name = "track_EggQ",
              value = track_MicroEggQ,
              overwrite = overwrite
    )


  }



  #################################################################
  # Imago Queue
  #################################################################

  # AquaticSite$set(which = "active",name = "ImagoQueueN",
  #           value = function(){
  #
  #           }
  # )


}


#################################################################
# EggQ: shared between emerge and EL4P
#################################################################

#' MICRO \code{AquaticSite} Method: Clear the EggQ
#'
#' Clear out all populated slots in an EggQ for the EL4P module of Aquatic Ecology; populated slots are found by calling \code{\link{full_MicroEggQ}}.
#' This method should be called after moving batches from the EggQ to EL4P object (whatever function does this should only move those batches that are ready to go, time-wise). WHEN YOU WRITE THESE FUNCTIONS UPDATE THESE DOCS!
#' This method is bound to \code{AquaticSite$clear_EggQ()}.
#'
clear_MicroEggQ <- function(){
  fullIx = self$full_EggQ()
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


#' MICRO \code{AquaticSite} Method: Extend the EggQ
#'
#' This method extends the EggQ by 2 times its current length of empty egg batch objects, see \code{\link{eggBatch}} for the egg batch structure.
#' This method is bound to \code{AquaticSite$extend_EggQ()}.
#'
extend_MicroEggQ <- function(){
  offset = length(private$EggQ)*2L
  private$EggQ = c(private$EggQ,replicate(n=offset,expr=eggBatch(),simplify=FALSE))
}


#' MICRO \code{AquaticSite} Method: Add a batch to the EggQ
#'
#' This method adds an egg batch to an EggQ, see \code{\link{eggBatch}} for the egg batch structure; it uses \code{\link{empty_MicroEggQ}} to find empty indices and
#' calls \code{\link{extend_MicroEggQ}} if no empty slots are found.
#' This method is bound to \code{AquaticSite$add_EggQ()}.
#'
#' @param eggBatch a single egg batch, see \code{\link{eggBatch}} for the egg batch structure
add_MicroEggQ <- function(eggBatch){
  # manage EggQ
  emptyIx = self$empty_EggQ()
  if(emptyIx==0L){
    self$extend_EggQ()
    emptyIx = self$empty_EggQ()
  }

  private$EggQ[[emptyIx[1]]]$N    = eggBatch$N
  private$EggQ[[emptyIx[1]]]$tm   = eggBatch$tm
  private$EggQ[[emptyIx[1]]]$ix   = eggBatch$ix
  private$EggQ[[emptyIx[1]]]$dam  = eggBatch$dam
  private$EggQ[[emptyIx[1]]]$sire = eggBatch$sire

}


#' MICRO \code{AquaticSite} Method: Zero out a slot in the EggQ
#'
#' This method zeros out a slot in the EggQ.
#' This method is bound to \code{AquaticSite$zero_EggQ()}.
#'
#' @param ixQ the slot in the EggQ to zero out
zero_MicroEggQ <- function(ixQ){
  private$EggQ[[ixQ]]$N    = 0L
  private$EggQ[[ixQ]]$tm   = 0
  private$EggQ[[ixQ]]$ix   = 0L
  private$EggQ[[ixQ]]$dam  = 0L
  private$EggQ[[ixQ]]$sire = 0L
}


#' MICRO \code{AquaticSite} Method: Get indices of full EggQ slots
#'
#' This method finds filled slots in the EggQ; if all slots are empty it returns 0. It is the complement of \code{\link{empty_MicroEggQ}
#' This method is bound to \code{AquaticSite$full_EggQ()}.
#'
full_MicroEggQ <- function(){
  fullIx = vapply(X = private$EggQ,FUN = function(x){x$N != 0L},FUN.VALUE = logical(1))
  if(any(!fullIx)){
    return(0L)
  } else {
    return(which(fullIx))
  }

}


#' MICRO \code{AquaticSite} Method: Get indices of empty EggQ slots
#'
#' This method finds empty slots in the EggQ; if all slots are full it returns 0. It is the complement of \code{\link{full_MicroEggQ}
#' This method is bound to \code{AquaticSite$empty_EggQ()}.
#'
empty_MicroEggQ <- function(){
  emptyIx = vapply(X = private$EggQ,FUN = function(x){x$N == 0L},FUN.VALUE = logical(1))
  if(any(!emptyIx)){
    return(0L)
  } else {
    return(which(emptyIx))
  }
}


#' MICRO \code{AquaticSite} Method: Get the EggQ
#'
#' Get either a single slot or the entire EggQ.
#' This method is bound to \code{AquaticSite$get_EggQ()}.
#'
#' @param ixQ if \code{NULL} return the entire EggQ, else, return the slot \code{ixQ}
get_MicroEggQ <- function(ixQ = NULL){
  if(is.null(ixQ)){
    return(private$EggQ)
  } else {
    return(private$EggQ[[ixQ]])
  }
}


#' MICRO \code{AquaticSite} Method: Set the EggQ
#'
#' Set either a single slot or the entire EggQ.
#' This method is bound to \code{AquaticSite$set_EggQ()}.
#'
#' @param EggQ the object to insert; if \code{ixQ = NULL} then it should be a full EggQ object, see \code{\link{allocEggQ}} for details, else see \code{\link{eggBatch}} for the structure of a single batch.
#' @param ixQ if \code{NULL} set the entire EggQ, else, set the slot \code{ixQ}
set_MicroEggQ <- function(EggQ ,ixQ = NULL){
  if(is.null(ixQ)){
    private$EggQ = EggQ
  } else {
    private$EggQ[[ixQ]] = EggQ
  }
}


#' MICRO \code{AquaticSite} Method: Track the EggQ
#'
#' Return the total number of eggs in this EggQ
#' This method is bound to \code{AquaticSite$track_EggQ()}.
#' DEV_NOTE: eventually may want to add argument time or something and only accumulate those values tm <= time or something...
#'
track_MicroEggQ <- function(){
  return(sum(vapply(X = private$EggQ,FUN = function(x){x$N},FUN.VALUE = integer(1))))
}
