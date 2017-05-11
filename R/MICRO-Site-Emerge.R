#################################################################
#
#   MASH
#   R6-ified
#   Aquatic Ecology for Sites:
#   Specific Functions for "Emerge" Module
#   Hector Sanchez & Sean Wu
#   May 10, 2017
#
#################################################################


#################################################################
# oneDay: control daily Aquatic Ecology dynamics for site
#################################################################

#' Initialize Emerge MODULE
#'
#' This function initializes methods and fields for the 'Emerge' MODULE of Aquatic Ecology COMPONENT.
#' It modifies \code{\link{AquaticSite}} and \code{\link{Landscape}} classes.
#'
#' @param dunno sdf
#' @return stuff
#' @examples
#' init.Emerge()
#' @export
init.Emerge <- function(){

  message("initializing 'Emerge' module for Aquatic Ecology")

  #
  AquaticSite$set(which = "public",name = "oneDay",
            value = function(){

              # log EggQ
              if(EggQ_TRACK){
                trackEggQ(con = NULL)
              }

              # clear EggQ
              self$clearEggQ()

            }
  )

}
