#################################################################
#
#   MASH
#   R6-ified
#   MACRO MacroPatch Class Methods
#   David Smith, Hector Sanchez, Sean Wu
#   May 24, 2016
#
#################################################################


#################################################################
# Aquatic Ecology Component:
# * Module-agnotic (Emerge/EL4P) functions to interface
#   with EggQ and ImagoQ in MacroPatch
#################################################################





#################################################################
# Biting & Kappa:
#################################################################

#' Set \code{MacroPatch} sumKappa
#'
#' generate local human biting propensity kappa
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
sumKappa <- function(){
  
}

# sumKappa = function(ixH){
#   here = HUMANS[[ixH]]$loc
#   LANDSCAPE$kappa[here] <<- LANDSCAPE$kappa[here] + HUMANS[[ixH]]$Pathogens$Pf$c*HUMANS[[ixH]]$w
# }


# updateKappa = function(){
#   for(ixH in 1:nHumans) sumKappa(ixH)
#   LANDSCAPE$kappa <<- with(LANDSCAPE,kappa/(w.human+w.zoo+w.zootox))
# }
