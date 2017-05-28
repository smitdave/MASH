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

# #' Update \code{MacroPatch} kappa For a Patch
# #'
# #' Generate human biting propensity kappa at a single Patch
# #'
# #' @param ixP index of patch
# #' @return does stuff
# #' @examples
# #' some_function()
# sumKappa <- function(ixP){
#   ixH = self$get_humanIDs(ixP)
#   kappaH = numeric(1)
#   for(ixHH in ixH){
#     kappaH = kappaH + (self$get_HumansPointer()$get_Human(ixHH)$get_humanPfSI()$get_b() * self$get_HumansPointer()$get_Human(ixHH)$get_bWeight())
#   }
#   return(kappaH)
# }
#
# # set sumKappa
# MacroPatch$set(which = "public",name = "sumKappa",
#           value = sumKappa,
#           overwrite = TRUE
# )
#
# #' Set \code{MacroPatch} updateKappa
# #'
# #' Update normalized biting propensities for all patches
# #'
# #' @param a parameter
# #' @return does stuff
# #' @examples
# #' some_function()
# updateKappa <- function(){
#   for(ixP in 1:private$N){
#     private$kappa[ixP] = private$kappa[ixP] + self$sumKappa(ixP) # update human component of kappa
#     private$kappa[ixP] = private$kappa[ixP] / (private$bWeightHuman[ixP]+private$bWeightZoo[ixP]+private$bWeightZootox[ixP]) # normalize by all biting
#   }
# }
#
# # set sumKappa
# MacroPatch$set(which = "public",name = "updateKappa",
#           value = updateKappa,
#           overwrite = TRUE
# )
