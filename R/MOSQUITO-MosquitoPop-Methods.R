#################################################################
#
#   MASH
#   R6-ified
#   MICRO Methods for mosquito population
#   David Smith, Hector Sanchez, Sean Wu
#   July 19, 2017
#
#################################################################

# #' MACRO: Update \code{Human} kappa For a Patch
# #'
# #' Add my contribution to kappa to current patch (current value of \code{location}).
# #' This method is bound to \code{Human$sumKappa()}
# #'
# #' @param a parameter
# #' @return does stuff
# #' @examples
# #' some_function()
# MacroHuman_sumKappa <- function(){
#   if(private$Pathogens$Pf$get_infected()){
#     self$get_PatchesPointer()$accumulate_kappa(kappa = (private$bWeight*private$Pathogens$Pf$get_c()), ix = private$location)
#   } else {
#     self$get_PatchesPointer()$accumulate_kappa(kappa = 0, ix = private$location)
#   }
# }
