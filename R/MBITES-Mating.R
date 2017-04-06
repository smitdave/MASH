#################################################################
#
#   MASH/MBITES
#   MBITES Female Mating Routines
#   MBITES-Swarming.R defines male behaviors
#   R version
#   Sean Wu
#   January 27, 2017
#
#################################################################

# chooseMate: focal mosquito M chooses a mate
chooseMate <- function(M,P){

  # if matingQ empty fly away
  if(all(is.na(LANDSCAPE$swarmSites[[M$ix]]$matingQ$id))){
    M$lspot = "l"
    return(M)
  }

  if(rbinom(1,1,P$M.s)){
    ixM = sample(x = na.omit(LANDSCAPE$swarmSites[[M$ix]]$matingQ$id), size = 1)
    M$sire = ixM
    M$mated = TRUE
    M$stateNew = sample(x = c("F","D"), size = 1, prob = c(P$M.s,1-P$M.s))
  }

  return(M)
}
