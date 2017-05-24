#################################################################
#
#   MASH
#   R6-ified
#   MACRO MosquitoPop Class Methods
#   David Smith, Hector Sanchez, Sean Wu
#   May 22, 2016
#
#################################################################


#################################################################
# Methods
#################################################################

# for now just add methods straight to class object; can make a seperate MACRO.MosquitoPop.Setup() later if necessary.

# EIP:
getEIP_MacroMosquitoPop <- function(tNow){
  # update later
  return(10L)
}

MacroMosquitoPop$set(which = "public",name = "getEIP",
          value = getEIP_MacroMosquitoPop,
          overwrite = TRUE
)

# Oviposition:
layEggs_MacroMosquitoPop <- function(tNow){
  for(ixP in 1:self$get_PatchesPointer()$get_N()){

  }
}

MacroMosquitoPop$set(which = "public",name = "getEIP",
          value = layEggs_MacroMosquitoPop,
          overwrite = TRUE
)
