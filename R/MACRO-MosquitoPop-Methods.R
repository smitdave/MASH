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
MacroMosquitoPop$set(which = "public",name = "getEIP",
          value = getEIP_MacroMosquitoPop,
          overwrite = TRUE
)

getEIP_MacroMosquitoPop <- function(tNow){
  # update later
  return(10L)
}

# Oviposition:
MacroMosquitoPop$set(which = "public",name = "getEIP",
          value = getEIP_MacroMosquitoPop,
          overwrite = TRUE
)

layEggs_MacroMosquitoPop <- function(tNow){
  
}
