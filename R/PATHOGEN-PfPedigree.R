#################################################################
#
#   MASH/MBITES
#   PfPedigree
#   R version
#   Sean Wu
#   March 13, 2017
#
#################################################################

#################################################################
#
#   Generic definitions of variables:
#
#   tStart  : when parasites emerge from the liver
#   tBite   : time of infectious bite
#   tMosy   : time mosquito vector initially infected
#   ixH     : index of human host
#   ixM     : index of mosquito vector
#   ixS     : site of infectious bite
#   PfID    : ID of new Pf infection
#
#   To initialize PfPedigree we must initialize the following:
#   initPfPedigree(n, PfPedigree_XX)
#   makePfPedigree = makePfPedigree_XX
#   makePfM = makePfM_XX
#   getPfParent = getPfParent_XX (here XX is one of SI, States, etc)
#
#################################################################

############################################################################
# Generic PfPedigree management functions
############################################################################

# NOTE: PfPedigree_XX should be global function ie; PfPedigree_XX <<- PfPedigree_full

# initPfPedigree: generic function to initialize the PfPedigree
# n: initial size of PfPedigree
# PfPedigree_func: version of PfPedigree_XX used
# ...: additional named parameters for PfPedigree_func
initPfPedigree <- function(n, PfPedigree_XX, ...){
  args = as.list(substitute(list(...)))[-1L]
  PfPedigree <<- replicate(n = n,expr = do.call(PfPedigree_XX,args),simplify = FALSE)
}

addPf2Pedigree <- function(tStart, tBite, ixH, ixM, ixS, PfM){
  thisPf = makePfPedigree(tStart, tBite, ixH, ixM, ixS, PfM) # make the new pedigree object
  if(is.null(thisPf)){return(NULL)} # to interface with PfPedigree_NULL
  emptyIx = which(sapply(PfPedigree,function(x){x$empty})) # find empty indices
  if(length(emptyIx)<2){ # extend pedigree if necessary
    extendPfPedigree()
    emptyIx = which(sapply(PfPedigree,function(x){x$empty}))
  }
  PfPedigree[[emptyIx[1]]] <<- thisPf # insert the new pedigree
}

extendPfPedigree <- function(offset = NULL){
  if(is.null(offset)){
    offset = ceiling(length(PfPedigree)*1.5)
  }

  PfPedigree <<- c(PfPedigree,replicate(n = offset,expr = PfPedigree_XX(),simplify = FALSE))
}

# asDfPfPedigree: returns the PfPedgiree as a data.frame
asDfPfPedigree <- function(){
  Reduce("rbind",PfPedigree,init=NULL)
}


############################################################################
# getPfParent_XX functions:
#
# for PfSI module this will only return a single pfid but in general
# the damID and sireID could be a vector of male and female gametocyte IDs
# ingested by the mosquito during host to vector transmission during a
# blood meal.
#
############################################################################

getPfParent_SI <- function(ixH){
  HUMANS[[ixH]]$Pathogens$Pf$pfid
}


############################################################################
# PfPedigree Modules:
#
# makePfM_XX: make a PfM object, at a minimum must include damID and sireID
# PfPedigree_XX: the pedigree slot; contains all information for that pedigree
# makePfPedigree_XX: during infectious bite, make a new pedigree for that variant
#
############################################################################


######################
# PfPedigree: full
######################

# makePfM_full: called from infectMosquito_XX
makePfM_full <- function(ixH, tBite, ixS){
  damID  = getPfParent(ixH)
  sireID = getPfParent(ixH)
  list(
    infected = TRUE,
    PfM = list(tm=tBite, ixS=ixS, ixH=ixH, damID=damID, sireID=sireID)
  )
}

# PfPedigree_full:
PfPedigree_full <- function(tStart=-1, tBite=-1, tMosy=-1, ixH=-1, ixM=-1, ixS=-1, damID=NULL, sireID=NULL, PfID=NULL, empty=TRUE){
  list(
    tStart = tStart,    # time of start of infection
    tBite  = tBite,     # time of infectious bite
    tMosy  = tMosy,     # time mosquito initially infected
    ixH    = ixH,       # index of human host
    ixS    = ixS,       # site of infectious bite
    ixM    = ixM,       # index of mosquito vector
    damID  = damID,     # vector of female gametocyte IDs
    sireID = sireID,    # vector of male gametocyte IDs
    PfID   = PfID,      # PfID; every infection that makes it to bloodstream is a new clonal variant
    empty  = empty      # is this a prealloc slot or does it have a pedigree?
  )
}

# makePfPedigree_full: called from addPf2Pedigree; called from infectiousBite_XX
makePfPedigree_full <- function(tStart, tBite, ixH, ixM, ixS, PfM){
  with(PfM,{
    PfPedigree_full(tStart, tBite, tm, ixH, ixM, ixS, damID, sireID, empty = FALSE)
  })
}

######################
# PfPedigree: null
######################

makePfM_NULL <- function(ixH, tBite, ixS){
  list(infected=TRUE, PfM=NULL)
}

PfPedigree_NULL <- function(){
  NULL
}

makePfPedigree_NULL <- function(tStart, tBite, ixH, ixM, ixS, PfM){
  NULL
}
