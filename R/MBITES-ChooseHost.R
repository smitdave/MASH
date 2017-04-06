#################################################################
#
#   MASH/MBITES
#   Choosehost routines
#   R version
#   Sean Wu
#   January 25, 2017
#
#################################################################


#chooseHost: mosquito host choosing routine
#M: focal mosquito
chooseHost <- function(M){

  if(M$inPointSet != "f"){ #check M is in a feeding site
    stop(paste0("chooseHost error; mosy ",M$id," inPointSet: ",M$inPointSet," , not in a feeding site"))
  }

  with(LANDSCAPE$feedSites[[M$ix]]$riskList,{

    pTmIx = pTm
    whoIx = who
    wIx = w

    #non-human hosts
    if(nO > 0){
      for(i in 1:nO){
        pTmIx = c(pTmIx,1)
        whoIx = c(whoIx,other[[i]]$typeID)
        wIx = c(wIx,other[[i]]$w)
      }
    }

    M$hostID = sample(x = whoIx,size = 1,prob = wIx*pTmIx) #select a host
    return(M)
  })

}
