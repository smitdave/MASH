#################################################################
#
#   MASH/MBITES
#   Timing Functions
#   R version
#   Sean Wu
#   January 25, 2017
#
#################################################################


timingExponential <- function(M,P){
  if(isActive(M)){
    rate = with(P,{
      switch(M$state,
        B = B.t,
        F = F.t,
        R = R.t,
        L = L.t,
        O = O.t,
        S = S.t,
        M = M.t
    )})
    M$tNext = M$tNow + rexp(n=1,rate=1/rate)
  }
  return(M)
}
