#################################################################
#
#   MASH
#   R6-ified
#   SimBite module for PfMOI
#   David Smith, Hector Sanchez, Sean Wu
#   June 9, 2017
#
#################################################################

# ###################################################################
# # From infectious bite to infection
# ###################################################################

#
# Pf0=list()
# Pf0$spz = 1
# mPf0 = list(damID=0,ixM=0,tm=0,xm=0,ym=0)
# Pf0$mPf[[1]] = mPf0
#
# add2Q_simbitePfMOI = function(ixH, t, PAR=Pf0){
#   addEvent2Q(ixH, event_simbitePfMOI(t))
# }
#
# event_simbitePfMOI = function(t, PAR=Pf0){
#   if(NOISY == TRUE) print("adding simbite")
#   list(t=t, PAR=Pf0, F=simbite_PfMOI, tag="simbite_PfMOI")
# }
#
# simbite_PfMOI = function(ixH, t, x, y, ixM, PAR=Pf0){with(PAR,{
#   probeHost_PfMOI(ixH,t,x,y,ixM,Pf0)
# })}
#
