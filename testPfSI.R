#################################################################
#
#   MASH/MBITES
#   Example Simulation and Analysis of PfSI Module
#   R version
#   Sean Wu
#   April 10, 2017
#
#################################################################

######################################################
# Initialize HUMANS object
######################################################

rm(list=ls())
library(MASH.MBPT)

nH = 1e3 # nH must be divisible by nS for now; fix later.
nS = 100

hhSizes = replicate(n = nS,expr = nH/nS,simplify = TRUE)

hhIx = sapply(X = 1:nS,FUN = function(x,hhSizes){
  ((hhSizes[x]*(x-1))+1) : (hhSizes[x]*x)
},hhSizes = hhSizes)

HUMANS = makeHumans(nH = nH,hhSizes = hhSizes ,hhIx = hhIx)


######################################################
# Initialize PfSI Module
######################################################

PFSI.SETUP(NOISY = FALSE)
PFSI.INIT(PfPR = 0)


######################################################
# Simulate PfSI Module
######################################################

# Queue infections
tMax = 8*365
for(i in 1:nH){
  t=0
  print(paste0("queueing for human ixH: ",i))
  while(t < tMax){
    t = t + rexp(1, 1/20)
    add2Q_simbitePfSI(ixH = i,t = t)
  }
}

# Vaccinate
tMax = 5*365
for(i in 1:round(nH/1.5)){
  print(paste0("vaccinating for human ixH: ",i))
  add2Q_pevaccinatePfSI(ixH = i,t = 730)
  add2Q_treatPfSI(ixH = i,t = 730.1)
}

# Simulating
tMax = 15*365
for(i in 1:nH){
  print(paste0("simulating human ixH: ",i))
  liveLife(ixH = i,tPause = tMax)
}

# PLEASE SET TO YOUR OWN DESIRED FOLDER TO HOLD OUTPUT/.. (this must end in /)
out = "/Users/slwu89/Desktop/mash.out/"
writeHumanEvent_PfSI(directory = out,fileName = "humanPfSI.json")


######################################################
# Analysis and Visualization
######################################################

humanHistories = importHumanEvent_PfSI(directory = out)
history = humanHistories

pfsiTrajectory()




plotPfsiOneTrajectory <- function(ixH, oneHistory, tMax){

  times = oneHistory$eventT[-1]
  events = oneHistory$events[-1]

  # fever
  ixF = which(events == "F")
  if(length(ixF)>0){
    points(x = times[ixF],y = rep(x = ixH,times = length(ixF)),pch=17,cex=0.5,col="red")
  }

  # prophylaxis
  ixP = which(events == "P")
  if(length(ixP)>0){
    points(x = times[ixP],y = rep(x = ixH,times = length(ixP)),pch=16,cex=0.5,col="blue")
  }

  events = events[-union(ixF,ixP)]
  times = times[-union(ixF,ixP)]

  # state trajectory
  if(length(events) > 0){

    for(i in 1:(length(events))){

      if(i == length(events)){ # final trajectory
        if(events[i] == "I"){
          segments(x0 = times[i],y0 = ixH,x1 = tMax,y1 = ixH,col = "red",lwd = 2)
        }
        if(events[i] == "S"){
          segments(x0 = times[i],y0 = ixH,x1 = tMax,y1 = ixH,col = "grey50",lwd = 2)
        }
        if(events[i] == "P"){
          segments(x0 = times[i],y0 = ixH,x1 = tMax,y1 = ixH,col = "blue",lwd = 2)
        }

      } else { # interior trajectory
        if(events[i] == "I"){
          segments(x0 = times[i],y0 = ixH,x1 = times[i+1],y1 = ixH,col = "red",lwd = 2)
        }
        if(events[i] == "S"){
          segments(x0 = times[i],y0 = ixH,x1 = times[i+1],y1 = ixH,col = "grey50",lwd = 2)
        }
        if(events[i] == "P"){
          segments(x0 = times[i],y0 = ixH,x1 = times[i+1],y1 = ixH,col = "blue",lwd = 2)
        }
      }

    } # end for
  } # end state trajectory

}

plotPfsiTrajectory <- function(history){

  tMax = max(sapply(X = history,FUN = function(x){max(x$eventT)}))

  plot(1,type="n",xaxt="n",yaxt="n",ylab="Humans",xlab="Time (Years)",xlim=c(0,tMax),ylim=c(0,length(history)))
  ttMax = tMax/365
  axis(side = 1,at = c(0:ttMax)*365,labels = c(0:ttMax))

  for(ixH in 1:length(history)){
    plotPfsiOneTrajectory(ixH = ixH,oneHistory = history[[ixH]],tMax = tMax)
  }



}

