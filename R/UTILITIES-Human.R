#################################################################
#
#   MASH/MBITES
#   Human Profile
#   R version
#   Sean Wu
#   January 24, 2017
#
#################################################################


##########################################
# PfSI Trajectories
##########################################

pfsiOneTrajectory <- function(ixH){

  eT = HUMANS[[ixH]]$Pathogens$Pf$eventT[-1]
  eV = HUMANS[[ixH]]$Pathogens$Pf$events[-1]
  ixF = which(eV == "F") # fever
  if(length(ixF >0)){
    fevers = eT[ixF]
    eT = eT[-ixF]
    eV = eV[-ixF]
    points(fevers, 0*fevers+ixH, pch = 17,cex = 0.5, col = "red")
  }

  ixP = which(eV == "P") # prophylaxis
  if(length(ixP >0)){
    treated = eT[ixP]
    points(treated, 0*treated+ixH, pch = 16, cex = .5, col = "blue")
  }

  if(length(eT[-1]) > 0){
    for(i in 1:length(eT[-1])){
      if(eV[i] == "I"){
        segments(eT[i],ixH,eT[i+1],ixH, col = "red", lwd = 2)
      }
      if(eV[i] == "S"){
        segments(eT[i],ixH,eT[i+1],ixH, col = grey(0.5), lwd = 2)
      }
      if(eV[i] == "P"){
        segments(eT[i],ixH,eT[i+1],ixH, col = "blue", lwd = 2)
      }
    }
  }

}

pfsiTrajectory <- function(){

  par(xpd = T, mar = par()$mar + c(0,0,0,7)) # adjust plotting area for legend

  rng = c(0,1)
  for(i in 1:length(HUMANS)){
    rng = range(rng,HUMANS[[i]]$Pathogens$Pf$eventT[-1])
  }

  plot(c(0,tMax), c(0.9,length(HUMANS)+0.1), type = "n", xaxt = "n", yaxt = "n", ylab = "Humans", xlab = "Time")
  ttMax = tMax/365
  axis(1, c(0:ttMax)*365, c(0:ttMax))

  for(i in 1:length(HUMANS)){
    pfsiOneTrajectory(i)
  }

  legend(x = tMax+(tMax*0.06), y = (length(HUMANS)+0.1) ,legend = c("Fever","Treatment","Infected","Susceptible","Prophylaxis"),
         pch = c(17,16,NA,NA,NA), lty = c(NA,NA,1,1,1), col = c("red", "blue", "red", grey(0.5), "blue"), bty = "n")

  par(xpd = F, mar=c(5, 4, 4, 2) + 0.1)
}



# N = length(HUMANS)
# maxT = ceiling(max(sapply(HUMANS,function(x){x$Pathogens$Pf$eventT[length(x$Pathogens$Pf$eventT)]})))




