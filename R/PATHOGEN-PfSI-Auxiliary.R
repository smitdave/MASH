#################################################################
#
#   MASH
#   R6-ified
#   PfSI Auxiiary
#   Define auxiliary and plotting routines
#   Sean Wu
#   May 21, 2017
#
#################################################################

#' Initialize Auxiliary PfSI Module Methods (Pathogen)
#'
#' \code{PfSI.Auxiliary} is called internally by \code{\link{PfSI.Setup}} to initialize functions auxiliary to the main PfSI module definition.
#'
#' @param Pf_c 0.15; transmission efficiency: infected human to mosquito
#' @return Defines a field (list) PfSI_PAR in \code{\link{HumanPop}} and public methods in \code{\link{Human}}
#' @examples
#'
#' @export
PfSI.Auxiliary <- function(){

  print(paste0("initializing PfSI auxiliary methods"))

  ##########################################
  # 'HumanPop' Class Methods
  ##########################################

  # set PfSI_PAR for a HumanPop; this is useful for simulating multiple populations with different parameter values
  HumanPop$set(which = "public",name = "set_PfSI_PAR",
            value = function(PfSI_PAR){
              for(ixH in 1:self$nHum){
                private$pop[[ixH]]$set_PfSI_PAR(PfSI_PAR)
              }
            }
  )

}


####################################################################################
# Plotting
####################################################################################

#' plot_PfSI_oneTrajectory
#'
#' write me
#'
#' @param a parameter
#' @return do something
#' @examples
#' plot_PfSI_oneTrajectory()
plot_PfSI_oneTrajectory <- function(ixH, oneHistory, tMax){

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

  # vaccination
  ixV = grep(pattern = "vaxx$",x = events)
  ixW = grep(pattern = "wane$",x = events)
  if(length(ixV)>0){
    points(x = times[ixV],y = rep(x = ixH,times = length(ixV)),pch=18,cex=0.5,col="darkorange")
  }
  if(length(ixW)>0){
    points(x = times[ixW],y = rep(x = ixH,times = length(ixW)),pch=18,cex=0.5,col="darkorange4")
  }

  events = events[-c(ixF,ixV,ixW)]
  times = times[-c(ixF,ixV,ixW)]

  # state trajectory
  if(length(events) > 0){

    for(i in 1:(length(events))){

      if(i == length(events)){ # final trajectory
        if(events[i] == "I"){
          segments(x0 = times[i],y0 = ixH,x1 = tMax,y1 = ixH,col = "red",lwd = 2)
        }
        if(events[i] == "S"){
          segments(x0 = times[i],y0 = ixH,x1 = tMax,y1 = ixH,col = "grey70",lwd = 2)
        }
        if(events[i] == "P"){
          segments(x0 = times[i],y0 = ixH,x1 = tMax,y1 = ixH,col = "blue",lwd = 2)
        }

      } else { # interior trajectory
        if(events[i] == "I"){
          segments(x0 = times[i],y0 = ixH,x1 = times[i+1],y1 = ixH,col = "red",lwd = 2)
        }
        if(events[i] == "S"){
          segments(x0 = times[i],y0 = ixH,x1 = times[i+1],y1 = ixH,col = "grey70",lwd = 2)
        }
        if(events[i] == "P"){
          segments(x0 = times[i],y0 = ixH,x1 = times[i+1],y1 = ixH,col = "blue",lwd = 2)
        }
      }

    } # end for
  } # end state trajectory

}

#' plot_PfSI
#'
#' write me
#'
#' @param a parameter
#' @return do something
#' @examples
#' plot_PfSI()
#' @export
plot_PfSI <- function(history){

  tMax = max(sapply(X = history,FUN = function(x){max(x$eventT)}))

  plot(1,type="n",xaxt="n",yaxt="n",ylab="Humans",xlab="Time (Years)",xlim=c(0,tMax),ylim=c(0,length(history)))
  ttMax = tMax/365
  axis(side = 1,at = c(0:ttMax)*365,labels = c(0:ttMax))

  for(ixH in 1:length(history)){
    plotPfsiOneTrajectory(ixH = ixH,oneHistory = history[[ixH]],tMax = tMax)
  }

}
