#################################################################
#
#   MASH
#   R6-ified
#   PfSI Auxiiary
#   Define auxiliary and plotting routines
#   David Smith, Hector Sanchez, Sean Wu
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
PfSI.Auxiliary.Setup <- function(){

  print(paste0("initializing PfSI auxiliary methods"))

  ##########################################
  # 'HumanPop' Class Methods
  ##########################################

  # set PfSI_PAR for a HumanPop; this is useful for simulating multiple populations with different parameter values
  HumanPop$set(which = "public",name = "set_PfSI_PAR",
            value = function(PfSI_PAR){
              for(ixH in 1:self$nHumans){
                private$pop[[ixH]]$set_PfSI_PAR(PfSI_PAR)
              }
            }
  )

}


####################################################################################
# PfSI history to continuous time occupancy vector
####################################################################################

#' PfSI: Calculate State Space Occupancy Vector
#'
#' From history output of a \code{\link{HumanPop}}, convert individual histories in to a continuous time occupany vector.
#' The PfSI module has the following state space:
#' * S: susceptible
#' * I: infected
#' * F: fever
#' * P: chemoprophylactic protection
#' * PEvaxx: begin PE vaccination protection
#' * PEwane: end PE vaccination protection
#' * GSvaxx: begin GS vaccination protection
#' * GSwane: end GS vaccination protection
#' @md
#'
#' @param history human event histories from \code{HumanPop$get_History()}
#' @param parallel run in parallel
#' @return list
#' @examples
#' util_PfSIHistory()
#' @export
util_PfSIHistory <- function(history, parallel = TRUE){

  # number of humans
  N = length(history)

  # create time bin for each discrete event
  eventTimes = sort(unlist(lapply(history,function(x){x$eventT})))
  timeBins = c(min(eventTimes),Filter(f = function(y){y > min(eventTimes)},x = eventTimes))

  # create empty time series (each element is slice of occupancy vector at that point in time)
  timeSeries = lapply(X = timeBins, oneTime, female = female) # bins

  initState = unique(sapply(cohortTraj,function(x){x$state[1]})) # initial state
  if(length(initState)>1){ # sanity check
    stop("more than one initial state")
  }

  # set initial occupancy vector
  switch(initState,
         "F" = for(i in 1:length(timeSeries)){timeSeries[[i]]$F <- cohortN},
         "B" = for(i in 1:length(timeSeries)){timeSeries[[i]]$B <- cohortN},
         "R" = for(i in 1:length(timeSeries)){timeSeries[[i]]$R <- cohortN},
         "L" = for(i in 1:length(timeSeries)){timeSeries[[i]]$L <- cohortN},
         "O" = for(i in 1:length(timeSeries)){timeSeries[[i]]$O <- cohortN},
         "M" = for(i in 1:length(timeSeries)){timeSeries[[i]]$M <- cohortN},
         "S" = for(i in 1:length(timeSeries)){timeSeries[[i]]$S <- cohortN},
         "D" = stop("initial state should not be D")
  )

  for(ixM in 1:cohortN){ # iterate over mosquitoes

    time = cohortTraj[[ixM]]$time
    state = cohortTraj[[ixM]]$state
    for(ixT in 2:length(time)){
      tIter = which(timeBins >= time[ixT]) # times over which to propagate current state change
      for(i in tIter){
        timeSeries[[i]][[state[ixT-1]]] = timeSeries[[i]][[state[ixT-1]]] - 1 # mosy ixM no longer in state[ixT-1] in all times ixT
        timeSeries[[i]][[state[ixT]]] = timeSeries[[i]][[state[ixT]]] + 1 # mosy ixM in state[ixT] is propagated forward
      }
    }

  }

  return(timeSeries)

}



#' PfSI: Empty Occupancy Vector
#'
#' Make an empty occupancy vector at a single time slice for the \code{PfSI} module.
#'
#' @param time current time slice
#' @return list
#' @examples
#' util_PfSISlice(time = 0)
util_PfSISlice <- function(time){
  list(
    time = time,
    S = 0,
    I = 0,
    F = 0,
    P = 0,
    PEvaxx = 0,
    PEwane = 0,
    GSvaxx = 0,
    GSwane = 0
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
plot_PfSIOneTrajectory <- function(ixH, oneHistory, tMax){

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
    plot_PfSIOneTrajectory(ixH = ixH,oneHistory = history[[ixH]],tMax = tMax)
  }

}
