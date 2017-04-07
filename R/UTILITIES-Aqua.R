#################################################################
#
#   MASH/MBITES
#   Visualization and Analysis of logged data for AQUATIC ECOLOGY modules
#   R version
#   Sean Wu
#   April 6, 2017
#
#################################################################


#################################################################
# EL4P Basic
#################################################################


#' Plot Imported EL4P .csv Data
#'
#' This is a utility to format logged EL4P data after it has been imported by importEL4P
#'
#' @param el4p a data frame (output of importEL4P)
#' @return formatted data object; a list of sites, each site has named elements L1, L2, L3, L4, P
#' @examples
#' formatEL4P(el4p)
plotEL4P <- function(el4p){

  par(mfrow=c(2,3))

  N = ncol(el4p)-1 # number of sites

  col = ggCol(n = N,alpha = 0.8)

  # L1 stage
  L1 = el4p[el4p$labels=="L1",-1]
  L1$time = 1:nrow(L1)
  matplot(x = L1[,-which(names(L1)=="time")],type = "l",col = col,lty = 1,
          ylab="Larval instar stage 1 (L1)",xlab="Time (Days)")
  nwSmooth = ksmooth(x = 1:nrow(L1),y=as.matrix(L1))
  lines(x = nwSmooth$x,y = nwSmooth$y,col = "grey20",lwd=2.5)

  # L2 stage
  L2 = el4p[el4p$labels=="L2",-1]
  L2$time = 1:nrow(L2)
  matplot(x = L2[,-which(names(L2)=="time")],type = "l",col = col,lty = 1,
          ylab="Larval instar stage 1 (L2)",xlab="Time (Days)")
  nwSmooth = ksmooth(x = 1:nrow(L2),y=as.matrix(L2))
  lines(x = nwSmooth$x,y = nwSmooth$y,col = "grey20",lwd=2.5)

  # L3 stage
  L3 = el4p[el4p$labels=="L3",-1]
  L3$time = 1:nrow(L3)
  matplot(x = L3[,-which(names(L3)=="time")],type = "l",col = col,lty = 1,
          ylab="Larval instar stage 1 (L3)",xlab="Time (Days)")
  nwSmooth = ksmooth(x = 1:nrow(L3),y=as.matrix(L3))
  lines(x = nwSmooth$x,y = nwSmooth$y,col = "grey20",lwd=2.5)

  # L4 stage
  L4 = el4p[el4p$labels=="L4",-1]
  L4$time = 1:nrow(L4)
  matplot(x = L4[,-which(names(L4)=="time")],type = "l",col = col,lty = 1,
          ylab="Larval instar stage 1 (L4)",xlab="Time (Days)")
  nwSmooth = ksmooth(x = 1:nrow(L4),y=as.matrix(L4))
  lines(x = nwSmooth$x,y = nwSmooth$y,col = "grey20",lwd=2.5)

  # P stage
  P = el4p[el4p$labels=="P",-1]
  P$time = 1:nrow(P)
  matplot(x = P[,-which(names(P)=="time")],type = "l",col = col,lty = 1,
          ylab="Larval instar stage 1 (P)",xlab="Time (Days)")
  nwSmooth = ksmooth(x = 1:nrow(P),y=as.matrix(P))
  lines(x = nwSmooth$x,y = nwSmooth$y,col = "grey20",lwd=2.5)

  par(mfrow=c(1,1))
}
