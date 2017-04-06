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
  N = ncol(el4p)-1 # number of sites

  col = ggCol(n = N,alpha = 0.8)
  L1 = el4p[el4p$labels=="L1",-1]
  L1$time = 1:nrow(L1)

  matplot(x = L1[,-which(names(L1)=="time")],type = "l",col = col,lty = 1,ylab="Larval instar stage 1 (L1)")
  smooth =rowMeans(L1[,-which(names(L1)=="time")])
  lo = loess(smooth ~ L1$time)
  lines(lo,lty=1,col="red",lwd=2)
  # need to do it with a kernel smoother.
}
