####################################################################################
#
#   MASH
#   R6-ified
#   MICRO Landscape Utility Functions
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   July 18, 2017
#
####################################################################################


##########################################
# Auxiliary Functions
##########################################

#' Sample Equally from Color Space
#'
#' This function is a low-level utility to sample at equal points from the color wheel to produce ggplot2 color scheme.
#'
#' @param n number of colors to sample
#' @param alpha opacity
#' @return a vector of colors in hex format
#' @examples
#' ggCol_utility(n=10, alpha = 0.5)
#' @export
ggCol_utility <- function(n, alpha = 1) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}

#' Sample Equally from Color Space with Offset
#'
#' This function is a low-level utility to sample at equal points from the color wheel to produce ggplot2 color scheme.
#' This is a modification of \code{\link{ggCol_utility}} with a simple offset where sampling begins at 1 + offset.
#'
#' @param n number of colors to sample
#' @param offset offset from 1
#' @param alpha opacity
#' @return a vector of colors in hex format
#' @examples
#' ggColOffset_utility(n=10, offset = 2, alpha = 0.5)
#' @export
ggColOffset_utility <- function(n,offset,alpha=1){
  hues = seq(15, 375, length = n + 1 + offset)
  hcl(h = hues,l = 65,c = 100, alpha = alpha)[(1+offset):(offset+n)]
}

#' Brighten or Darken Colors
#'
#' With input of hcl colors (hex code), brighten or darken by a factor
#'
#' @param color vector of hcl colors
#' @param factor factor to brighten or darken colors
#' @param bright logical variable to brighten or darken
#' @param alpha opacity
#' @return a vector of colors in hex format
#' @examples
#' colLuminosity_utility(color=MASH::ggCol_utility(n=5), factor = 1.15, bright = TRUE)
#' @export
colLuminosity_utility <- function(color,factor,bright,alpha=NULL){

  if(!is.logical(bright)){ #sanity check
    stop("i don't know if you wan't me to make your color brighter or darker!")
  }

  col = col2rgb(color,alpha=FALSE) #convert to rgba color space
  if(bright){
    col = col*factor
  } else {
    col = col/factor
  }
  if(!is.null(alpha)){ #adjust alpha if specified
    rbind(col,alpha = 255 * alpha)
  }
  col = rgb(t(as.matrix(apply(col, 1, function(x) if (x > 255) 255 else x))), maxColorValue=255)
  return(col)
}


# emergeSeasonsPlot <- function(){
#   pcol = viridis(n = LANDSCAPE$nA)
#   ylim = range(sapply(LANDSCAPE$aquaSites,function(x){x$season}))
#   plot(1:365,type="n",ylim = ylim, ylab = expression(lambda[i]), xlab = "Time (Days)")
#   grid()
#   for(ix in 1:LANDSCAPE$nA){
#     lines(1:365, LANDSCAPE$aquaSites[[ix]]$season, col=pcol[ix])
#   }
# }


# ##########################################
# # Static Landscape Plot
# ##########################################
#
# #setup_plot will print the static landscape surface
# #offset controls the offset on maximum/minimum XY coordinates due to plotting error
# #bgcol is the background color of the plot; default is #f2ffe6
# staticLandscapePlot <- function(offset=0.05,bgCol="grey20",cex=0.75){
#
#   feedXY = t(sapply(LANDSCAPE$feedSites,function(x){x$siteXY}))
#   aquaXY = t(sapply(LANDSCAPE$aquaSites,function(x){x$siteXY}))
#   sugarXY = t(sapply(LANDSCAPE$sugarSites,function(x){x$siteXY}))
#   mateXY = t(sapply(LANDSCAPE$swarmSites,function(x){x$siteXY}))
#
#   #set up plotting options
#   if(!exists(x = "defaultPar",where = .GlobalEnv)){
#     # if does not exist, save default graphical parameters
#     .GlobalEnv$defaultPar = par()
#   }
#   par(bg=bgCol,mar=c(0,0,0,0),mgp=c(0,0,0))
#
#   #colors
#   setup_col = ggCol(n=4)
#
#   #set up empty grid
#   plot(1,type="n",axes=F,frame.plot=F,ann=F,xlim=c(LANDSCAPE$xLim[1]-offset,LANDSCAPE$xLim[2]+offset),
#        ylim=c(LANDSCAPE$yLim[1]-offset,LANDSCAPE$yLim[2]+offset))
#
#   points(x = feedXY, pch = 15, col = setup_col[1], cex = cex) #draw feeding sites
#   points(x = aquaXY, pch = 16, col = setup_col[2], cex = cex) #draw aquatic habitats
#   points(x = sugarXY, pch = 17, col = setup_col[3], cex = cex) #draw sugar sources
#   points(x = mateXY, pch = 18, col = setup_col[4], cex = cex) #draw mating sites
#
#   legend(x = "topleft", legend = c("Feeding Site","Aquatic Habitat","Sugar Source","Mating Site"),pch = 15:18,
#          col = setup_col, bty = "n", text.col = "grey80")
#
#   par(bg=.GlobalEnv$defaultPar$bg,mar=.GlobalEnv$defaultPar$mar,mgp=.GlobalEnv$defaultPar$mgp)
# }
