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
# Generic Event History Management Functions
##########################################

#' Write eventT and events from HUMANS to .json
#'
#' Write HUMANS histories from PfSI module to .json file.
#'
#' @param directory directory; files will be put in directory/OUTPUT/..
#' @param fileName name of the file to write to; directory/OUTPUT/fileName.csv
#' @return nothing
#' @examples
#' writeHumanEvent(directory, fileName)
writeHumanEvent_PfSI <- function(directory, fileName){
  if(!dir.exists(paste0(directory,"OUTPUT"))){
    dir.create(paste0(directory))
    dir.create(paste0(directory,"OUTPUT"))
  }
  if(file.exists(paste0(directory,"OUTPUT/",fileName))){
    stop("writeHumanEvent_PfSI cannot write to a file that already exists!")
  }

  con = file(description = paste0(directory,"OUTPUT/",fileName),open = "wt")

  humanHistories = lapply(HUMANS,function(x){
    list(
      eventT = x$Pathogens$Pf$eventT,
      events = x$Pathogens$Pf$events
    )
  })
  humanID = sapply(HUMANS,function(x){x$myID})
  names(humanHistories) = paste0("human",humanID)
  writeLines(text = jsonlite::toJSON(x = humanHistories,pretty = TRUE),con = con)
  close(con)
}

#' Reimport Human eventT and events from .json
#'
#' Import human histories exported to .json from PfSI module.
#'
#' @param directory directory; files are in directory/OUTPUT/..
#' @return list of histories
#' * eventT: times events occur
#' * events: events
#' @md
#' @examples
#' writeHumanEvent(directory, fileName)
importHumanEvent_PfSI <- function(directory){
  dirFiles = system(command = paste0("ls ",directory,"OUTPUT/"),intern = TRUE)
  humanFile = grep("humanPfSI.json",dirFiles)
  humanHistories = jsonlite::fromJSON(txt = paste0(directory,"OUTPUT/",dirFiles[humanFile]))
  return(humanHistories)
}


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

  par(xpd = T, mar = par()$mar + c(-1,-1,-4,7)) # adjust plotting area for legend

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
