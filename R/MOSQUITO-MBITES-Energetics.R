#################################################################
#
#   MASH
#   R6-ified
#   MBITES Energetics Method Definitions
#   Hector Sanchez & Sean Wu
#   May 8, 2017
#
#################################################################

# clearOutput <- function(directory){
#   if(!dir.exists(paste0(directory,"OUTPUT"))){
#     stop("directory does not exist; nothing to clear")
#   }
#   dirFiles = system(command = paste0("ls ",directory,"OUTPUT/"),intern = TRUE)
#   xx = menu(c("Yes", "No"), title=paste0("There are ",length(dirFiles)," files in OUTPUT/.. are you sure you want to delete all?"))
#   if(xx==1){
#     file.remove(paste0(directory,"OUTPUT/",dirFiles))
#   }
# }

#' Initialize Energetics Methods for M-BITES
#'
#' This function initializes generic methods for M-BITES models; please note that the
#' switches for this function modify only the methods that are added to the MosquitoFemale
#' and MosquitoMale classes. Different genotypes still depend on the internal list of parameters
#' to parameterize these functions and functional forms for equations.
#'
#' @param batchSize character switch that should be one of \code{"bms","norm"} for egg batch sizes dependent on bloodmeal size or normally distributed
#' @param eggMatT character switch that should be one of \code{"off","norm"} for egg batch maturation time turned off or normally distributed
#' @param reFeedType character switch that should be one of \code{"pr","egg"} for refeeding as probability based on size of bloodmeal or based on egg batch size.
#'
#'
#' @return modifies the \code{MosquitoFemale} and \code{MosquitoMale} classes.
#' @export
init.mbitesEnergetics <- function(

    SUGAR = TRUE,
    OVERFEED = TRUE,
    REFEED = TRUE,
    batchSize = "bms",
    eggMatT = "off",
    reFeedType = "pr"

  ){

  ##########################################
  # Sugar Energetics
  ##########################################

  if(SUGAR){
    # energetics: manage sugar-based mosquito energetics
    Mosquito$set(which = "public",name = "energetics",
                 value = function(){
                   if(self$isAlive()){

                     energyNew = private$energy - private$PAR$S.u
                     private$energy = max(energyNew,0)

                     if(runif(1) < 1-self$pEnergySurvival()){
                       private$stateNew = "D"
                     } else {
                       self$queueSugarBout()
                     }

                   }
                 }
    )

    # pEnergySurvival: Incremental mortality as a function of energy reserves
    Mosquito$set(which = "public",name = "pEnergySurvival",
                 value = function(){
                   return(exp(private$PAR$S.a*private$energy)/(S.b + exp(private$PAR$S.a*private$energy)))
                 }
    )

    # pSugarBout: probability to queue sugar bout as function of energy reserves
    Mosquito$set(which = "public",name = "pSugarBout",
                 value = function(){
                   return((2+private$PAR$S.sb)/(1+private$PAR$S.sb)-exp(private$PAR$S.sa*private$energy)/(private$PAR$S.sb+exp(private$PAR$S.sa*private$energy)))
                 }
    )

    # queueSugarBout: queue sugar feeding bout based on energy reserves
    Mosquito$set(which = "public",name = "queueSugarBout",
                 value = function(){
                   if(runif(1) < self$pSugarBout){
                     private$stateNew = "S"
                   }
                 }
    )
  }

  ##########################################
  # Bloodmeal Energetics
  ##########################################

  # rBloodMealSize: random size of blood meal
  MosquitoFemale$set(which = "public",name = "rBloodMealSize",
               value = function(){
                 return(rbeta(n=1,private$PAR$bm.a,private$PAR$bm.b))
               }
  )

  # bloodEnergetics: manage blood-based mosquito energetics
  MosquitoFemale$set(which = "public",name = "bloodEnergetics",
               value = function(){

                 private$energy = max(1,(private$energy + private$PAR$B.energy))

                 if(!private$mature){
                   private$energyPreG = private$energyPreG - private$PAR$preGblood
                   if(private$energyPreG <= 0){
                     private$mature = TRUE
                   }
                 }

               }
  )

  # bloodMeal: take a blood meal on a host
  MosquitoFemale$set(which = "public",name = "bloodMeal",
                     value = function(){

                       private$bmSize = self$rBloodMealSize()
                       self$bloodEnergetics()
                       if(private$PAR$OVERFEED){ # overfeed
                         self$overFeed()
                         if(!self$isAlive()){
                           return(NULL)
                         }
                       }
                       # mated and mature mosquitoes produce eggs
                       if(private$mated & private$mature){
                         private$batch = self$BatchSize()
                         private$eggT = private$tNow + self$eggMaturationTime()
                       }

                     }
  )

  ##############################
  #  Overfeed
  ##############################

  if(OVERFEED){

    MosquitoFemale$set(which = "public",name = "pOverFeed",
                       value = function(){
                         return(exp(private$PAR$of.a*private$bmSize)/(private$PAR$of.b + exp(private$PAR$of.a*private$bmSize)))
                       }
    )

    MosquitoFemale$set(which = "public",name = "overFeed",
                       value = function(){
                         if(runif(1) < self$pOverFeed){
                           private$stateNew = "D"
                         }
                       }
    )

  }

  ##############################
  #  Refeed
  ##############################

  if(REFEED){

    if(reFeedType == "pr"){

      ##############################
      #  reFeedF.Pr
      ##############################

      MosquitoFemale$set(which = "public",name = "pReFeed",
                         value = function(){
                           return((2+private$PAR$rf.b)/(1+private$PAR$rf.b) - exp(private$PAR$rf.a*private$bmSize)/(private$PAR$rf.b + exp(private$PAR$rf.a*private$bmSize)))
                         }
      )

      MosquitoFemale$set(which = "public",name = "reFeed",
                         value = function(){

                          if(self$isAlive()){
                            if(runif(1) < self$pReFeed()){
                              private$stateNew = "B"
                            } else {
                              private$stateNew = "L"
                            }
                          }

                         }
      )

    } else if(reFeedType == "egg"){

      ##############################
      #  reFeedF.Egg
      ##############################

      MosquitoFemale$set(which = "public",name = "reFeed",
                         value = function(){

                           if(self$isAlive()){
                             if(private$tNow > private$eggT & private$eggP > private$PAR$eggP.min){
                               private$stateNew = "L"
                             } else {
                               private$stateNew = "B"
                             }
                           }

                         }
      )

    } else {
      stop("if REFEED enabled please select reFeedType from {'pr','egg'}")
    }

  }

  ######################################
  #  Egg Batch
  ######################################

  if(batchSize == "bms"){

    MosquitoFemale$set(which = "public",name = "BatchSize",
                       value = function(){
                         return(private$bmSize*private$PAR$maxBatch)
                       }
    )

  } else if(batchSize == "norm"){

    MosquitoFemale$set(which = "public",name = "BatchSize",
                       value = function(){
                         return(ceiling(rnorm(1, private$PAR$bs.m, private$PAR$bs.v)))
                       }
    )

  } else {
    stop("must specify batchSize in {'bms','norm'}")
  }

  if(eggMatT == "off"){

    MosquitoFemale$set(which = "public",name = "eggMaturationTime",
                       value = function(){
                         return(0)
                       }
    )

  } else if(eggMatT == "norm"){

    MosquitoFemale$set(which = "public",name = "eggMaturationTime",
                       value = function(){
                         max(0,rnorm(1, private$PAR$emt.m, private$PAR$emt.V))
                       }
    )

  } else {
    stop("must specify eggMatT in {'off','norm'}")
  }

  MosquitoFemale$set(which = "public",name = "makeBatches",
                     value = function(){
                       # #. makeBatches: make an egg batch and deposit on the landscape
                       # #batch: size of egg batch
                       # #ixM: mosquito ID
                       # #tNow: current time
                       # addBatch2Q(M$batch, M$ix, M$tNow, M$id, M$sire) # aquaticEcology.R
                       print(paste0("mosy: ",private$id,", is calling makeBatches() at: ",private$tNow))
                     }
  )


}
