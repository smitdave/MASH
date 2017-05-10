#################################################################
#
#   MASH
#   R6-ified
#   Class definition for human
#   Sean Wu
#   December 10, 2016
#
#################################################################


##########################################
# Fire Human Event
##########################################

#' Run a Single Human Event
#'
#' runEvent is a wrapper than extracts event time, parameters, and passes the private env down to the event function to be called.
#' All event functions (FUN) must take exactly four arguments; see below.
#'
#' @param tEvent time the event will fire
#' @param PAR optional list of parameters for FUN
#' @param FUN the function that defines the event (FUN takes four arguments: tEvent, PAR, private, self)
#' @param private the private environment of the human
#' @param self the public environment of the human
#' @return fire a single human event
#' @examples
#' runEvent(tEvent=tEvent,PAR=PAR,FUN=FUN,private=private,self=self)
#' @export
runEvent <- function(tEvent, PAR, FUN, private, self){
  FUN(tEvent, PAR, private, self)
}


##########################################
# Human Life Events
##########################################

# event_maxDeath: the maximum death time event package
event_maxDeath <- function(tEvent = 73000, PAR = NULL){
  list(tEvent = tEvent, PAR = PAR, FUN = death)
}

# death: the death event
death <- function(tEvent, PAR, private, self){
  self$trackHist(tEvent = tEvent, event = "D")
  private$Alive = FALSE
}


##########################################
# Human Class Definition
##########################################

#' MICRO-Human Class Definition
#'
#' This is a generic human being blah blah ...
#'  categorical summary measure \code{A[j]}. This class inherits from \code{\link{GenericModel}} class.
#'  Defines the fitting algorithm for a regression model \code{A[j] ~ W + ...}.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#' @details
#' \itemize{
#' \item{\code{reg}} - .
#' \item{\code{outvar}} - .
#' \item{\code{levels}} - .
#' \item{\code{nbins}} - .
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{new(reg, DataStorageClass.g0, ...)}}{...}
#'   \item{\code{fit(data)}}{...}
#'   \item{\code{predict(newdata)}}{...}
#'   \item{\code{predictAeqa(newdata)}}{...}
#' }
#' @section Active Bindings:
#' \describe{
#'   \item{\code{cats}}{...}
#' }
#' @export
Human <- R6::R6Class(classname="Human",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 #public members
                 public = list(

                   #initializer
                   initialize = function(myID, hhID, bDay){
                     private$myID = myID
                     private$hhID = hhID
                     private$bDay = bDay
                   },

                   #################################################
                   # Accessor Methods
                   #################################################

                   #myID
                   getmyID = function(){
                     return(private$myID)
                   },

                   #hhID
                   gethhID = function(){
                     return(private$hhID)
                   },

                   #bDay
                   getbDay = function(){
                     return(private$bDay)
                   },

                   #eventQ
                   getEventQ = function(){
                     return(private$eventQ)
                   },

                   #Alive
                   getAlive = function(){
                     return(private$Alive)
                   },

                   #queueN
                   getqueueN = function(){
                     return(private$queueN)
                   },

                   # history
                   getHistory = function(){
                     list(
                       events = private$events,
                       eventT = private$eventT
                     )
                   },

                   # Pathogens
                   getPf = function(){
                     return(private$Pathogens$Pf)
                   },

                   getPv = function(){
                     return(private$Pathogens$Pv)
                   },

                   # accessors
                   getPrivate = function(){
                     return(as.list(private))
                   },

                   getPublic = function(){
                     return(as.list(self))
                   },

                   #################################################
                   # Event Queue
                   #################################################

                   #addEvent2Q: add a single event to the event queue in proper location
                   addEvent2Q = function(event){

                     NN = private$queueN
                     tt = sapply(X = private$eventQ,FUN = function(x){x$tEvent})
                     private$queueN = NN + 1
                     ix = which(event$tEvent > tt)
                     if(length(ix)==0){ #if event has already occured at event$tEvent <= tt; advance to front of queue
                       private$eventQ = c(list(event),private$eventQ)
                     } else { #if event will occur at event$tEvent > tt; place at proper position in queue
                       ixn = c(1:NN)[-ix]
                       private$eventQ = c(private$eventQ[ix],list(event),private$eventQ[ixn])
                     }

                   },

                   #rmFirstEventFromQ:
                   rmFirstEventFromQ = function(){
                     private$eventQ = private$eventQ[-1]
                     private$queueN = private$queueN - 1
                   },

                   #oneEvent:
                   oneEvent = function(tPause){
                     tEvent = private$eventQ[[1]]$tEvent #extract time of event
                     PAR = private$eventQ[[1]]$PAR #extract PAR of event
                     FUN = private$eventQ[[1]]$FUN #extact event function
                     runEvent(tEvent=tEvent,PAR=PAR,FUN=FUN,private=private,self=self) #fire event
                     self$rmFirstEventFromQ()
                   },

                   #liveLife:
                   liveLife = function(tPause){
                     while(private$Alive & private$eventQ[[1]]$tEvent < tPause){
                       self$oneEvent(tPause)
                       if(private$queueN == 0){
                         break()
                       }
                     }
                   },

                   #################################################
                   # Pathogen Module-specific Functions
                   #################################################

                   setPathogensObject = function(pathogen){
                     switch(pathogen,
                            PfSI = {private$Pathogens$Pf = pathOBJ_PfSI()}
                            )
                   },

                   #################################################
                   # Auxiliary Functions
                   #################################################

                   trackHist = function(tEvent, event){
                     private$events = c(private$events, event)
                     private$eventT = c(private$eventT, tEvent)
                   }

                 ),

                 #private members
                 private = list(

                   #General Information
                   myID = NULL,
                   hhID = NULL,
                   Alive = TRUE,
                   bDay = NULL,
                   sex = NULL,
                   weight = 0,
                   height = 0,

                   #Event Queue
                   eventQ = list(event_maxDeath()), #event queue
                   queueN = 1, #number of events in queue

                   # Event History
                   events = c("init"),
                   eventT = c(-1),

                   # Pathogens
                   Pathogens = list(
                     Pf = NULL,
                     Pv = NULL
                   )

                 )

) #end class definition
