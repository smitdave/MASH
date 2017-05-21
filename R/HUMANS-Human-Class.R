#################################################################
#
#   MASH
#   R6-ified
#   Class definition for human
#   Sean Wu
#   May 19, 2016
#
#################################################################


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

                       #initialize
                       initialize = function(myID, hhID, bDay){
                         private$myID = myID
                         private$hhID = hhID
                         private$bDay = bDay
                         private$eventQ[[1]] = self$event_maxDeath()
                       },

                       #################################################
                       # Accessor Methods
                       #################################################

                       #myID
                       get_myID = function(){
                         return(private$myID)
                       },

                       #hhID
                       get_hhID = function(){
                         return(private$hhID)
                       },
                       set_hhID = function(hhID){
                         private$hhID = hhID
                       },

                       #bDay
                       get_bDay = function(){
                         return(private$bDay)
                       },

                       #eventQ
                       get_EventQ = function(){
                         return(private$eventQ)
                       },

                       #Alive
                       get_Alive = function(){
                         return(private$Alive)
                       },
                       set_Alive = function(Alive){
                        #  if(!is.logical(alive)){stop("alive must be boolean/logical value")}
                         private$Alive = Alive
                       },

                       #queueN
                       get_queueN = function(){
                         return(private$queueN)
                       },

                       # history
                       get_History = function(){
                         list(
                           events = private$events,
                           eventT = private$eventT
                         )
                       },

                       # accessors
                       get_Private = function(){
                         return(as.list(private))
                       },

                       get_Public = function(){
                         return(as.list(self))
                       },

                      # # #  TESTING TESTING TESTING TESTING TESTING TESTING TESTING TESTING TESTING TESTING TESTING TESTING TESTING TESTING
                      # killAPerson = function(ixH, Pointers){
                      #   Pointers$pointHumanPop()$get_Human(ixH)$set_Alive(alive = FALSE)
                      #   print(paste0("human: ",self$get_myID()," has just killed human: ",ixH,", the proof is: ",Pointers$pointHumanPop()$get_Human(ixH)$get_Alive()))
                      # },

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
                         tag = private$eventQ[[1]]$tag
                         self$runEvent(tEvent, PAR, tag)
                         self$rmFirstEventFromQ()
                       },

                       # run an event
                       runEvent = function(tEvent, PAR, tag){
                         self[[tag]](tEvent, PAR)
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
                       # Life Events
                       #################################################

                       # event_maxDeath: the maximum death time event package
                       event_maxDeath = function(tEvent = 73000, PAR = NULL, tag = "death"){
                         list(tEvent = tEvent, PAR = PAR, tag = tag)
                       },

                       # death: the death event
                       death = function(tEvent, PAR){
                         self$trackHist(tEvent = tEvent, event = "D")
                         private$Alive = FALSE
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
                       eventQ = list(), #event queue
                       queueN = 1, #number of events in queue

                       # Event History
                       events = c("init"),
                       eventT = c(-1),

                       # Pathogens
                       Pathogens = list()

                     )

) #end class definition
